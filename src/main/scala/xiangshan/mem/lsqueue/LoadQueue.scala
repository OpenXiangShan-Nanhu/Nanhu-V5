/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import utils._
import xs.utils._
import xs.utils.perf._
import xiangshan._
import xiangshan.backend._
import xiangshan.backend.fu.fpu._
import xiangshan.backend.rob.RobLsqIO
import xiangshan.cache._
import xiangshan.cache.mmu._
import xiangshan.frontend.FtqPtr
import xiangshan.ExceptionNO._
import xiangshan.mem.mdp._
import xiangshan.backend.Bundles.{DynInst, MemExuOutput, MemMicroOpRbExt}
import xiangshan.backend.rob.RobPtr

class LqPtr(implicit p: Parameters) extends CircularQueuePtr[LqPtr](
  p => p(XSCoreParamsKey).VirtualLoadQueueSize
){
}

object LqPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): LqPtr = {
    val ptr = Wire(new LqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

trait HasLoadHelper { this: XSModule =>
  def rdataHelper(uop: DynInst, rdata: UInt): UInt = {
    val fpWen = uop.fpWen
    LookupTree(uop.fuOpType, List(
      LSUOpType.lb   -> SignExt(rdata(7, 0) , XLEN),
      LSUOpType.lh   -> SignExt(rdata(15, 0), XLEN),
      /*
          riscv-spec-20191213: 12.2 NaN Boxing of Narrower Values
          Any operation that writes a narrower result to an f register must write
          all 1s to the uppermost FLEN−n bits to yield a legal NaN-boxed value.
      */
      LSUOpType.lw   -> Mux(fpWen, FPU.box(rdata, FPU.S), SignExt(rdata(31, 0), XLEN)),
      LSUOpType.ld   -> Mux(fpWen, FPU.box(rdata, FPU.D), SignExt(rdata(63, 0), XLEN)),
      LSUOpType.lbu  -> ZeroExt(rdata(7, 0) , XLEN),
      LSUOpType.lhu  -> ZeroExt(rdata(15, 0), XLEN),
      LSUOpType.lwu  -> ZeroExt(rdata(31, 0), XLEN),

      // hypervisor
      LSUOpType.hlvb -> SignExt(rdata(7, 0), XLEN),
      LSUOpType.hlvh -> SignExt(rdata(15, 0), XLEN),
      LSUOpType.hlvw -> SignExt(rdata(31, 0), XLEN),
      LSUOpType.hlvd -> SignExt(rdata(63, 0), XLEN),
      LSUOpType.hlvbu -> ZeroExt(rdata(7, 0), XLEN),
      LSUOpType.hlvhu -> ZeroExt(rdata(15, 0), XLEN),
      LSUOpType.hlvwu -> ZeroExt(rdata(31, 0), XLEN),
      LSUOpType.hlvxhu -> ZeroExt(rdata(15, 0), XLEN),
      LSUOpType.hlvxwu -> ZeroExt(rdata(31, 0), XLEN),
    ))
  }

  def genRdataOH(uop: DynInst): UInt = {
    val fuOpType = uop.fuOpType
    val fpWen    = uop.fpWen
    val result = Cat(
      (fuOpType === LSUOpType.lw && fpWen),
      (fuOpType === LSUOpType.lh && fpWen),
      (fuOpType === LSUOpType.lw && !fpWen) || (fuOpType === LSUOpType.hlvw),
      (fuOpType === LSUOpType.lh && !fpWen) || (fuOpType === LSUOpType.hlvh),
      (fuOpType === LSUOpType.lb)           || (fuOpType === LSUOpType.hlvb),
      (fuOpType === LSUOpType.ld)           || (fuOpType === LSUOpType.hlvd),
      (fuOpType === LSUOpType.lwu)          || (fuOpType === LSUOpType.hlvwu) || (fuOpType === LSUOpType.hlvxwu),
      (fuOpType === LSUOpType.lhu)          || (fuOpType === LSUOpType.hlvhu) || (fuOpType === LSUOpType.hlvxhu),
      (fuOpType === LSUOpType.lbu)          || (fuOpType === LSUOpType.hlvbu),
    )
    result
  }

  def newRdataHelper(select: UInt, rdata: UInt): UInt = {
    XSError(PopCount(select) > 1.U, "data selector must be One-Hot!\n")
    val selData = Seq(
      ZeroExt(rdata(7, 0), XLEN),
      ZeroExt(rdata(15, 0), XLEN),
      ZeroExt(rdata(31, 0), XLEN),
      rdata(63, 0),
      SignExt(rdata(7, 0) , XLEN),
      SignExt(rdata(15, 0) , XLEN),
      SignExt(rdata(31, 0) , XLEN),
      FPU.box(rdata, FPU.H),
      FPU.box(rdata, FPU.S)
    )
    Mux1H(select, selData)
  }

  def genDataSelectByOffset(addrOffset: UInt): Vec[Bool] = {
    require(addrOffset.getWidth == 3)
    VecInit((0 until 8).map{ case i =>
      addrOffset === i.U
    })
  }

  def rdataVecHelper(alignedType: UInt, rdata: UInt): UInt = {
    LookupTree(alignedType, List(
      "b00".U -> ZeroExt(rdata(7, 0), VLEN),
      "b01".U -> ZeroExt(rdata(15, 0), VLEN),
      "b10".U -> ZeroExt(rdata(31, 0), VLEN),
      "b11".U -> ZeroExt(rdata(63, 0), VLEN)
    ))
  }
}

class LqEnqIO(implicit p: Parameters) extends MemBlockBundle {
  val canAccept = Output(Bool())
  val sqCanAccept = Input(Bool())
  val needAlloc = Vec(LSQEnqWidth, Input(Bool()))
  val req = Vec(LSQEnqWidth, Flipped(ValidIO(new DynInst)))
  val resp = Vec(LSQEnqWidth, Output(new LqPtr))
}

class LqTriggerIO(implicit p: Parameters) extends XSBundle {
  val hitLoadAddrTriggerHitVec = Input(Vec(TriggerNum, Bool()))
  val lqLoadAddrTriggerHitVec = Output(Vec(TriggerNum, Bool()))
}

class LoadQueueTopDownIO(implicit p: Parameters) extends XSBundle {
  val robHeadVaddr = Flipped(Valid(UInt(VAddrBits.W)))
  val robHeadTlbReplay = Output(Bool())
  val robHeadTlbMiss = Output(Bool())
  val robHeadLoadVio = Output(Bool())
  val robHeadLoadMSHR = Output(Bool())
  val robHeadMissInDTlb = Input(Bool())
  val robHeadOtherReplay = Output(Bool())
}


object PipeWithName {
  def apply[T <: Data](enqValid: Bool, enqBits: T, latency: Int, name: String): Valid[T] = {
    require(latency >= 0, "Pipe latency must be greater than or equal to zero!")
    if (latency == 0) {
      val out = Wire(Valid(chiselTypeOf(enqBits)))
      out.valid := enqValid
      out.bits := enqBits
      out
    } else {
      prefix(name) {
        val v = RegNext(enqValid, false.B)
        val b = RegEnable(enqBits, enqValid)
        apply(v, b, latency - 1, name)
      }
    }
  }

  def apply[T <: Data](enqValid: Bool, enqBits: T ,name: String): Valid[T] = {
    apply(enqValid, enqBits, 1, name)
  }

  def apply[T <: Data](enq: Valid[T], latency: Int = 1, name: String): Valid[T] = {
    apply(enq.valid, enq.bits, latency, name)
  }
}


object PipeDup {
  def apply[T <: Data](useName: String, dupNames: List[String], d: ValidIO[T]): Map[String, ValidIO[T]] = {
    dupNames.map { name =>
      name -> PipeWithName(d, name = useName + "_dup_" + name)
    }.toMap
  }
}

object RegDup{
  def apply[T <: Data](useName: String, dupNames: List[String], d: T, needInit: Boolean = false): Map[String, T] = {
    val reg = if (needInit) RegNext(d, 0.U.asTypeOf(d)) else RegNext(d)
    dupNames.zipWithIndex.map { case (name, _) =>
      name -> reg.suggestName(useName + "_dup_" + name)
    }.toMap
  }
}

class LoadQueue(implicit p: Parameters) extends XSModule
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasLoadHelper
  with HasPerfEvents
{
  val io = IO(new Bundle() {
    val redirect = Flipped(Valid(new Redirect))
    val vecFeedback = Vec(VecLoadPipelineWidth, Flipped(ValidIO(new FeedbackToLsqIO)))
    val enq = new LqEnqIO
    val ldu = new Bundle() {
        val stld_nuke_query = Vec(LoadPipelineWidth, Flipped(new LoadNukeQueryIO)) // from load_s2
        val ldld_nuke_query = Vec(LoadPipelineWidth, Flipped(new LoadNukeQueryIO)) // from load_s2
        val mmio_paddr = Vec(LoadPipelineWidth, Input(Valid(new LoadMMIOPaddrIO))) // from load_s2
        val ldin         = Vec(LoadPipelineWidth, Flipped(Decoupled(new LqWriteBundle))) // from load_s3
    }
    val sta = new Bundle() {
      val storeAddrIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle))) // from store_s1
    }
    val std = new Bundle() {
      val storeDataIn = Vec(StorePipelineWidth, Flipped(Valid(new MemExuOutput(isVector = true)))) // from store_s0, store data, send to sq from rs
    }
    val sq = new Bundle() {
      val stAddrReadySqPtr = Input(new SqPtr)
      val stAddrReadyVec   = Input(Vec(StoreQueueSize, Bool()))
      val stDataReadySqPtr = Input(new SqPtr)
      val stDataReadyVec   = Input(Vec(StoreQueueSize, Bool()))
      val stIssuePtr       = Input(new SqPtr)
      val sqEmpty          = Input(Bool())
    }
    val ldout = DecoupledIO(new MemExuOutput)
    val ld_raw_data = Output(new LoadDataFromLQBundle)
    val replay = Vec(LoadPipelineWidth, Decoupled(new LsPipelineBundle))
    val tl_d_channel  = Input(new DcacheToLduForwardIO)
    val release = Flipped(Valid(new Release))
    val nuke_rollback = Vec(StorePipelineWidth, Output(Valid(new Redirect)))
    val rob = Flipped(new RobLsqIO)
    val uncache = new UncacheWordIO
    val exceptionAddr = new ExceptionAddrIO
    val flushFrmMaBuf = Input(Bool())
    val lqFull = Output(Bool())
    val lqDeq = Output(UInt(log2Up(CommitWidth + 1).W))
    val lqCancelCnt = Output(UInt(log2Up(VirtualLoadQueueSize+1).W))
    val lq_rep_full = Output(Bool())
    val l2_hint = Input(Valid(new L2ToL1Hint()))
    val tlb_hint = Flipped(new TlbHintIO)
    val lqEmpty = Output(Bool())

    val lqDeqPtr = Output(new LqPtr)
    val replayQValidCount = Output(UInt(log2Up(LoadQueueReplaySize + 1).W))

    val debugTopDown = new LoadQueueTopDownIO
  })

  // val loadQueueRAR = Module(new LoadQueueRAR)  //  read-after-read violation
  val loadQueueRAW = Module(new LoadQueueRAW)  //  read-after-write violation
  val loadQueueReplay = Module(new LoadQueueReplay)  //  enqueue if need replay
  val virtualLoadQueue = Module(new VirtualLoadQueue)  //  control state
  val exceptionBuffer = Module(new LqExceptionBuffer) // exception buffer

  val redirectDupName = List("loadQueueRAW", "loadQueueReplay_0", "loadQueueReplay_1", "exceptionBuffer",
                            "virtualLoadQueue_0", "virtualLoadQueue_1", "virtualLoadQueue_2")
  val redirectReg = PipeDup("redirect", redirectDupName, io.redirect)

  /**
   * LoadQueueRAW
   */
  loadQueueRAW.io.redirect := redirectReg("loadQueueRAW")
  loadQueueRAW.io.vecFeedback      <> io.vecFeedback
  loadQueueRAW.io.storeIn          <> io.sta.storeAddrIn
  loadQueueRAW.io.stAddrReadySqPtr <> io.sq.stAddrReadySqPtr
  loadQueueRAW.io.stIssuePtr       <> io.sq.stIssuePtr
  for (w <- 0 until LoadPipelineWidth) {
    loadQueueRAW.io.query(w).req    <> io.ldu.stld_nuke_query(w).req // from load_s2
    loadQueueRAW.io.query(w).resp   <> io.ldu.stld_nuke_query(w).resp // to load_s3
    loadQueueRAW.io.query(w).revoke := io.ldu.stld_nuke_query(w).revoke // from load_s3
  }

  /**
   * VirtualLoadQueue
   */
  virtualLoadQueue.io.redirect_dup.zipWithIndex.foreach(r => r._1 := redirectReg(s"virtualLoadQueue_${r._2}"))
  virtualLoadQueue.io.vecCommit     <> io.vecFeedback
  virtualLoadQueue.io.enq           <> io.enq
  virtualLoadQueue.io.ldin          <> io.ldu.ldin // from load_s3
  virtualLoadQueue.io.mmio_paddr    := io.ldu.mmio_paddr // from load_s2
  virtualLoadQueue.io.lqFull        <> io.lqFull
  virtualLoadQueue.io.lqDeq         <> io.lqDeq
  virtualLoadQueue.io.lqCancelCnt   <> io.lqCancelCnt
  virtualLoadQueue.io.lqEmpty       <> io.lqEmpty
  virtualLoadQueue.io.ldWbPtr       <> io.lqDeqPtr
  virtualLoadQueue.io.mmioWbPtr.valid := loadQueueReplay.io.ldout.fire
  virtualLoadQueue.io.mmioWbPtr.bits := loadQueueReplay.io.ldout.bits.uop.lqIdx
  // add for RAR violation check
  virtualLoadQueue.io.release       <> io.release
  for (w <- 0 until LoadPipelineWidth) {
    virtualLoadQueue.io.query(w).req    <> io.ldu.ldld_nuke_query(w).req // from load_s2
    virtualLoadQueue.io.query(w).resp   <> io.ldu.ldld_nuke_query(w).resp // to load_s3
    virtualLoadQueue.io.query(w).revoke := io.ldu.ldld_nuke_query(w).revoke // from load_s3
  }
  virtualLoadQueue.io.debugInfo.pendingRobPtr := io.rob.pendingPtr
  virtualLoadQueue.io.debugInfo.replayAllocateVec := loadQueueReplay.io.replayEntryInfo.allocateVec
  virtualLoadQueue.io.debugInfo.replayUopVec := loadQueueReplay.io.replayEntryInfo.uopVec
  virtualLoadQueue.io.debugInfo.mmioEntry := loadQueueReplay.io.replayEntryInfo.mmioEntry

  /**
   * Load queue exception buffer
   */
  exceptionBuffer.io.redirect := redirectReg("exceptionBuffer")
  for (i <- 0 until LoadPipelineWidth) {
    exceptionBuffer.io.req(i).valid := io.ldu.ldin(i).valid && !io.ldu.ldin(i).bits.isvec // from load_s3
    exceptionBuffer.io.req(i).bits := io.ldu.ldin(i).bits
  }
  // vlsu exception!
  for (i <- 0 until VecLoadPipelineWidth) {
    exceptionBuffer.io.req(LoadPipelineWidth + i).valid                 := io.vecFeedback(i).valid && io.vecFeedback(i).bits.feedback(VecFeedbacks.FLUSH) // have exception
    exceptionBuffer.io.req(LoadPipelineWidth + i).bits                  := DontCare
    exceptionBuffer.io.req(LoadPipelineWidth + i).bits.vaddr            := io.vecFeedback(i).bits.vaddr
    exceptionBuffer.io.req(LoadPipelineWidth + i).bits.fullva           := io.vecFeedback(i).bits.vaddr
    exceptionBuffer.io.req(LoadPipelineWidth + i).bits.vaNeedExt        := io.vecFeedback(i).bits.vaNeedExt
    exceptionBuffer.io.req(LoadPipelineWidth + i).bits.gpaddr           := io.vecFeedback(i).bits.gpaddr
    exceptionBuffer.io.req(LoadPipelineWidth + i).bits.uop.uopIdx       := io.vecFeedback(i).bits.uopidx
    exceptionBuffer.io.req(LoadPipelineWidth + i).bits.uop.robIdx       := io.vecFeedback(i).bits.robidx
    exceptionBuffer.io.req(LoadPipelineWidth + i).bits.uop.vpu.vstart   := io.vecFeedback(i).bits.vstart
    exceptionBuffer.io.req(LoadPipelineWidth + i).bits.uop.vpu.vl       := io.vecFeedback(i).bits.vl
    exceptionBuffer.io.req(LoadPipelineWidth + i).bits.uop.exceptionVec := io.vecFeedback(i).bits.exceptionVec
  }
  // mmio non-data error exception
  exceptionBuffer.io.req.last := loadQueueReplay.io.exception
  exceptionBuffer.io.req.last.bits.vaNeedExt := true.B
  exceptionBuffer.io.flushFrmMaBuf := io.flushFrmMaBuf

  io.exceptionAddr <> exceptionBuffer.io.exceptionAddr

  /**
   * Load uncache buffer
   */
  loadQueueReplay.io.ldout      <> io.ldout
  loadQueueReplay.io.ld_raw_data  <> io.ld_raw_data
  loadQueueReplay.io.rob        <> io.rob
  loadQueueReplay.io.uncache    <> io.uncache
  io.nuke_rollback := loadQueueRAW.io.rollback

  /* <------- DANGEROUS: Don't change sequence here ! -------> */

  /**
   * LoadQueueReplay
   */
  loadQueueReplay.io.redirect_dup.zipWithIndex.foreach(r => r._1 := redirectReg(s"loadQueueReplay_${r._2}"))
  loadQueueReplay.io.enq              <> io.ldu.ldin // from load_s3
  loadQueueReplay.io.storeAddrIn      <> io.sta.storeAddrIn // from store_s1
  loadQueueReplay.io.storeDataIn      <> io.std.storeDataIn // from store_s0
  loadQueueReplay.io.replay           <> io.replay
  loadQueueReplay.io.tl_d_channel     <> io.tl_d_channel
  loadQueueReplay.io.stAddrReadySqPtr <> io.sq.stAddrReadySqPtr
  loadQueueReplay.io.stAddrReadyVec   <> io.sq.stAddrReadyVec
  loadQueueReplay.io.stDataReadySqPtr <> io.sq.stDataReadySqPtr
  loadQueueReplay.io.stDataReadyVec   <> io.sq.stDataReadyVec
  loadQueueReplay.io.sqEmpty          <> io.sq.sqEmpty
  loadQueueReplay.io.lqFull           <> io.lq_rep_full
  loadQueueReplay.io.ldWbPtr          <> virtualLoadQueue.io.ldWbPtr
  loadQueueReplay.io.mmioLqIdx        <> virtualLoadQueue.io.mmioLqIdx
  loadQueueReplay.io.rawFull          <> loadQueueRAW.io.lqFull
  loadQueueReplay.io.l2_hint          <> io.l2_hint
  loadQueueReplay.io.tlb_hint         <> io.tlb_hint
  // TODO: implement it!
  loadQueueReplay.io.vecFeedback := io.vecFeedback

  io.replayQValidCount := loadQueueReplay.io.validCount
  loadQueueReplay.io.debugTopDown <> io.debugTopDown

  // virtualLoadQueue.io.lqFull replaces loadQueueRAR.io.lqFull
  val full_mask = Cat(virtualLoadQueue.io.lqFull, loadQueueRAW.io.lqFull, loadQueueReplay.io.lqFull)
  XSPerfAccumulate("full_mask_000", full_mask === 0.U)
  XSPerfAccumulate("full_mask_001", full_mask === 1.U)
  XSPerfAccumulate("full_mask_010", full_mask === 2.U)
  XSPerfAccumulate("full_mask_011", full_mask === 3.U)
  XSPerfAccumulate("full_mask_100", full_mask === 4.U)
  XSPerfAccumulate("full_mask_101", full_mask === 5.U)
  XSPerfAccumulate("full_mask_110", full_mask === 6.U)
  XSPerfAccumulate("full_mask_111", full_mask === 7.U)
  XSPerfAccumulate("nuke_rollback", io.nuke_rollback.map(_.valid).reduce(_ || _).asUInt)
  // XSPerfAccumulate("nack_rollabck", io.nack_rollback.valid)

  // perf cnt
  // val perfEvents = Seq(virtualLoadQueue, loadQueueRAR, loadQueueRAW, loadQueueReplay).flatMap(_.getPerfEvents) ++
  val perfEvents = Seq(virtualLoadQueue, loadQueueRAW, loadQueueReplay).flatMap(_.getPerfEvents) ++
  Seq(
    ("full_mask_000", full_mask === 0.U),
    ("full_mask_001", full_mask === 1.U),
    ("full_mask_010", full_mask === 2.U),
    ("full_mask_011", full_mask === 3.U),
    // ("full_mask_100", full_mask === 4.U),
    // ("full_mask_101", full_mask === 5.U),
    // ("full_mask_110", full_mask === 6.U),
    // ("full_mask_111", full_mask === 7.U),
    ("nuke_rollback", io.nuke_rollback.map(_.valid).reduce(_ || _).asUInt),
    // ("nack_rollback", io.nack_rollback.valid)
  )
  generatePerfEvent()
  // end
}