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
import chisel3.util._
import xs.utils._
import xs.utils.perf._
import xs.utils.cache.common._
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, MemExuOutput}
import xiangshan.cache._
import xiangshan.cache.mmu.{TlbHintIO, TlbRequestIO}
import xiangshan.backend._
import xiangshan.backend.rob.RobLsqIO
import xiangshan.backend.fu.FuType
import xiangshan.mem.mdp.MDPResUpdateIO
import xs.utils.cache.common.{CMOResp, CMOReq}

class ExceptionAddrIO(implicit p: Parameters) extends XSBundle {
  val isStore = Input(Bool())
  val vaddr = Output(UInt(XLEN.W))
  val vaNeedExt = Output(Bool())
  val isHyper = Output(Bool())
  val vstart = Output(UInt((log2Up(VLEN) + 1).W))
  val vl = Output(UInt((log2Up(VLEN) + 1).W))
  val gpaddr = Output(UInt(XLEN.W))
  val isForVSnonLeafPTE = Output(Bool())
}

class FwdEntry extends Bundle {
  val validFast = Bool() // validFast is generated the same cycle with query
  val valid = Bool() // valid is generated 1 cycle after query request
  val data = UInt(8.W) // data is generated 1 cycle after query request
}

// inflight miss block reqs
class InflightBlockInfo(implicit p: Parameters) extends XSBundle {
  val block_addr = UInt(PAddrBits.W)
  val valid = Bool()
}

class LsqEnqIO(implicit p: Parameters) extends MemBlockBundle {
  val canAccept = Output(Bool())
  val needAlloc = Vec(LSQEnqWidth, Input(UInt(2.W)))
  val req       = Vec(LSQEnqWidth, Flipped(ValidIO(new DynInst)))
  val iqAccept  = Input(Vec(LSQEnqWidth, Bool()))
  val resp      = Vec(LSQEnqWidth, Output(new LSIdx))
}

// Load / Store Queue Wrapper for XiangShan Out of Order LSU
class LsqWrapper(implicit p: Parameters) extends XSModule with HasDCacheParameters with HasPerfEvents {
  val io = IO(new Bundle() {
    val hartId = Input(UInt(hartIdLen.W))
    val redirect = Flipped(ValidIO(new Redirect))
    val stvecFeedback = Vec(VecStorePipelineWidth, Flipped(ValidIO(new FeedbackToLsqIO)))
    val ldvecFeedback = Vec(VecLoadPipelineWidth, Flipped(ValidIO(new FeedbackToLsqIO)))
    val enq = new LsqEnqIO
    val ldu = new Bundle() {
        val stld_nuke_query = Vec(LoadPipelineWidth, Flipped(new LoadNukeQueryIO)) // from load_s2
        val ldld_nuke_query = Vec(LoadPipelineWidth, Flipped(new LoadNukeQueryIO)) // from load_s2
        val mmio_paddr = Vec(LoadPipelineWidth, Input(Valid(new LoadMMIOPaddrIO))) // from load_s2
        val ldin = Vec(LoadPipelineWidth, Flipped(Decoupled(new LqWriteBundle))) // from load_s3
    }
    val sta = new Bundle() {
      val storeMaskIn = Vec(StorePipelineWidth, Flipped(Valid(new StoreMaskBundle))) // from store_s0, store mask, send to sq from rs
      val storeAddrIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle))) // from store_s1
      val storeAddrInRe = Vec(StorePipelineWidth, Input(new LsPipelineBundle())) // from store_s2
    }
    val std = new Bundle() {
      val storeDataIn = Vec(StorePipelineWidth, Flipped(Valid(new MemExuOutput(isVector = true)))) // from store_s0, store data, send to sq from rs
    }
    val ldout = DecoupledIO(new MemExuOutput)
    val ld_raw_data = Output(new LoadDataFromLQBundle)
    val replay = Vec(LoadPipelineWidth, Decoupled(new LsPipelineBundle))
    val sbuffer = Vec(EnsbufferWidth, Decoupled(new DCacheWordReqWithVaddrAndPfFlag))
    val sbufferVecDifftestInfo = Vec(EnsbufferWidth, Decoupled(new DynInst)) // The vector store difftest needs is
    val forward = Vec(LoadPipelineWidth, Flipped(new PipeLoadForwardQueryIO))
    val rob = Flipped(new RobLsqIO)
    val nuke_rollback = Vec(StorePipelineWidth, Output(Valid(new Redirect)))
    val release = Flipped(Valid(new Release))
    val tl_d_channel  = Input(new DcacheToLduForwardIO)
    val maControl     = Flipped(new StoreMaBufToSqControlIO)
    val uncache = new UncacheWordIO
    val mmioStout = DecoupledIO(new MemExuOutput) // writeback uncached store
    // val cboZeroStout = DecoupledIO(new MemExuOutput)
    // TODO: implement vector store
    val vecmmioStout = DecoupledIO(new MemExuOutput(isVector = true)) // vec writeback uncached store
    val sqEmpty = Output(Bool())
    val lq_rep_full = Output(Bool())
    val sqFull = Output(Bool())
    val lqFull = Output(Bool())
    val sqCancelCnt = Output(UInt(log2Up(StoreQueueSize+1).W))
    val lqCancelCnt = Output(UInt(log2Up(VirtualLoadQueueSize+1).W))
    val lqDeq = Output(UInt(log2Up(CommitWidth + 1).W))
    val sqDeq = Output(UInt(log2Ceil(EnsbufferWidth + 1).W))
    val lqCanAccept = Output(Bool())
    val sqCanAccept = Output(Bool())
    val lqDeqPtr = Output(new LqPtr)
    val sqDeqPtr = Output(new SqPtr)
    val exceptionAddr = new ExceptionAddrIO
    val flushFrmMaBuf = Input(Bool())
    val issuePtrExt = Output(new SqPtr)
    val l2_hint = Input(Valid(new L2ToL1Hint()))
    val tlb_hint = Flipped(new TlbHintIO)
    val cmoOpReq  = DecoupledIO(new MissReq)
    // val cmoOpResp = Flipped(DecoupledIO(new CMOResp))
    val flushSbuffer = new SbufferFlushBundle
    val force_write = Output(Bool())
    val lqEmpty = Output(Bool())
    val replayQValidCount = Output(UInt(log2Up(LoadQueueReplaySize + 1).W))
    val mdpTrainUpdate = Vec(LoadPipelineWidth, Output(Valid(new MDPResUpdateIO)))
    val sqHasCmo = Output(Bool())

    // top-down
    val debugTopDown = new LoadQueueTopDownIO
  })

  val loadQueue = Module(new LoadQueue)
  val storeQueue = Module(new StoreQueue)

  storeQueue.io.hartId := io.hartId
  io.mdpTrainUpdate := storeQueue.io.mdpTrainUpdate
  io.sqHasCmo := storeQueue.io.sqHasCmo


  // io.enq logic
  // LSQ: send out canAccept when both load queue and store queue are ready
  // Dispatch: send instructions to LSQ only when they are ready
  io.enq.canAccept := loadQueue.io.enq.canAccept && storeQueue.io.enq.canAccept
  io.lqCanAccept := loadQueue.io.enq.canAccept
  io.sqCanAccept := storeQueue.io.enq.canAccept
  io.replayQValidCount := loadQueue.io.replayQValidCount
  loadQueue.io.enq.sqCanAccept := storeQueue.io.enq.canAccept
  storeQueue.io.enq.lqCanAccept := loadQueue.io.enq.canAccept
  io.lqDeqPtr := loadQueue.io.lqDeqPtr
  io.sqDeqPtr := storeQueue.io.sqDeqPtr
  for (i <- io.enq.req.indices) {
    loadQueue.io.enq.needAlloc(i)      := io.enq.needAlloc(i)(0)
    loadQueue.io.enq.req(i).valid      := io.enq.needAlloc(i)(0) && io.enq.req(i).valid
    loadQueue.io.enq.req(i).bits       := io.enq.req(i).bits
    loadQueue.io.enq.req(i).bits.sqIdx := storeQueue.io.enq.resp(i)

    storeQueue.io.enq.needAlloc(i)      := io.enq.needAlloc(i)(1)
    storeQueue.io.enq.req(i).valid      := io.enq.needAlloc(i)(1) && io.enq.req(i).valid
    storeQueue.io.enq.req(i).bits       := io.enq.req(i).bits
    storeQueue.io.enq.req(i).bits.lqIdx := loadQueue.io.enq.resp(i)

    io.enq.resp(i).lqIdx := loadQueue.io.enq.resp(i)
    io.enq.resp(i).sqIdx := storeQueue.io.enq.resp(i)
  }

  // store queue wiring
  storeQueue.io.brqRedirect <> RegNextWithEnable(io.redirect)
  storeQueue.io.vecFeedback   <> io.stvecFeedback
  storeQueue.io.storeAddrIn <> io.sta.storeAddrIn // from store_s1
  storeQueue.io.storeAddrInRe <> io.sta.storeAddrInRe // from store_s2
  storeQueue.io.storeDataIn <> io.std.storeDataIn // from store_s0
  storeQueue.io.storeMaskIn <> io.sta.storeMaskIn // from store_s0
  storeQueue.io.sbuffer     <> io.sbuffer
  storeQueue.io.sbufferVecDifftestInfo <> io.sbufferVecDifftestInfo
  storeQueue.io.mmioStout   <> io.mmioStout
  // storeQueue.io.cboZeroStout <> io.cboZeroStout
  storeQueue.io.vecmmioStout <> io.vecmmioStout
  storeQueue.io.rob         <> io.rob
  storeQueue.io.exceptionAddr.isStore := DontCare
  storeQueue.io.sqCancelCnt  <> io.sqCancelCnt
  storeQueue.io.sqDeq        <> io.sqDeq
  storeQueue.io.sqEmpty      <> io.sqEmpty
  storeQueue.io.sqFull       <> io.sqFull
  storeQueue.io.forward      <> io.forward // overlap forwardMask & forwardData, DO NOT CHANGE SEQUENCE
  storeQueue.io.force_write  <> io.force_write
  storeQueue.io.cmoOpReq     <> io.cmoOpReq
  // storeQueue.io.cmoOpResp    <> io.cmoOpResp
  storeQueue.io.flushSbuffer <> io.flushSbuffer
  storeQueue.io.maControl    <> io.maControl

  /* <------- DANGEROUS: Don't change sequence here ! -------> */

  //  load queue wiring
  loadQueue.io.redirect            <> io.redirect
  loadQueue.io.vecFeedback           <> io.ldvecFeedback
  loadQueue.io.ldu                 <> io.ldu
  loadQueue.io.ldout               <> io.ldout
  loadQueue.io.ld_raw_data         <> io.ld_raw_data
  loadQueue.io.rob                 <> io.rob
  loadQueue.io.nuke_rollback       <> io.nuke_rollback
  loadQueue.io.replay              <> io.replay
  loadQueue.io.tl_d_channel        <> io.tl_d_channel
  loadQueue.io.release             <> io.release
  loadQueue.io.exceptionAddr.isStore := DontCare
  loadQueue.io.flushFrmMaBuf       := io.flushFrmMaBuf
  loadQueue.io.lqCancelCnt         <> io.lqCancelCnt
  loadQueue.io.sq.stAddrReadySqPtr <> storeQueue.io.stAddrReadySqPtr
  loadQueue.io.sq.stAddrReadyVec   <> storeQueue.io.stAddrReadyVec
  loadQueue.io.sq.stDataReadySqPtr <> storeQueue.io.stDataReadySqPtr
  loadQueue.io.sq.stDataReadyVec   <> storeQueue.io.stDataReadyVec
  loadQueue.io.sq.stIssuePtr       <> storeQueue.io.stIssuePtr
  loadQueue.io.sq.sqEmpty          <> storeQueue.io.sqEmpty
  loadQueue.io.sta.storeAddrIn     <> io.sta.storeAddrIn // store_s1
  loadQueue.io.std.storeDataIn     <> io.std.storeDataIn // store_s0
  loadQueue.io.lqFull              <> io.lqFull
  loadQueue.io.lq_rep_full         <> io.lq_rep_full
  loadQueue.io.lqDeq               <> io.lqDeq
  loadQueue.io.l2_hint             <> io.l2_hint
  loadQueue.io.tlb_hint            <> io.tlb_hint
  loadQueue.io.lqEmpty             <> io.lqEmpty

  // rob commits for lsq is delayed for two cycles, which causes the delayed update for deqPtr in lq/sq
  // s0: commit
  // s1:               exception find
  // s2:               exception triggered
  // s3: ptr updated & new address
  // address will be used at the next cycle after exception is triggered
  io.exceptionAddr.vaddr := Mux(RegNext(io.exceptionAddr.isStore), storeQueue.io.exceptionAddr.vaddr, loadQueue.io.exceptionAddr.vaddr)
  io.exceptionAddr.vaNeedExt := Mux(RegNext(io.exceptionAddr.isStore), storeQueue.io.exceptionAddr.vaNeedExt, loadQueue.io.exceptionAddr.vaNeedExt)
  io.exceptionAddr.isHyper := Mux(RegNext(io.exceptionAddr.isStore), storeQueue.io.exceptionAddr.isHyper, loadQueue.io.exceptionAddr.isHyper)
  io.exceptionAddr.vstart := Mux(RegNext(io.exceptionAddr.isStore), storeQueue.io.exceptionAddr.vstart, loadQueue.io.exceptionAddr.vstart)
  io.exceptionAddr.vl     := Mux(RegNext(io.exceptionAddr.isStore), storeQueue.io.exceptionAddr.vl, loadQueue.io.exceptionAddr.vl)
  io.exceptionAddr.gpaddr := Mux(RegNext(io.exceptionAddr.isStore), storeQueue.io.exceptionAddr.gpaddr, loadQueue.io.exceptionAddr.gpaddr)
  io.exceptionAddr.isForVSnonLeafPTE:= Mux(RegNext(io.exceptionAddr.isStore), storeQueue.io.exceptionAddr.isForVSnonLeafPTE, loadQueue.io.exceptionAddr.isForVSnonLeafPTE)
  io.issuePtrExt := storeQueue.io.stAddrReadySqPtr

  loadQueue.io.uncache.req.ready := io.uncache.req.ready
  storeQueue.io.uncache.req.ready := io.uncache.req.ready
  io.uncache.req.valid := loadQueue.io.uncache.req.valid || storeQueue.io.uncache.req.valid
  io.uncache.req.bits := Mux(loadQueue.io.uncache.req.valid, loadQueue.io.uncache.req.bits, storeQueue.io.uncache.req.bits)

  loadQueue.io.uncache.resp.valid := io.uncache.resp.valid && !io.uncache.resp.bits.isStore
  storeQueue.io.uncache.resp.valid := io.uncache.resp.valid && io.uncache.resp.bits.isStore
  loadQueue.io.uncache.resp.bits := io.uncache.resp.bits
  storeQueue.io.uncache.resp.bits := io.uncache.resp.bits
  io.uncache.resp.ready := true.B

  loadQueue.io.debugTopDown <> io.debugTopDown

  assert(!(loadQueue.io.uncache.req.fire && storeQueue.io.uncache.req.fire))
  assert(!(loadQueue.io.uncache.resp.fire && storeQueue.io.uncache.resp.fire))


  val perfEvents = Seq(loadQueue, storeQueue).flatMap(_.getPerfEvents)
  generatePerfEvent()
}

class LsqEnqCtrl(implicit p: Parameters) extends XSModule
  with HasVLSUParameters  {
  val io = IO(new Bundle {
    val redirect = Flipped(ValidIO(new Redirect))
    // to dispatch
    val enq = new LsqEnqIO
    // from `memBlock.io.lqDeq
    val lcommit = Input(UInt(log2Up(CommitWidth + 1).W))
    // from `memBlock.io.sqDeq`
    val scommit = Input(UInt(log2Ceil(EnsbufferWidth + 1).W))
    // from/tp lsq
    val lqCancelCnt = Input(UInt(log2Up(VirtualLoadQueueSize + 1).W))
    val sqCancelCnt = Input(UInt(log2Up(StoreQueueSize + 1).W))
    val lqFreeCount = Output(UInt(log2Up(VirtualLoadQueueSize + 1).W))
    val sqFreeCount = Output(UInt(log2Up(StoreQueueSize + 1).W))
    val enqLsq = Flipped(new LsqEnqIO)
  })

  val lqPtr = RegInit(0.U.asTypeOf(new LqPtr))
  val sqPtr = RegInit(0.U.asTypeOf(new SqPtr))
  val lqCounter = RegInit(VirtualLoadQueueSize.U(log2Up(VirtualLoadQueueSize + 1).W))
  val sqCounter = RegInit(StoreQueueSize.U(log2Up(StoreQueueSize + 1).W))
  val canAccept = RegInit(false.B)

  val blockVec = io.enq.iqAccept.map(!_) :+ true.B
  val numLsElem = io.enq.req.map(_.bits.numLsElem)
  val needEnqLoadQueue = VecInit(io.enq.req.map(x => FuType.isLoad(x.bits.fuType) || FuType.isVNonsegLoad(x.bits.fuType)))
  val needEnqStoreQueue = VecInit(io.enq.req.map(x => FuType.isStore(x.bits.fuType) || FuType.isVNonsegStore(x.bits.fuType)))
  val loadQueueElem = needEnqLoadQueue.zip(numLsElem).map(x => Mux(x._1, x._2, 0.U))
  val storeQueueElem = needEnqStoreQueue.zip(numLsElem).map(x => Mux(x._1, x._2, 0.U))
  val loadFlowPopCount = 0.U +: loadQueueElem.zipWithIndex.map{ case (l, i) =>
    loadQueueElem.take(i + 1).reduce(_ + _)
  }
  val storeFlowPopCount = 0.U +: storeQueueElem.zipWithIndex.map { case (s, i) =>
    storeQueueElem.take(i + 1).reduce(_ + _)
  }
  val lqAllocNumber = PriorityMux(blockVec.zip(loadFlowPopCount))
  val sqAllocNumber = PriorityMux(blockVec.zip(storeFlowPopCount))

  io.lqFreeCount  := lqCounter
  io.sqFreeCount  := sqCounter
  // How to update ptr and counter:
  // (1) by default, updated according to enq/commit
  // (2) when redirect and dispatch queue is empty, update according to lsq
  val t1_redirect = RegNext(io.redirect.valid)
  val t2_redirect = RegNext(t1_redirect)
  val t2_update = t2_redirect && !VecInit(io.enq.needAlloc.map(_.orR)).asUInt.orR
  val t3_update = RegNext(t2_update)
  val t3_lqCancelCnt = GatedRegNext(io.lqCancelCnt)
  val t3_sqCancelCnt = GatedRegNext(io.sqCancelCnt)
  when (t3_update) {
    lqPtr := lqPtr - t3_lqCancelCnt
    lqCounter := lqCounter + io.lcommit + t3_lqCancelCnt
    sqPtr := sqPtr - t3_sqCancelCnt
    sqCounter := sqCounter + io.scommit + t3_sqCancelCnt
  }.elsewhen (!io.redirect.valid && io.enq.canAccept) {
    lqPtr := lqPtr + lqAllocNumber
    lqCounter := lqCounter + io.lcommit - lqAllocNumber
    sqPtr := sqPtr + sqAllocNumber
    sqCounter := sqCounter + io.scommit - sqAllocNumber
  }.otherwise {
    lqCounter := lqCounter + io.lcommit
    sqCounter := sqCounter + io.scommit
  }


  //TODO MaxAllocate and width of lqOffset/sqOffset needs to be discussed
  val lqMaxAllocate = LSQLdEnqWidth
  val sqMaxAllocate = LSQStEnqWidth
  val maxAllocate = lqMaxAllocate max sqMaxAllocate
  val ldCanAccept = lqCounter >= lqAllocNumber +& lqMaxAllocate.U
  val sqCanAccept = sqCounter >= sqAllocNumber +& sqMaxAllocate.U
  // It is possible that t3_update and enq are true at the same clock cycle.
  // For example, if redirect.valid lasts more than one clock cycle,
  // after the last redirect, new instructions may enter but previously redirect has not been resolved (updated according to the cancel count from LSQ).
  // To solve the issue easily, we block enqueue when t3_update, which is RegNext(t2_update).
  io.enq.canAccept := RegNext(ldCanAccept && sqCanAccept && !t2_update)
  val lqOffset = Wire(Vec(io.enq.resp.length, UInt(lqPtr.value.getWidth.W)))
  val sqOffset = Wire(Vec(io.enq.resp.length, UInt(sqPtr.value.getWidth.W)))
  for ((resp, i) <- io.enq.resp.zipWithIndex) {
    lqOffset(i) := loadFlowPopCount(i)
    resp.lqIdx := lqPtr + lqOffset(i)
    sqOffset(i) := storeFlowPopCount(i)
    resp.sqIdx := sqPtr + sqOffset(i)
  }

  io.enqLsq.needAlloc := RegNext(io.enq.needAlloc)
  io.enqLsq.iqAccept := RegNext(io.enq.iqAccept)
  io.enqLsq.req.zip(io.enq.req).zip(io.enq.resp).foreach{ case ((toLsq, enq), resp) =>
    val do_enq = enq.valid && !io.redirect.valid && io.enq.canAccept
    toLsq.valid := RegNext(do_enq)
    toLsq.bits := RegEnable(enq.bits, do_enq)
    toLsq.bits.lqIdx := RegEnable(resp.lqIdx, do_enq)
    toLsq.bits.sqIdx := RegEnable(resp.sqIdx, do_enq)
  }

}