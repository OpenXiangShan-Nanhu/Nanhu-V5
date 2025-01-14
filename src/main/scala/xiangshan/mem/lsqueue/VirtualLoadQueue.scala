/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xiangshan._
import xiangshan.backend.rob.{RobLsqIO, RobPtr}
import xiangshan.ExceptionNO._
import xiangshan.cache._
import utils._
import xs.utils._
import xs.utils.perf._
import xiangshan.backend.Bundles.{DynInst, MemExuOutput}
import xiangshan.backend.fu.FuConfig.LduCfg
import xiangshan.backend.decode.isa.bitfield.{InstVType, XSInstBitFields}

class VirtualLoadQueue(implicit p: Parameters) extends XSModule
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasLoadHelper
  with HasPerfEvents
  with HasVLSUParameters {
  val io = IO(new Bundle() {
    // control
    val redirect    = Flipped(Valid(new Redirect))
    val vecCommit   = Vec(VecLoadPipelineWidth, Flipped(ValidIO(new FeedbackToLsqIO)))
    // from dispatch
    val enq         = new LqEnqIO
    // from ldu s3
    val ldin        = Vec(LoadPipelineWidth, Flipped(DecoupledIO(new LqWriteBundle)))
    // to LoadQueueReplay and LoadQueueRAR
    val ldWbPtr     = Output(new LqPtr)
    // global
    val lqFull      = Output(Bool())
    val lqEmpty     = Output(Bool())
    // to dispatch
    val lqDeq       = Output(UInt(log2Up(CommitWidth + 1).W))
    val lqCancelCnt = Output(UInt(log2Up(VirtualLoadQueueSize+1).W))

    // add for RAR violation check
    // release cacheline
    val release = Flipped(Valid(new Release)) //paddr
    // violation query
    val query = Vec(LoadPipelineWidth, Flipped(new LoadNukeQueryIO))

  })

  println("VirtualLoadQueue: size: " + VirtualLoadQueueSize)
  //  VirtualLoadQueue field
  //  +-----------+---------+-------+
  //  | Allocated | MicroOp | Flags |
  //  +-----------+---------+-------+
  //  Allocated   : entry has been allocated already
  //  MicroOp     : inst's microOp
  //  Flags       : load flags
  val allocated = RegInit(VecInit(List.fill(VirtualLoadQueueSize)(false.B))) // The control signals need to explicitly indicate the initial value
  val uop = Reg(Vec(VirtualLoadQueueSize, new DynInst))
  val addrvalid = RegInit(VecInit(List.fill(VirtualLoadQueueSize)(false.B))) // non-mmio addr is valid
  val datavalid = RegInit(VecInit(List.fill(VirtualLoadQueueSize)(false.B))) // non-mmio data is valid
  // vector load: inst -> uop (pdest registor) -> flow (once load operation in loadunit)
  val isvec = RegInit(VecInit(List.fill(VirtualLoadQueueSize)(false.B))) // vector load flow
  val veccommitted = RegInit(VecInit(List.fill(VirtualLoadQueueSize)(false.B))) // vector load uop has commited

  //  LoadQueueRAR field
  //  +-------+-------+-------+----------+--------------+
  //  | Valid |  Uop  | PAddr | Released | isfromDCache
  //  +-------+-------+-------+----------+--------------+
  //
  //  Field descriptions:
  //  PAddr       : physical address.
  //  Released    : DCache released.
  val paddrModule = Module(new LqPAddrModule(
    gen = UInt(PAddrBits.W),
    numEntries = VirtualLoadQueueSize,
    numRead = LoadPipelineWidth,
    numWrite = LoadPipelineWidth,
    numWBank = LoadQueueNWriteBanks,
    numWDelay = 2,
    numCamPort = LoadPipelineWidth
  ))
  paddrModule.io.ren := List.fill(LoadPipelineWidth)(false.B)
  paddrModule.io.raddr := List.fill(LoadPipelineWidth)(0.U(VirtualLoadQueueSize.W))
  paddrModule.io.violationMdataValid := List.fill(LoadPipelineWidth)(false.B)     // is used in raw module
  paddrModule.io.violationMdata := List.fill(LoadPipelineWidth)(0.U(PAddrBits.W)) // is used in raw module
  paddrModule.io.waddr := List.fill(LoadPipelineWidth)(0.U(VirtualLoadQueueSize.W))
  paddrModule.io.wdata := List.fill(LoadPipelineWidth)(0.U(PAddrBits.W))
  for (i <- 0 until LoadPipelineWidth - 1) {
    paddrModule.io.releaseMdataValid(i) := false.B
    paddrModule.io.releaseMdata(i) := WireInit(0.U(PAddrBits.W)) // only use the last port to ld ld query
  }
  val released = RegInit(VecInit(List.fill(VirtualLoadQueueSize)(false.B)))
  val bypassPAddr = Reg(Vec(LoadPipelineWidth, UInt(PAddrBits.W)))
  // pseudo-RAR: ld src flag 
  // isfromDCache = false, namely totally from store forward
  // st0 ld1 ld0 : ld1 src is totally from st0 forward, so if release occurs at ld0 is pseudo-RAR,
  // because even if the cache line was modified, st0's result is not changed, so don't need to report RAR vio;
  val isFromDCache = RegInit(VecInit(List.fill(VirtualLoadQueueSize)(true.B)))

  /**
   * RAR violation check logic needs to 2 extra regfile
   * 1. paddrmodule to store paddr
   * 2. released module to store whether the ld's cacheline is released
   *    the update of release signal has three parts because of the pipe in paddrmodule 
   * and a RAR check logic 
   * 
   */

  /**
   * used for debug
   */
  val debug_mmio = Reg(Vec(VirtualLoadQueueSize, Bool())) // mmio: inst is an mmio inst
  val debug_paddr = Reg(Vec(VirtualLoadQueueSize, UInt(PAddrBits.W))) // mmio: inst's paddr

  //  maintain pointers
  val enqPtrExt = RegInit(VecInit((0 until io.enq.req.length).map(_.U.asTypeOf(new LqPtr))))
  val enqPtr = enqPtrExt(0).value
  val deqPtr = RegInit(0.U.asTypeOf(new LqPtr))
  val deqPtrNext = Wire(new LqPtr)
  val deqPtr_dup_allocated = RegInit(0.U.asTypeOf(new LqPtr))

  /**
   * update pointer
   */
  val lastCycleRedirect = RegNext(io.redirect)
  val lastLastCycleRedirect = RegNext(lastCycleRedirect)

  val validCount = distanceBetween(enqPtrExt(0), deqPtr)
  val allowEnqueue = validCount <= (VirtualLoadQueueSize - LSQLdEnqWidth).U
  val canEnqueue = io.enq.req.map(_.valid)
  val needCancel = WireInit(VecInit((0 until VirtualLoadQueueSize).map(i => {
    uop(i).robIdx.needFlush(io.redirect) && allocated(i)
  })))
  val lastNeedCancel = GatedValidRegNext(needCancel)
  val enqCancel = canEnqueue.zip(io.enq.req).map{case (v , x) =>
    v && x.bits.robIdx.needFlush(io.redirect)
  }
  val enqCancelNum = enqCancel.zip(io.enq.req).map{case (v, req) =>
    Mux(v, req.bits.numLsElem, 0.U)
  }
  val lastEnqCancel = GatedRegNext(enqCancelNum.reduce(_ + _))
  val lastCycleCancelCount = PopCount(lastNeedCancel)
  val redirectCancelCount = RegEnable(lastCycleCancelCount + lastEnqCancel, 0.U, lastCycleRedirect.valid)

  // update enqueue pointer
  val vLoadFlow = io.enq.req.map(_.bits.numLsElem)
  val validVLoadFlow = vLoadFlow.zipWithIndex.map{case (vLoadFlowNumItem, index) => Mux(canEnqueue(index), vLoadFlowNumItem, 0.U)}
  val validVLoadOffset = vLoadFlow.zip(io.enq.needAlloc).map{case (flow, needAllocItem) => Mux(needAllocItem, flow, 0.U)}
  val validVLoadOffsetRShift = 0.U +: validVLoadOffset.take(validVLoadFlow.length - 1)

  val enqNumber = validVLoadFlow.reduce(_ + _)
  val enqPtrExtNextVec = Wire(Vec(io.enq.req.length, new LqPtr))
  val enqPtrExtNext = Wire(Vec(io.enq.req.length, new LqPtr))
  when (lastLastCycleRedirect.valid) {
    // we recover the pointers in the next cycle after redirect
    enqPtrExtNextVec := VecInit(enqPtrExt.map(_ - redirectCancelCount))
  } .otherwise {
    enqPtrExtNextVec := VecInit(enqPtrExt.map(_ + enqNumber))
  }
  assert(!(lastCycleRedirect.valid && enqNumber =/= 0.U))

  when (isAfter(enqPtrExtNextVec(0), deqPtrNext)) {
    enqPtrExtNext := enqPtrExtNextVec
  } .otherwise {
    enqPtrExtNext := VecInit((0 until io.enq.req.length).map(i => deqPtrNext + i.U))
  }
  enqPtrExt := enqPtrExtNext

  // update dequeue pointer
  val DeqPtrMoveStride = CommitWidth
  require(DeqPtrMoveStride == CommitWidth, "DeqPtrMoveStride must be equal to CommitWidth!")
  val deqLookupVec = VecInit((0 until DeqPtrMoveStride).map(deqPtr + _.U))
  val deqLookup = VecInit(deqLookupVec.map(ptr => allocated(ptr.value)
    && ((datavalid(ptr.value) && addrvalid(ptr.value) && !isvec(ptr.value)) || (isvec(ptr.value) && veccommitted(ptr.value)))
    && ptr =/= enqPtrExt(0)))
  val deqInSameRedirectCycle = VecInit(deqLookupVec.map(ptr => needCancel(ptr.value)))
  // make chisel happy
  val deqCountMask = Wire(UInt(DeqPtrMoveStride.W))
  deqCountMask := deqLookup.asUInt & (~deqInSameRedirectCycle.asUInt).asUInt  // the redirected ld don't need to deq
  val commitCount = PopCount(PriorityEncoderOH(~deqCountMask) - 1.U)
  val lastCommitCount = GatedRegNext(commitCount)

  // update deqPtr
  // cycle 1: generate deqPtrNext
  // cycle 2: update deqPtr
  val deqPtrUpdateEna = lastCommitCount =/= 0.U
  deqPtrNext := deqPtr + lastCommitCount
  when(deqPtrUpdateEna){
    deqPtr := deqPtrNext
    deqPtr_dup_allocated := deqPtrNext
  }

  io.lqDeq := GatedRegNext(lastCommitCount)
  io.lqCancelCnt := redirectCancelCount
  io.ldWbPtr := deqPtr
  io.lqEmpty := RegNext(validCount === 0.U)

  /**
   * Enqueue at dispatch
   *
   * Currently, VirtualLoadQueue only allows enqueue when #emptyEntries > EnqWidth
   */
  io.enq.canAccept := allowEnqueue
  for (i <- 0 until io.enq.req.length) {
    val lqIdx = enqPtrExt(0) + validVLoadOffsetRShift.take(i + 1).reduce(_ + _)
    val index = io.enq.req(i).bits.lqIdx
    val enqInstr = io.enq.req(i).bits.instr.asTypeOf(new XSInstBitFields)
    when (canEnqueue(i) && !enqCancel(i)) {
      // The maximum 'numLsElem' number that can be emitted per dispatch port is:
      //    16 2 2 2 2 2.
      // Therefore, VecMemLSQEnqIteratorNumberSeq = Seq(16, 2, 2, 2, 2, 2)
      for (j <- 0 until VecMemLSQEnqIteratorNumberSeq(i)) {
        when (j.U < validVLoadOffset(i)) {
          allocated((index + j.U).value) := true.B
          uop((index + j.U).value) := io.enq.req(i).bits
          uop((index + j.U).value).lqIdx := lqIdx + j.U

          // init
          addrvalid((index + j.U).value) := false.B
          datavalid((index + j.U).value) := false.B
          isvec((index + j.U).value) := enqInstr.isVecLoad
          veccommitted((index + j.U).value) := false.B

          debug_mmio((index + j.U).value) := false.B
          debug_paddr((index + j.U).value) := 0.U

          XSError(!io.enq.canAccept || !io.enq.sqCanAccept, s"must accept $i\n")
          XSError(index.value =/= lqIdx.value, s"must be the same entry $i\n")
        }
      }
    }
    io.enq.resp(i) := lqIdx
  }

  
  // RAR  paddrModule enq
  val release1Cycle = io.release
  val release2Cycle = RegNext(release1Cycle)
  val queryRAR_canEnqueue = io.query.map(_.req.valid)
  val queryRAR_cancelEnqueue = io.query.map(_.req.bits.uop.robIdx.needFlush(io.redirect))
  val queryRAR_needEnqueue = queryRAR_canEnqueue.zip(queryRAR_cancelEnqueue).map { case (v, c) => v && !c}
  // Allocate logic
  val acceptedVec = Wire(Vec(LoadPipelineWidth, Bool()))
  val enqIndexVec = Wire(Vec(LoadPipelineWidth, UInt(log2Up(VirtualLoadQueueSize).W)))
  for ((enq, w) <- io.query.map(_.req).zipWithIndex) {
    paddrModule.io.wen(w) := false.B
    enq.ready := true.B
    acceptedVec(w) := false.B
    // initalize
    enqIndexVec(w) := 0.U(log2Up(VirtualLoadQueueSize).W)
    bypassPAddr(w) := 0.U(PAddrBits.W)

    // lqidx is pointer, so need to add .value
    when(queryRAR_needEnqueue(w) && enq.ready) {
      val index = enq.bits.uop.lqIdx.value
      enqIndexVec(w) := index
      acceptedVec(w) := true.B
      paddrModule.io.wen(w) := true.B
      paddrModule.io.waddr(w) := index
      paddrModule.io.wdata(w) := enq.bits.paddr
      bypassPAddr(w) := enq.bits.paddr
      
      // release update
      released(index) := 
        enq.bits.data_valid && 
        (release2Cycle.valid &&
        enq.bits.paddr(PAddrBits-1, DCacheLineOffset) === release2Cycle.bits.paddr(PAddrBits-1, DCacheLineOffset) ||
        release1Cycle.valid &&
        enq.bits.paddr(PAddrBits-1, DCacheLineOffset) === release1Cycle.bits.paddr(PAddrBits-1, DCacheLineOffset))

      isFromDCache(index) := !enq.bits.full_fwd   // if full from fwd , namely is not from DCache, then don't send rar vio
    }
  }
  
  // When io.release.valid (release1cycle.valid), it uses the last ld-ld paddr cam port to
  // update release flag in 1 cycle
  paddrModule.io.releaseMdataValid.takeRight(1)(0) := release1Cycle.valid
  paddrModule.io.releaseMdata.takeRight(1)(0) := Mux(release1Cycle.valid,release1Cycle.bits.paddr,0.U)

  val bypassPAddr2Cycle = RegNext(bypassPAddr)
  val lastCanAccept = RegNext(acceptedVec)
  val lastAllocIndex = RegNext(enqIndexVec)
  val lastAllocIndexOH = lastAllocIndex.map(UIntToOH(_))
  val lastReleasePAddrMatch = VecInit((0 until LoadPipelineWidth).map(i => {
    (bypassPAddr2Cycle(i)(PAddrBits-1, DCacheLineOffset) === release1Cycle.bits.paddr(PAddrBits-1, DCacheLineOffset)) && release1Cycle.valid
  }))

  (0 until VirtualLoadQueueSize).map(i => {
    val bypassMatch = VecInit((0 until LoadPipelineWidth).map(j => lastCanAccept(j) && lastAllocIndexOH(j)(i) && lastReleasePAddrMatch(j))).asUInt.orR
    when (RegNext(( (paddrModule.io.releaseMmask.takeRight(1)(0)(i) && release2Cycle.valid) || bypassMatch) && allocated(i))) {
      released(i) := true.B
    }
  })


  // LoadQueueRAR Query
  // Load-to-Load violation check condition:
  // 1. Physical address match by CAM port.
  // 2. release is set.
  // 3. Younger than current load instruction.
  val allocatedUInt = RegNext(allocated.asUInt)
  val deqMask = UIntToMask(deqPtr.value, VirtualLoadQueueSize)
  for ((query, w) <- io.query.zipWithIndex) {
    paddrModule.io.releaseViolationMdataValid(w) := query.req.valid
    paddrModule.io.releaseViolationMdata(w) := query.req.bits.paddr
    query.resp.valid := RegNext(query.req.valid)

    val queryMask = UIntToMask(query.req.bits.uop.lqIdx.value, VirtualLoadQueueSize)
    val different_flag = deqPtr.flag =/= query.req.bits.uop.lqIdx.flag
    val lqIdxMask1 = Mux(different_flag, ~deqMask, deqMask ^ queryMask)
    val lqIdxMask2 = Mux(different_flag, queryMask, 0.U(VirtualLoadQueueSize.W))
    val lqIdxMask = (lqIdxMask1 | lqIdxMask2).asBools
    // val lqIdxMask = lqIdxMask1.asBools.zip(lqIdxMask2.asBools).map { case ( lq1, lq2 ) => lq1 || lq2}
    val matchMask = (0 until VirtualLoadQueueSize).map(i => {
      RegNext(
        isFromDCache(i) &
        (allocated(i) & 
      paddrModule.io.releaseViolationMmask(w)(i) &
      lqIdxMask(i) &
      released(i)))
    })
    val ldLdViolationMask = VecInit(matchMask)
    ldLdViolationMask.suggestName("ldLdViolationMask_" + w)
    query.resp.bits.rep_frm_fetch := ParallelORR(ldLdViolationMask)
  }
 

  /**
    * Load commits
    *
    * When load commited, mark it as !allocated and move deqPtr forward.
    */
  (0 until DeqPtrMoveStride).map(i => {
    when (commitCount > i.U) {
      allocated((deqPtr_dup_allocated+i.U).value) := false.B
      XSError(!allocated((deqPtr_dup_allocated+i.U).value), s"why commit invalid entry $i?\n")
    }
  })

  // vector commit or replay
  val vecLdCommittmp = Wire(Vec(VirtualLoadQueueSize, Vec(VecLoadPipelineWidth, Bool())))
  val vecLdCommit = Wire(Vec(VirtualLoadQueueSize, Bool()))
  for (i <- 0 until VirtualLoadQueueSize) {
    val cmt = io.vecCommit
    for (j <- 0 until VecLoadPipelineWidth) {
      vecLdCommittmp(i)(j) := allocated(i) && cmt(j).valid && uop(i).robIdx === cmt(j).bits.robidx && uop(i).uopIdx === cmt(j).bits.uopidx
    }
    vecLdCommit(i) := vecLdCommittmp(i).reduce(_ || _)

    when (vecLdCommit(i)) {
      veccommitted(i) := true.B
    }
  }

  // misprediction recovery / exception redirect
  // invalidate lq term using robIdx
  for (i <- 0 until VirtualLoadQueueSize) {
    when (needCancel(i)) {
      allocated(i) := false.B
    }
  }

  XSDebug(p"(ready, valid): ${io.enq.canAccept}, ${Binary(Cat(io.enq.req.map(_.valid)))}\n")

  /**
    * Writeback load from load units
    *
    * Most load instructions writeback to regfile at the same time.
    * However,
    *   (1) For ready load instruction (no need replay), it writes back to ROB immediately.
    */
  for(i <- 0 until LoadPipelineWidth) {
    //   most lq status need to be updated immediately after load writeback to lq
    //   flag bits in lq needs to be updated accurately
    io.ldin(i).ready := true.B
    val loadWbIndex = io.ldin(i).bits.uop.lqIdx.value

    when (io.ldin(i).valid) {
      val hasExceptions = ExceptionNO.selectByFu(io.ldin(i).bits.uop.exceptionVec, LduCfg).asUInt.orR
      val need_rep = io.ldin(i).bits.rep_info.need_rep

      when (!need_rep) {
      // update control flag
        addrvalid(loadWbIndex) := hasExceptions || !io.ldin(i).bits.tlbMiss || io.ldin(i).bits.isSWPrefetch
        datavalid(loadWbIndex) :=
          (if (EnableFastForward) {
              hasExceptions ||
              io.ldin(i).bits.mmio ||
             !io.ldin(i).bits.miss && // dcache miss
             !io.ldin(i).bits.dcacheRequireReplay || // do not writeback if that inst will be resend from rs
              io.ldin(i).bits.isSWPrefetch
           } else {
              hasExceptions ||
              io.ldin(i).bits.mmio ||
             !io.ldin(i).bits.miss ||
              io.ldin(i).bits.isSWPrefetch
           })

        //
        when (io.ldin(i).bits.data_wen_dup(1)) {
          uop(loadWbIndex) := io.ldin(i).bits.uop
        }
        when (io.ldin(i).bits.data_wen_dup(4)) {
          uop(loadWbIndex).debugInfo := io.ldin(i).bits.uop.debugInfo
        }
        uop(loadWbIndex).debugInfo := io.ldin(i).bits.rep_info.debug

        //  Debug info
        debug_mmio(loadWbIndex) := io.ldin(i).bits.mmio
        debug_paddr(loadWbIndex) := io.ldin(i).bits.paddr

        XSInfo(io.ldin(i).valid,
          "load hit write to lq idx %d pc 0x%x vaddr %x paddr %x mask %x forwardData %x forwardMask: %x mmio %x isvec %x\n",
          io.ldin(i).bits.uop.lqIdx.asUInt,
          io.ldin(i).bits.uop.pc,
          io.ldin(i).bits.vaddr,
          io.ldin(i).bits.paddr,
          io.ldin(i).bits.mask,
          io.ldin(i).bits.forwardData.asUInt,
          io.ldin(i).bits.forwardMask.asUInt,
          io.ldin(i).bits.mmio,
          io.ldin(i).bits.isvec
        )
      }
    }
  }

  //  perf counter
  QueuePerf(VirtualLoadQueueSize, validCount, !allowEnqueue)
  val vecValidVec = WireInit(VecInit((0 until VirtualLoadQueueSize).map(i => allocated(i) && isvec(i))))
  QueuePerf(VirtualLoadQueueSize, PopCount(vecValidVec), !allowEnqueue)
  io.lqFull := !allowEnqueue
  val perfEvents: Seq[(String, UInt)] = Seq()
  generatePerfEvent()

  // debug info
  XSDebug("enqPtrExt %d:%d deqPtrExt %d:%d\n", enqPtrExt(0).flag, enqPtr, deqPtr.flag, deqPtr.value)

  def PrintFlag(flag: Bool, name: String): Unit = {
    when(flag) {
      XSDebug(false, true.B, name)
    }.otherwise {
      XSDebug(false, true.B, " ")
    }
  }

  for (i <- 0 until VirtualLoadQueueSize) {
    XSDebug(s"$i pc %x pa %x ", uop(i).pc, debug_paddr(i))
    PrintFlag(allocated(i), "v")
    PrintFlag(allocated(i) && datavalid(i), "d")
    PrintFlag(allocated(i) && addrvalid(i), "a")
    PrintFlag(allocated(i) && addrvalid(i) && datavalid(i), "w")
    PrintFlag(allocated(i) && isvec(i), "c")
    XSDebug(false, true.B, "\n")
  }
  // end
}
