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

package xiangshan.backend.dispatch

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import xs.utils._
import xs.utils.perf._
import xiangshan._
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.Bundles.{DynInst, SimpleVPUCtrlSignals, UopIdx}
import xiangshan.backend.fu.FuType
import xiangshan.frontend.{FtqPtr, PreDecodeInfo}
import xiangshan.mem.{LqPtr, SqPtr}
import xiangshan.backend.fu.vector.Bundles.NumLsElem
import xs.utils.queue.QPtrMatchMatrix

// for dq datamodule
  class DqBundle(implicit p: Parameters) extends XSBundle {
    // passed from StaticInst
    val instr           = UInt(32.W)
    // val pc              = UInt(VAddrBits.W)
    // val foldpc          = UInt(MemPredPCWidth.W)
    // val exceptionVec    = ExceptionVec()
    val isFetchMalAddr  = Bool()
    // val hasException    = Bool()
    val trigger         = TriggerAction()
    val preDecodeInfo   = new PreDecodeInfo
    val pred_taken      = Bool()
    val crossPageIPFFix = Bool()
    val ftqPtr          = new FtqPtr
    val ftqOffset       = UInt(log2Up(PredictWidth).W)
    // passed from DecodedInst
    val srcType         = Vec(backendParams.numSrc, SrcType())
    // val ldest           = UInt(LogicRegsWidth.W)
    val fuType          = FuType()
    val fuOpType        = FuOpType()
    val rfWen           = Bool()
    val fpWen           = Bool()
    val vecWen          = Bool()
    val v0Wen           = Bool()
    val vlWen           = Bool()
    // val isXSTrap        = Bool()
    // val waitForward     = Bool() // no speculate execution
    // val blockBackward   = Bool()
    val flushPipe       = Bool() // This inst will flush all the pipe when commit, like exception but can commit
    // val canRobCompress  = Bool()
    val selImm          = SelImm()
    val imm             = UInt(32.W)
    val fpu             = new FPUCtrlSignals
    val vpu             = new SimpleVPUCtrlSignals
    val vlsInstr        = Bool()
    val wfflags         = Bool()
    // val isMove          = Bool()
    val uopIdx          = UopIdx()
    val isVset          = Bool()
    val firstUop        = Bool()
    val lastUop         = Bool()
    // val numUops         = UInt(log2Up(MaxUopSize).W) // rob need this
    // val numWB           = UInt(log2Up(MaxUopSize).W) // rob need this
    // val commitType      = CommitType()
    // rename
    val srcState        = Vec(backendParams.numSrc, SrcState())
    val srcLoadDependency  = Vec(backendParams.numSrc, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))
    val psrc            = Vec(backendParams.numSrc, UInt(PhyRegIdxWidth.W))
    val pdest           = UInt(PhyRegIdxWidth.W)
    // reg cache
    // val useRegCache     = Vec(backendParams.numIntRegSrc, Bool())
    // val regCacheIdx     = Vec(backendParams.numIntRegSrc, UInt(RegCacheIdxWidth.W))
    val robIdx          = new RobPtr
    // val instrSize       = UInt(log2Ceil(RenameWidth + 1).W)
    // val dirtyFs         = Bool()
    // val dirtyVs         = Bool()
    // val traceBlockInPipe = new TracePipe(log2Up(RenameWidth * 2 + 1))

    // val eliminatedMove  = Bool()
    // Take snapshot at this CFI inst
    // val snapshot        = Bool()
    val debugInfo       = new PerfDebugInfo
    val mdpTag = UInt(MemPredPCWidth.W)
    // Todo
    val lqIdx = new LqPtr
    val sqIdx = new SqPtr
    // debug module
    val singleStep      = Bool()
    // schedule
    val replayInst      = Bool()

    val debug_fuType    = OptionWrapper(backendParams.debugEn, FuType())

    val numLsElem       = NumLsElem()
  }

class DispatchQueueIO(enqnum: Int, deqnum: Int, size: Int)(implicit p: Parameters) extends XSBundle {
  val fanoutNum= 4
  val enq = new Bundle {
    val canAccept_fanout = Vec(fanoutNum, Output(Bool()))
    // output: dispatch queue can accept new requests
    val canAccept = Output(Bool())
    // input: need to allocate new entries (for address computing)
    val needAlloc = Vec(enqnum, Input(Bool()))
    // input: actually do the allocation (for write enable)
    val req = Vec(enqnum, Flipped(ValidIO(new DynInst)))
  }
  val deq = Vec(deqnum, DecoupledIO(new DynInst))
  val redirect = Flipped(ValidIO(new Redirect))
  val dqFull = Output(Bool())
  val validDeq0Num = Output(UInt(size.U.getWidth.W))
  val validDeq1Num = Output(UInt(size.U.getWidth.W))
}

// dispatch queue: accepts at most enqnum uops from dispatch1 and dispatches deqnum uops at every clock cycle
class DispatchQueue(size: Int, enqnum: Int, deqnum: Int, dqIndex: Int = 0)(implicit p: Parameters)
  extends XSModule with HasCircularQueuePtrHelper with HasPerfEvents {
  val fanoutNum= 4
  val io = IO(new DispatchQueueIO(enqnum, deqnum, size))
  // require(dpParams.IntDqDeqWidth == 8, "dpParams.IntDqDeqWidth must be 8")
  // require(backendParams.intSchdParams.get.issueBlockParams.size == 4, "int issueBlockParams must be 4")
  // backendParams.intSchdParams.get.issueBlockParams.map(x => require(x.exuBlockParams.size == 2, "int issueBlockParam's must be 2"))

  val s_invalid :: s_valid :: Nil = Enum(2)

  // queue data array
  private def hasRen: Boolean = true
  val dataModule = Module(new SyncDataModuleTemplate(new DqBundle, size, 2 * deqnum, enqnum, hasRen = hasRen))
  val robIdxEntries = Reg(Vec(size, new RobPtr))
  val stateEntries = RegInit(VecInit(Seq.fill(size)(s_invalid)))
  val validDeq0 = RegInit(VecInit(Seq.fill(size)(false.B)))
  val validDeq1 = RegInit(VecInit(Seq.fill(size)(false.B)))
  io.validDeq0Num := PopCount(validDeq0.zip(stateEntries).map{case (v, s) => v && (s===s_valid)})
  io.validDeq1Num := PopCount(validDeq1.zip(stateEntries).map{case (v, s) => v && (s===s_valid)})

  class DispatchQueuePtr extends CircularQueuePtr[DispatchQueuePtr](size)

  // head: first valid entry (dispatched entry)
  val headPtr = RegInit(VecInit((0 until 2 * deqnum).map(_.U.asTypeOf(new DispatchQueuePtr))))
  val headPtrNext = Wire(Vec(2 * deqnum, new DispatchQueuePtr))
  val headPtrMask = UIntToMask(headPtr(0).value, size)
  val headPtrOH = RegInit(1.U(size.W))
  val headPtrOHShift = CircularShift(headPtrOH)
  val headPtrOHVec = VecInit.tabulate(deqnum + 1)(headPtrOHShift.left)
  // tail: first invalid entry (free entry)
  val tailPtr = RegInit(VecInit((0 until enqnum).map(_.U.asTypeOf(new DispatchQueuePtr))))
  val tailPtrMask = UIntToMask(tailPtr(0).value, size)
  val tailPtrOH = RegInit(1.U(size.W))
  val tailPtrOHShift = CircularShift(tailPtrOH)
  val tailPtrOHVec = VecInit.tabulate(enqnum + 1)(tailPtrOHShift.left)
  // valid entries counter
  val validCounter = RegInit(0.U(log2Ceil(size + 1).W))
  val allowEnqueue = RegInit(true.B)
  val allowEnqueue_dup_fanout = RegInit((VecInit.fill(fanoutNum)(false.B)))

  val isTrueEmpty = !VecInit(stateEntries.map(_ === s_valid)).asUInt.orR
  val canEnqueue = allowEnqueue

  /**
   * Part 1: update states and uops when enqueue, dequeue, commit, redirect/replay
   *
   * uop only changes when a new instruction enqueues.
   *
   * state changes when
   * (1) enqueue: from s_invalid to s_valid
   * (2) dequeue: from s_valid to s_dispatched
   * (3) commit: from s_dispatched to s_invalid
   * (4) redirect (branch misprediction or exception): from any state to s_invalid (flushed)
   * (5) redirect (replay): from s_dispatched to s_valid (re-dispatch)
   */
  // enqueue: from s_invalid to s_valid
  io.enq.canAccept := canEnqueue
  for (i <- 0 until fanoutNum) {
    io.enq.canAccept_fanout(i) := allowEnqueue_dup_fanout(i)
  }
  val enqOffset = (0 until enqnum).map(i => PopCount(io.enq.needAlloc.take(i)))
  val enqIndexOH = (0 until enqnum).map(i => tailPtrOHVec(enqOffset(i)))
  for (i <- 0 until size) {
    val validVec = io.enq.req.map(_.valid).zip(enqIndexOH).map{ case (v, oh) => v && oh(i) }
    when (VecInit(validVec).asUInt.orR && canEnqueue) {
      robIdxEntries(i) := Mux1H(validVec, io.enq.req.map(_.bits.robIdx))
      stateEntries(i) := s_valid
      if (dqIndex == 0) {
        validDeq0(i) := FuType.isIntDq0Deq0(Mux1H(validVec, io.enq.req.map(_.bits.fuType)))
        validDeq1(i) := FuType.isIntDq0Deq1(Mux1H(validVec, io.enq.req.map(_.bits.fuType)))
      }
      else {
        validDeq0(i) := FuType.isIntDq1Deq0(Mux1H(validVec, io.enq.req.map(_.bits.fuType)))
        validDeq1(i) := FuType.isIntDq1Deq1(Mux1H(validVec, io.enq.req.map(_.bits.fuType)))
      }
    }
  }
  for (i <- 0 until enqnum) {
    dataModule.io.wen(i) := canEnqueue && io.enq.req(i).valid
    dataModule.io.waddr(i) := tailPtr(enqOffset(i)).value
    (dataModule.io.wdata(i): Data).waiveAll :<= (io.enq.req(i).bits: Data).waiveAll
  }

  // dequeue: from s_valid to s_dispatched
  for (i <- 0 until size) {
    val validVec = io.deq.map(_.fire).zip(headPtrOHVec).map{ case (v, oh) => v && oh(i) }
    when (VecInit(validVec).asUInt.orR && !io.redirect.valid) {
      stateEntries(i) := s_invalid
    }
  }

  // redirect: cancel uops currently in the queue
  val needCancel = Wire(Vec(size, Bool()))
  for (i <- 0 until size) {
    needCancel(i) := stateEntries(i) =/= s_invalid && robIdxEntries(i).needFlush(io.redirect)

    when(needCancel(i)) {
      stateEntries(i) := s_invalid
    }

    XSInfo(needCancel(i), p"valid entry($i): robIndex ${robIdxEntries(i)} " +
      p"cancelled with redirect robIndex 0x${Hexadecimal(io.redirect.bits.robIdx.asUInt)}\n")
  }

  /**
   * Part 2: update indices
   *
   * tail: (1) enqueue; (2) redirect
   * head: dequeue
   */

  // dequeue
  val currentValidCounter = distanceBetween(tailPtr(0), headPtr(0))
  val numDeqTryMask = Mux(currentValidCounter >= deqnum.U,
    // all deq are valid
    (1 << deqnum).U,
    // only the valid bits are set
    UIntToOH(currentValidCounter, deqnum)
  )
  val deqEnable_n = io.deq.zipWithIndex.map { case (deq, i) =>
    // For dequeue, the first entry should never be s_invalid
    // Otherwise, there should be a redirect and tail walks back
    // in this case, we set numDeq to 0
    if (i == 0) !deq.fire || numDeqTryMask(i)
    // When the state is s_invalid, we set deqEnable_n to false.B because
    // the entry may leave earlier and require to move forward the deqPtr.
    else (!deq.fire && stateEntries(headPtr(i).value) =/= s_invalid) || numDeqTryMask(i)
  } :+ true.B
  val numDeq = PriorityEncoder(deqEnable_n)
  // agreement with reservation station: don't dequeue when redirect.valid
  for (i <- 0 until 2 * deqnum) {
    headPtrNext(i) := Mux(io.redirect.valid, headPtr(i), headPtr(i) + numDeq)
  }
  headPtr := headPtrNext
  headPtrOH := Mux(io.redirect.valid, headPtrOH, ParallelPriorityMux(deqEnable_n, headPtrOHVec))
  XSError(headPtrOH =/= headPtr.head.toOH, p"head: $headPtrOH != UIntToOH(${headPtr.head})")

  // For branch mis-prediction or memory violation replay,
  // we delay updating the indices for one clock cycle.
  // For now, we simply use PopCount to count #instr cancelled.
  val lastCycleMisprediction = GatedValidRegNext(io.redirect.valid)
  // find the last one's position, starting from headPtr and searching backwards
  val validBitVec = VecInit((0 until size).map(i => stateEntries(i) === s_valid))
  val loValidBitVec = Cat((0 until size).map(i => validBitVec(i) && headPtrMask(i)))
  val hiValidBitVec = Cat((0 until size).map(i => validBitVec(i) && !headPtrMask(i)))
  val flippedFlag = loValidBitVec.orR || validBitVec(size - 1)
  val leadingZeros = PriorityEncoder(Mux(loValidBitVec.orR, loValidBitVec, hiValidBitVec))
  val lastOneIndex = Mux(leadingZeros === 0.U, 0.U, size.U - leadingZeros)
  val walkedTailPtr = Wire(new DispatchQueuePtr)
  walkedTailPtr.flag := flippedFlag ^ headPtr(0).flag
  walkedTailPtr.value := lastOneIndex

  // enqueue
  val numEnq = Mux(io.enq.canAccept, PopCount(io.enq.req.map(_.valid)), 0.U)
  val numNeedAlloc = Mux(io.enq.canAccept, PopCount(io.enq.needAlloc), 0.U)
  tailPtr(0) := Mux(io.redirect.valid,
    tailPtr(0),
    Mux(lastCycleMisprediction,
      Mux(isTrueEmpty, headPtr(0), walkedTailPtr),
      tailPtr(0) + numEnq)
  )
  val lastLastCycleMisprediction = GatedValidRegNext(lastCycleMisprediction)
  for (i <- 1 until enqnum) {
    tailPtr(i) := Mux(io.redirect.valid,
      tailPtr(i),
      Mux(lastLastCycleMisprediction,
        tailPtr(0) + i.U,
        tailPtr(i) + numEnq)
    )
  }
  tailPtrOH := Mux(lastLastCycleMisprediction, tailPtr.head.toOH, tailPtrOHVec(numEnq))
  val tailPtrOHAccurate = !lastCycleMisprediction && !lastLastCycleMisprediction
  XSError(tailPtrOHAccurate && tailPtrOH =/= tailPtr.head.toOH, p"tail: $tailPtrOH != UIntToOH(${tailPtr.head})")

  // update valid counter and allowEnqueue reg
  validCounter := Mux(io.redirect.valid,
    validCounter,
    Mux(lastLastCycleMisprediction,
      currentValidCounter,
      validCounter + numEnq - numDeq)
  )
  allowEnqueue := (numNeedAlloc +& currentValidCounter <= (size - enqnum).U) || (numNeedAlloc +& currentValidCounter - (size - enqnum).U <= numDeq)
  for (i <- 0 until fanoutNum) {
    allowEnqueue_dup_fanout(i) := (numNeedAlloc +& currentValidCounter <= (size - enqnum).U) || (numNeedAlloc +& currentValidCounter - (size - enqnum).U <= numDeq)
  }

  /**
   * Part 3: set output valid and data bits
   */
  val deqData = Reg(Vec(deqnum, new DynInst))
  // How to pipeline the data read:
  // T: get the required read data
  for (i <- 0 until deqnum) {
    io.deq(i).bits := deqData(i)
    // Some bits have bad timing in Dispatch but will not be used at Dispatch2
    // They will use the slow path from data module
    io.deq(i).bits.fpu := dataModule.io.rdata(i).fpu
    // do not dequeue when io.redirect valid because it may cause dispatchPtr work improperly
    io.deq(i).valid := Mux1H(headPtrOHVec(i), stateEntries) === s_valid && !lastCycleMisprediction
  }
  // T-1: select data from the following (deqnum + 1 + numEnq) sources with priority
  // For data(i): (1) current output (deqnum - i); (2) next-step data (i + 1)
  // For the next-step data(i): (1) enqueue data (enqnum); (2) data from storage (1)
  val nextStepData = Wire(Vec(2 * deqnum, new DynInst))
  val currentData = Wire(Vec(2 * deqnum, new DynInst))
  val ptrMatch = new QPtrMatchMatrix(headPtr, tailPtr)
  for (i <- 0 until 2 * deqnum) {
    val enqMatchVec = VecInit(ptrMatch(i))
    val enqBypassEnVec = io.enq.needAlloc.zip(enqOffset).map{ case (v, o) => v && enqMatchVec(o) }
    val enqBypassEn = io.enq.canAccept && VecInit(enqBypassEnVec).asUInt.orR
    val enqBypassData = Mux1H(enqBypassEnVec, io.enq.req.map(_.bits))
    currentData(i) := DontCare
    (currentData(i): Data).waiveAll :<= (dataModule.io.rdata(i): Data).waiveAll
    val readData = if (i < deqnum) deqData(i) else currentData(i)
    nextStepData(i) := Mux(enqBypassEn, enqBypassData, readData)
  }
  for (i <- 0 until deqnum) {
    when (!io.redirect.valid) {
      deqData(i) := ParallelPriorityMux(deqEnable_n, nextStepData.drop(i).take(deqnum + 1))
    }
  }
  // T-2: read data from storage: next
  for (i <- 0 until 2 * deqnum) {
    dataModule.io.ren.get(i) := io.redirect.valid || io.deq.map(_.valid).reduce(_|_)
  }
  dataModule.io.raddr := headPtrNext.map(_.value)

  // debug: dump dispatch queue states
  XSDebug(p"head: ${headPtr(0)}, tail: ${tailPtr(0)}\n")
  XSDebug(p"state: ")
  stateEntries.reverse.foreach { s =>
    XSDebug(false, s === s_invalid, "-")
    XSDebug(false, s === s_valid, "v")
  }
  XSDebug(false, true.B, "\n")
  XSDebug(p"ptr:   ")
  (0 until size).reverse.foreach { i =>
    val isPtr = i.U === headPtr(0).value || i.U === tailPtr(0).value
    XSDebug(false, isPtr, "^")
    XSDebug(false, !isPtr, " ")
  }
  XSDebug(false, true.B, "\n")

  // XSError(isAfter(headPtr(0), tailPtr(0)), p"assert greaterOrEqualThan(tailPtr: ${tailPtr(0)}, headPtr: ${headPtr(0)}) failed\n")
  QueuePerf(size, PopCount(stateEntries.map(_ =/= s_invalid)), !canEnqueue)
  io.dqFull := !canEnqueue
  XSPerfAccumulate("in", numEnq)
  XSPerfAccumulate("out", PopCount(io.deq.map(_.fire)))
  XSPerfAccumulate("out_try", PopCount(io.deq.map(_.valid)))
  val fake_block = currentValidCounter <= (size - enqnum).U && !canEnqueue
  XSPerfAccumulate("fake_block", fake_block)

  val validEntries = RegNext(PopCount(stateEntries.map(_ =/= s_invalid)))
  val perfEvents = Seq(
    ("dispatchq_in",         numEnq                                                          ),
    ("dispatchq_out",        PopCount(io.deq.map(_.fire))                                    ),
    ("dispatchq_out_try",    PopCount(io.deq.map(_.valid))                                   ),
    ("dispatchq_fake_block", fake_block                                                      ),
    ("dispatchq_1_4_valid ", validEntries <  (size / 4).U                                    ),
    ("dispatchq_2_4_valid ", validEntries >= (size / 4).U && validEntries <= (size / 2).U    ),
    ("dispatchq_3_4_valid ", validEntries >= (size / 2).U && validEntries <= (size * 3 / 4).U),
    ("dispatchq_4_4_valid ", validEntries >= (size * 3 / 4).U                                )
  )
  generatePerfEvent()
}
