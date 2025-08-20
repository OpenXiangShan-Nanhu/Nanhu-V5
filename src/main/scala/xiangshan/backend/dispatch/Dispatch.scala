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
import difftest._
import utils._
import xs.utils._
import xs.utils.perf._
import xiangshan.ExceptionNO._
import xiangshan._
import xiangshan.backend.MemCoreTopDownIO
import xiangshan.backend.rob.{RobDispatchTopDownIO, RobEnqIO, RobPtr}
import xiangshan.mem.mdp._
import xiangshan.backend.Bundles.DynInst
import xiangshan.backend.fu.FuType

case class DispatchParameters
(
  IntDqSize: Int,
  FpDqSize: Int,
  LsDqSize: Int,
  IntDqDeqWidth: Int,
  FpDqDeqWidth: Int,
  VecDqDeqWidth: Int,
  LsDqDeqWidth: Int
)

class CoreDispatchTopDownIO extends Bundle {
  val l2MissMatch = Input(Bool())
  val l3MissMatch = Input(Bool())
  val fromMem = Flipped(new MemCoreTopDownIO)
}

// read rob and enqueue
class Dispatch(implicit p: Parameters) extends XSModule with HasPerfEvents {
  private val fanoutNum = 4 // 0 to identify and ready  1 to rob   2 to dq   3 to perf & busytable
  private val blockWindow = 7
  val io = IO(new Bundle() {
    val hartId = Input(UInt(hartIdLen.W))
    // from rename
    // 0 to identify and ready  1 to rob   2 to dq   3 to perf & busytable
    val fromRename = Vec(fanoutNum, Vec(RenameWidth, Flipped(DecoupledIO(new DynInst))))
    val toRenameAllFire = Output(Bool())
    // enq Rob
    val enqRob = Flipped(new RobEnqIO)
    // enq Lsq
    val allocPregs = Vec(RenameWidth, Output(new ResetPregStateReq))
    // to dispatch queue
    val toIntDq = new Bundle {
      val canAccept_fanout = Vec(fanoutNum, Input(Bool()))
      val canAccept = Input(Bool())
      val needAlloc = Vec(RenameWidth, Output(Bool()))
      val req = Vec(RenameWidth, ValidIO(new DynInst))
    }
    val toIntDq1 = new Bundle {
      val canAccept_fanout = Vec(fanoutNum, Input(Bool()))
      val canAccept = Input(Bool())
      val needAlloc = Vec(RenameWidth, Output(Bool()))
      val req = Vec(RenameWidth, ValidIO(new DynInst))
    }
    val intIQValidNumVec = Input(MixedVec(backendParams.genIntIQValidNumBundle))
    // val fpIQValidNumVec = Input(MixedVec(backendParams.genFpIQValidNumBundle))
    val fromIntDQ = new Bundle {
      val intDQ0ValidDeq0Num = Input(UInt(dpParams.IntDqSize.U.getWidth.W))
      val intDQ0ValidDeq1Num = Input(UInt(dpParams.IntDqSize.U.getWidth.W))
      val intDQ1ValidDeq0Num = Input(UInt(dpParams.IntDqSize.U.getWidth.W))
      val intDQ1ValidDeq1Num = Input(UInt(dpParams.IntDqSize.U.getWidth.W))
    }
    // val toFpDq = new Bundle {
    //   val canAccept = Input(Bool())
    //   val needAlloc = Vec(RenameWidth, Output(Bool()))
    //   val req = Vec(RenameWidth, ValidIO(new DynInst))
    // }
    val toVecDq = new Bundle {
      val canAccept_fanout = Vec(fanoutNum, Input(Bool()))
      val canAccept = Input(Bool())
      val needAlloc = Vec(RenameWidth, Output(Bool()))
      val req = Vec(RenameWidth, ValidIO(new DynInst))
    }
    val toLsDq = new Bundle {
      val canAccept_fanout = Vec(fanoutNum, Input(Bool()))
      val canAccept = Input(Bool())
      val needAlloc = Vec(RenameWidth, Output(Bool()))
      val req = Vec(RenameWidth, ValidIO(new DynInst))
    }
    val redirect = Flipped(ValidIO(new Redirect))
    // singleStep
    val singleStep = Input(Bool())
    // perf only
    val robHead = Input(new DynInst)
    val stallReason = Flipped(new StallReasonIO(RenameWidth))
    val lqCanAccept = Input(Bool())
    val sqCanAccept = Input(Bool())
    val robHeadNotReady = Input(Bool())
    val robFull = Input(Bool())
    val debugTopDown = new Bundle {
      val fromRob = Flipped(new RobDispatchTopDownIO)
      val fromCore = new CoreDispatchTopDownIO
    }
    val cmoFinish = Input(Bool())
    val dispatchIsInBlock = Output(Bool())

    def toDq = Seq(toIntDq, toVecDq, toLsDq)
  })

  /**
    * Part 1: choose the target dispatch queue and the corresponding write ports
    */
  // valid bits for different dispatch queues
  val isInt = VecInit(io.fromRename(0).map(req => req.valid && FuType.isInt(req.bits.fuType)))
  val isIntDq0 = VecInit(io.fromRename(0).map(req => req.valid && FuType.isIntDq0(req.bits.fuType)))
  val isIntDq1 = VecInit(io.fromRename(0).map(req => req.valid && FuType.isIntDq1(req.bits.fuType)))
  val isAlu = VecInit(io.fromRename(0).map(req => req.valid && FuType.isBothDeq0(req.bits.fuType)))
  val isBrh = VecInit(io.fromRename(0).map(req => req.valid && (FuType.isBrh(req.bits.fuType) || FuType.isJump(req.bits.fuType))))
  val popAlu = isAlu.zipWithIndex.map { case (_, i) => PopCount(isAlu.take(i + 1)) }
  val popBrh = isBrh.zipWithIndex.map { case (_, i) => PopCount(isBrh.take(i + 1)) }
  val isOnlyDq0 = VecInit(isIntDq0.zip(isIntDq1).map { case (dq0, dq1) => dq0 && !dq1 })
  val isOnlyDq1 = VecInit(isIntDq0.zip(isIntDq1).map { case (dq0, dq1) => dq1 && !dq0 })
  val isBothDq01 = VecInit(isIntDq0.zip(isIntDq1).map { case (dq0, dq1) => dq0 || dq1 }) // alu,brh
  val IQ0Deq0Num = io.intIQValidNumVec(0)(0)
  val IQ0Deq1Num = io.intIQValidNumVec(0)(1)
  val IQ1Deq0Num = io.intIQValidNumVec(1)(0)
  val IQ1Deq1Num = io.intIQValidNumVec(1)(1)
  val IQ2Deq0Num = io.intIQValidNumVec(2)(0)
  val IQ2Deq1Num = io.intIQValidNumVec(2)(1)
  val IQ3Deq0Num = io.intIQValidNumVec(3)(0)
  val IQ3Deq1Num = io.intIQValidNumVec(3)(1)
  val DQ0Deq0 = io.fromIntDQ.intDQ0ValidDeq0Num
  val DQ0Deq1 = io.fromIntDQ.intDQ0ValidDeq1Num
  val DQ1Deq0 = io.fromIntDQ.intDQ1ValidDeq0Num
  val DQ1Deq1 = io.fromIntDQ.intDQ1ValidDeq1Num
  val IQ01Deq0 = IQ0Deq0Num +& IQ1Deq0Num
  val IQ01Deq1 = IQ0Deq1Num +& IQ1Deq1Num
  val IQ23Deq0 = IQ2Deq0Num +& IQ3Deq0Num
  val IQ23Deq1 = IQ2Deq1Num +& IQ3Deq1Num
  val Dq0EnqDeq0 = PopCount(isOnlyDq0)
  val Dq1EnqDeq1 = PopCount(isOnlyDq1)
  val Dq0SumDeq0 = DQ0Deq0 +& IQ01Deq0 +& Dq0EnqDeq0
  val Dq0SumDeq1 = DQ0Deq1 +& IQ01Deq1
  val Dq1SumDeq0 = DQ1Deq0 +& IQ23Deq0
  val Dq1SumDeq1 = DQ1Deq1 +& IQ23Deq1 +& Dq1EnqDeq1
  val lessDeq0IsDq0 = Dq1SumDeq0 > Dq0SumDeq0
  val lessDeq1IsDq0 = (Dq1SumDeq1 << 1).asUInt > Dq0SumDeq1
  val equalDeq0IsDq0 = Dq1SumDeq0 === Dq0SumDeq0
  val equalDeq1IsDq0 = (Dq1SumDeq1 << 1).asUInt  === Dq0SumDeq1
  // val diffDeq0 = Mux(lessDeq0IsDq0, Dq1SumDeq0 - Dq0SumDeq0, Dq0SumDeq0 - Dq1SumDeq0)
  // val diffDeq1 = Mux(lessDeq1IsDq0, Dq1SumDeq1 - Dq0SumDeq1, Dq0SumDeq1 - Dq1SumDeq1)
  // val popAluIsMore = popAlu.map(_ > diffDeq0)
  // val popBrhIsMore = popBrh.map(_ > diffDeq1)
  val lastLastAluSelectDq0 = RegInit(false.B)
  val lastLastBrhSelectDq0 = RegInit(false.B)
  // val aluSelectLessDq = isAlu.zip(popAluIsMore).zip(popAlu).map { case ((i, pm), p) => i && (!pm || (pm && p(0).asBool) ^ lastLastAluSelectDq0) }
  // val brhSelectLessDq = isBrh.zip(popBrhIsMore).zip(popBrh).map { case ((i, pm), p) => i && (!pm || (pm && p(0).asBool) ^ lastLastBrhSelectDq0) }
  val aluSelectDq0 = isAlu.zip(popAlu).map { case (i, p) => i && (p(0).asBool ^ lastLastAluSelectDq0) }
  //val brhSelectDq0 = isBrh.zip(popBrh).map { case (i, p) => i && (p(0).asBool ^ lastLastBrhSelectDq0) }
  val brhSelectDq0 = isBrh.zip(popBrh).map { case (i, p) => i && !(lastLastBrhSelectDq0 && (p === 1.U || p === 4.U) || !lastLastBrhSelectDq0 && (p === 3.U || p === 6.U))}
  val lastAluSelectDq0 = PriorityMuxDefault(isAlu.reverse.zip(aluSelectDq0.reverse), lastLastAluSelectDq0)
  val lastBrhSelectDq0 = PriorityMuxDefault(isBrh.reverse.zip(brhSelectDq0.reverse), lastLastBrhSelectDq0)
  when(isAlu.asUInt.orR && io.toIntDq.canAccept && io.toIntDq1.canAccept){
    lastLastAluSelectDq0 := Mux(equalDeq0IsDq0, lastAluSelectDq0, !lessDeq0IsDq0)
  }
  when(isBrh.asUInt.orR && io.toIntDq.canAccept && io.toIntDq1.canAccept){
    lastLastBrhSelectDq0 := Mux(equalDeq1IsDq0, lastBrhSelectDq0, !lessDeq1IsDq0)
  }
  val toIntDq0Valid = Wire(Vec(RenameWidth, Bool()))
  val toIntDq1Valid = Wire(Vec(RenameWidth, Bool()))
  toIntDq0Valid.indices.map { case i =>
    toIntDq0Valid(i) := Mux(!io.toIntDq.canAccept, false.B, Mux(!io.toIntDq1.canAccept, isOnlyDq0(i) || isBothDq01(i), isOnlyDq0(i) || aluSelectDq0(i) || brhSelectDq0(i)))
  }
  toIntDq1Valid.indices.map { case i =>
    toIntDq1Valid(i) := Mux(!io.toIntDq1.canAccept, false.B, Mux(!io.toIntDq.canAccept, isOnlyDq1(i) || isBothDq01(i), isOnlyDq1(i) || (isAlu(i) ^ aluSelectDq0(i)) || (isBrh(i) && !brhSelectDq0(i))))
  }
  val isBranch = VecInit(io.fromRename(0).map(req =>
    // cover auipc (a fake branch)
    (!req.bits.preDecodeInfo.notCFI || FuType.isJump(req.bits.fuType)) && req.valid
  ))
  val isFp = VecInit(io.fromRename(0).map(req => FuType.isFArith(req.bits.fuType) && req.valid))
  val isVec     = VecInit(io.fromRename(0).map(req => (FuType.isVArith (req.bits.fuType) ||
                                                  FuType.isVsetRvfWvf(req.bits.fuType)) && req.valid))
  val isMem    = VecInit(io.fromRename(0).map(req => (FuType.isMem(req.bits.fuType) ||
                                                  FuType.isVls (req.bits.fuType)) && req.valid))
  val isLs     = VecInit(io.fromRename(0).map(req => FuType.isLoadStore(req.bits.fuType) && req.valid))
  val isVls    = VecInit(io.fromRename(0).map(req => FuType.isVls (req.bits.fuType) && req.valid))
  val isStore  = VecInit(io.fromRename(0).map(req => FuType.isStore(req.bits.fuType) && req.valid))
  val isVStore = VecInit(io.fromRename(0).map(req => FuType.isVStore(req.bits.fuType) && req.valid))
  val isAMO    = VecInit(io.fromRename(0).map(req => FuType.isAMO(req.bits.fuType) && req.valid))
  val isCmo = VecInit(io.fromRename(0).map(req => req.valid && (LSUOpType.isCboAll(req.bits.fuOpType) && FuType.isStore(req.bits.fuType))))
  val isBlockBackward  = VecInit(io.fromRename(0).map(x => x.valid && x.bits.blockBackward))
  val isWaitForward    = VecInit(io.fromRename(0).map(x => x.valid && x.bits.waitForward))
  
  // Singlestep should only commit one machine instruction after dret, and then hart enter debugMode according to singlestep exception.
  val s_holdRobidx :: s_updateRobidx :: Nil = Enum(2)
  val singleStepState = RegInit(s_updateRobidx)
  
  val robidxStepNext  = WireInit(0.U.asTypeOf(io.fromRename(0)(0).bits.robIdx))
  val robidxStepReg   = RegInit(0.U.asTypeOf(io.fromRename(0)(0).bits.robIdx))
  val robidxCanCommitStepping = WireInit(0.U.asTypeOf(io.fromRename(0)(0).bits.robIdx))
  
  when(!io.singleStep) {
    singleStepState := s_updateRobidx
  }.elsewhen(io.singleStep && io.fromRename(0)(0).fire && io.enqRob.req(0).valid) {
    singleStepState := s_holdRobidx
    robidxStepNext := io.fromRename(0)(0).bits.robIdx
  }
  
  when(singleStepState === s_updateRobidx) {
    robidxStepReg := robidxStepNext
    robidxCanCommitStepping := robidxStepNext
  }.elsewhen(singleStepState === s_holdRobidx) {
    robidxStepReg := robidxStepReg 
    robidxCanCommitStepping := robidxStepReg
  }

  // 0 to identify and ready  1 to rob   2 to dq   3 to perf & busytable
  val updatedUop = Wire(Vec(fanoutNum, Vec(RenameWidth, new DynInst)))
  val checkpoint_id = RegInit(0.U(64.W))
  checkpoint_id := checkpoint_id + PopCount((0 until RenameWidth).map(i =>
    io.fromRename(0)(i).fire
  ))

  //cmo hardware fence state
  private val currentCycleNeedBlockVec = Wire(Vec(RenameWidth, Bool()))
  private val previousHasLs = Wire(Vec(RenameWidth, Bool()))
  private val hasValidCmo = io.fromRename(0).map(x => x.valid && (LSUOpType.isCboAll(x.bits.fuOpType) && FuType.isStore(x.bits.fuType))).reduce(_ || _)

  for (i <- 0 until fanoutNum) {
    updatedUop(i).zipWithIndex.foreach({case (update, j) =>
      update := io.fromRename(i)(j).bits
      update.debugInfo.eliminatedMove := io.fromRename(i)(j).bits.eliminatedMove
      // For the LUI instruction: psrc(0) is from register file and should always be zero.
      when (io.fromRename(i)(j).bits.isLUI) {
        update.psrc(0) := 0.U
      }

      // // update singleStep, singleStep exception only enable in next machine instruction.
      update.singleStep := io.singleStep && (io.fromRename(i)(j).bits.robIdx =/= robidxCanCommitStepping)
      when (io.fromRename(i)(j).fire) {
        XSDebug(TriggerAction.isDmode(update.trigger) || update.exceptionVec(breakPoint), s"Debug Mode: inst ${i} has frontend trigger exception\n")
        XSDebug(update.singleStep, s"Debug Mode: inst ${i} has single step exception\n")
      }
        // for fload vector read same regfile TD
      when (io.fromRename(i)(j).bits.srcType(0) === SrcType.fp) {
        update.srcType(0) := SrcType.vp
      }
      when (io.fromRename(i)(j).bits.srcType(1) === SrcType.fp) {
        update.srcType(1) := SrcType.vp
      }
      when (io.fromRename(i)(j).bits.srcType(2) === SrcType.fp) {
        update.srcType(2) := SrcType.vp
      }
    })
    
  }

  for (i <- 0 until RenameWidth) {
      if (env.EnableDifftest) {
      // debug runahead hint
      val debug_runahead_checkpoint_id = Wire(checkpoint_id.cloneType)
      if(i == 0){
        debug_runahead_checkpoint_id := checkpoint_id
      } else {
        debug_runahead_checkpoint_id := checkpoint_id + PopCount((0 until RenameWidth).map(j =>
          io.fromRename(1)(j).fire
        ))
      }
    }
  }

  /**
    * Part 3:
    *   acquire ROB (all), LSQ (load/store only) and dispatch queue slots
    *   only set valid when all of them provides enough entries
    */
  val toIntDqCanAccept = io.toIntDq.canAccept && io.toIntDq1.canAccept
  val allResourceReady = io.enqRob.canAccept && toIntDqCanAccept && io.toVecDq.canAccept && io.toLsDq.canAccept

  // Instructions should enter dispatch queues in order.
  // blockedByWaitForward: this instruction is blocked by itself (based on waitForward)
  // nextCanOut: next instructions can out (based on blockBackward)
  // notBlockedByPrevious: previous instructions can enqueue
  val hasException = VecInit(io.fromRename(0).zip(updatedUop(0)).map {
    case (fromRename: DecoupledIO[DynInst], uop: DynInst) =>
      fromRename.bits.hasException || uop.singleStep
  })

  private val blockedByWaitForward = Wire(Vec(RenameWidth, Bool()))
  blockedByWaitForward(0) := !io.enqRob.isEmpty && isWaitForward(0)
  for (i <- 1 until RenameWidth) {
    blockedByWaitForward(i) := blockedByWaitForward(i - 1) || (!io.enqRob.isEmpty || Cat(io.fromRename(0).take(i).map(_.valid)).orR) && isWaitForward(i)
  }
  if(backendParams.debugEn){
    dontTouch(blockedByWaitForward)
  }

  // Only the uop with block backward flag will block the next uop
  val nextCanOut = VecInit((0 until RenameWidth).map(i =>
    !isBlockBackward(i)
  ))
  val notBlockedByPrevious = VecInit((0 until RenameWidth).map(i =>
    if (i == 0) true.B
    else Cat((0 until i).map(j => nextCanOut(j))).andR
  ))

  // for noSpecExec: (robEmpty || !this.noSpecExec) && !previous.noSpecExec
  // For blockBackward:
  // this instruction can actually dequeue: 3 conditions
  // (1) resources are ready
  // (2) previous instructions are ready
  val thisCanActualOut = (0 until RenameWidth).map(i => !blockedByWaitForward(i) && notBlockedByPrevious(i))
  val thisActualOut = (0 until RenameWidth).map(i => io.enqRob.req(i).valid && io.enqRob.canAccept)
  val hasValidException = io.fromRename(0).zip(hasException).map(x => x._1.valid && x._2)

  //cmo hardware fence state

  private val currentCycleNeedBlock = currentCycleNeedBlockVec.reduce(_ || _)
  private val allDqReady = io.toIntDq.canAccept && io.toIntDq1.canAccept && io.toVecDq.canAccept && io.toLsDq.canAccept
  private val blockStartCnt = RegInit(blockWindow.U(log2Ceil(blockWindow).W))
  private val blockEndCnt = RegInit(blockWindow.U(log2Ceil(blockWindow).W))

  private val s_idle :: s_cmoBlock :: Nil = Enum(2)
  private val cmoBlockState = RegInit(s_idle)
  private val hasCmo = isCmo.reduce(_ || _)
  private val hasMem = isMem.reduce(_ || _)
  private val isMemNotCmo = isMem.zip(isCmo).map(x => x._1 && !x._2)

  for (i <- 0 until RenameWidth) {
    if (i > 0) {
      currentCycleNeedBlockVec(i) := hasValidCmo && isCmo(i) && io.fromRename(0)(i).fire && !hasValidException(i) && !VecInit(hasValidException.take(i)).asUInt.orR
      previousHasLs(i) := VecInit(isMemNotCmo.take(i)).asUInt.orR
    } else {
      currentCycleNeedBlockVec(i) := hasValidCmo && isCmo(i) && io.fromRename(0)(i).fire && !hasValidException(i)
      previousHasLs(i) := false.B
    }
  }

  when(cmoBlockState === s_cmoBlock && io.cmoFinish && !(blockEndCnt === 0.U)){
    blockEndCnt := blockEndCnt - 1.U
  }.elsewhen(cmoBlockState === s_idle || !io.cmoFinish){
    blockEndCnt := blockWindow.U
  }.otherwise{
    blockEndCnt := blockEndCnt
  }

  when(cmoBlockState === s_cmoBlock && !(blockStartCnt === 0.U)){
    blockStartCnt := blockStartCnt - 1.U
  }.elsewhen(cmoBlockState === s_idle){
    blockStartCnt := blockWindow.U
  }.otherwise{
    blockStartCnt := blockStartCnt
  }

  switch(cmoBlockState){
    is(s_idle){
      when(hasCmo && currentCycleNeedBlock && io.enqRob.canAccept && allDqReady && !io.redirect.valid){
        cmoBlockState := s_cmoBlock
      }.otherwise{
        cmoBlockState := cmoBlockState
      }
    }
    is(s_cmoBlock){
      when(currentCycleNeedBlock) {
        cmoBlockState := cmoBlockState
        blockEndCnt := blockWindow.U
      }.elsewhen(io.cmoFinish && (blockStartCnt === 0.U) && (blockEndCnt === 0.U)){
        cmoBlockState := s_idle
      }.otherwise{
        cmoBlockState := cmoBlockState
      }
    }
  }

  dontTouch(currentCycleNeedBlockVec)
  dontTouch(currentCycleNeedBlock)

  // All dispatch queues can accept new RenameWidth uops
  // A possibly better implementation is as follows. We need block all uops when some one DispatchQueue is full.
  //   However, dispatch stage cannot rely on the infos from rename stage, since the `rename.io.out.valid` relies on its
  // ready which is assigned from `dispatch.io.recv`. This would cause a combinational loop:
  //   dispatch.io.recv <- dispatch.io.fromRename.valid <- rename.io.out.valid <- rename.io.out.ready <- dispatch.io.recv
  // val dqCanAccept = !((isOnlyDq0.asUInt.orR && !io.toIntDq0.canAccept) || (isOnlyDq1.asUInt.orR && !io.toIntDq1.canAccept) ||
  //   (isBothDq01.asUInt.orR && !io.toIntDq0.canAccept && !io.toIntDq1.canAccept) ||
  //   (isFp.asUInt.orR && !io.toVecDq.canAccept) || (isLs.asUInt.orR && !io.toLsDq.canAccept))

  // Todo: use decode2dispatch bypass infos to loose `can accept` condition
  val dqCanAccept = io.toIntDq.canAccept && io.toIntDq1.canAccept && io.toVecDq.canAccept && io.toLsDq.canAccept
  val dqCanAccept_toRob = io.toIntDq.canAccept_fanout(0) && io.toIntDq1.canAccept_fanout(0) && io.toVecDq.canAccept_fanout(0) && io.toLsDq.canAccept_fanout(0)
  val dqCanAccept_tointDq = io.toIntDq.canAccept_fanout(1) && io.toIntDq1.canAccept_fanout(1) && io.toVecDq.canAccept_fanout(1) && io.toLsDq.canAccept_fanout(1)
  val dqCanAccept_tovecDq = io.toIntDq.canAccept_fanout(2) && io.toIntDq1.canAccept_fanout(2) && io.toVecDq.canAccept_fanout(2) && io.toLsDq.canAccept_fanout(2)
  val dqCanAccept_tolsDq = io.toIntDq.canAccept_fanout(3) && io.toIntDq1.canAccept_fanout(3) && io.toVecDq.canAccept_fanout(3) && io.toLsDq.canAccept_fanout(3)

  // input for ROB, LSQ, Dispatch Queue
  for (i <- 0 until RenameWidth) {
    io.enqRob.needAlloc(i) := io.fromRename(2)(i).valid
    io.enqRob.req(i).valid := io.fromRename(2)(i).valid && thisCanActualOut(i) && dqCanAccept_toRob && (cmoBlockState === s_idle || (cmoBlockState === s_cmoBlock && !previousHasLs(i) && !isMemNotCmo(i)))
    io.enqRob.req(i).bits := updatedUop(2)(i)
    io.enqRob.req(i).bits.hasException := updatedUop(2)(i).hasException || updatedUop(2)(i).singleStep
    io.enqRob.req(i).bits.numWB := Mux(updatedUop(2)(i).singleStep, 0.U, updatedUop(2)(i).numWB)
    XSDebug(io.enqRob.req(i).valid, p"pc 0x${Hexadecimal(io.fromRename(2)(i).bits.pc)} receives nrob ${io.enqRob.resp(i)}\n")

    // When previous instructions have exceptions, following instructions should not enter dispatch queues.
    val previousHasException = if (i == 0) false.B else VecInit(hasValidException.take(i)).asUInt.orR
    val canEnterDpq = !hasException(i) && thisCanActualOut(i) && !previousHasException && io.enqRob.canAccept

    // send uops to dispatch queues
    // Note that if one of their previous instructions cannot enqueue, they should not enter dispatch queue.
    val doesNotNeedExec = io.fromRename(1)(i).bits.eliminatedMove
    io.toIntDq.needAlloc(i) := io.fromRename(1)(i).valid && !doesNotNeedExec && isIntDq0(i) && toIntDq0Valid(i)
    io.toIntDq.req(i).valid := io.fromRename(1)(i).valid && !doesNotNeedExec && isIntDq0(i) && toIntDq0Valid(i) &&
      canEnterDpq && dqCanAccept_tointDq && (cmoBlockState === s_idle || (cmoBlockState === s_cmoBlock && !previousHasLs(i)))
    io.toIntDq.req(i).bits := updatedUop(1)(i)

    io.toIntDq1.needAlloc(i) := io.fromRename(1)(i).valid && isIntDq1(i) && !doesNotNeedExec && toIntDq1Valid(i)
    io.toIntDq1.req(i).valid := io.fromRename(1)(i).valid && isIntDq1(i) && !doesNotNeedExec && toIntDq1Valid(i) && 
      canEnterDpq && dqCanAccept_tointDq && (cmoBlockState === s_idle || (cmoBlockState === s_cmoBlock && !previousHasLs(i)))
    io.toIntDq1.req(i).bits := updatedUop(1)(i)

    io.toVecDq.needAlloc(i)  := io.fromRename(1)(i).valid && (isVec(i) || isFp(i))
    io.toVecDq.req(i).valid  := io.fromRename(1)(i).valid && (isVec(i) || isFp(i)) && canEnterDpq && dqCanAccept_tovecDq && 
      (cmoBlockState === s_idle || (cmoBlockState === s_cmoBlock && !previousHasLs(i)))
    io.toVecDq.req(i).bits   := updatedUop(1)(i)

    io.toLsDq.needAlloc(i)  := io.fromRename(1)(i).valid && isMem(i)
    io.toLsDq.req(i).valid  := io.fromRename(1)(i).valid && isMem(i) &&
                                canEnterDpq && dqCanAccept_tolsDq && (cmoBlockState === s_idle || (cmoBlockState === s_cmoBlock && !previousHasLs(i) && isCmo(i)))
    io.toLsDq.req(i).bits   := updatedUop(1)(i)

    //delete trigger message from frontend
    io.toDq.map(dq => { dq.req(i).bits.trigger := TriggerAction.None })

    XSDebug(io.toIntDq.req(i).valid, p"pc 0x${Hexadecimal(io.toIntDq.req(i).bits.pc)} int index $i\n")
    XSDebug(io.toIntDq1.req(i).valid, p"pc 0x${Hexadecimal(io.toIntDq1.req(i).bits.pc)} int index $i\n")
    XSDebug(io.toVecDq.req(i).valid , p"pc 0x${Hexadecimal(io.toVecDq.req(i).bits.pc )} fp  index $i\n")
    XSDebug(io.toLsDq.req(i).valid , p"pc 0x${Hexadecimal(io.toLsDq.req(i).bits.pc )} ls  index $i\n")
  }

  val enq_dq0_cnt = io.toIntDq.needAlloc.zip(io.toIntDq.req.map(_.valid)).map{ case(needAlloc, valid) => needAlloc && valid}
  XSPerfAccumulate("enq_dq0_cnt", PopCount(enq_dq0_cnt))
  val enq_dq0_alu_cnt = enq_dq0_cnt.zip(isAlu).map{ case (enq,isalu) => enq && isalu }
  XSPerfAccumulate("enq_dq0_alu_cnt", PopCount(enq_dq0_alu_cnt))
  val enq_dq0_brh_cnt = enq_dq0_cnt.zip(isBrh).map { case (enq, isbrh) => enq && isbrh }
  XSPerfAccumulate("enq_dq0_brh_cnt", PopCount(enq_dq0_brh_cnt))

  /**
    * Part 4: send response to rename when dispatch queue accepts the uop
    */
  val hasValidInstr = VecInit(io.fromRename(3).map(_.valid)).asUInt.orR
  val hasSpecialInstr = Cat((0 until RenameWidth).map(i => isBlockBackward(i))).orR

  private val notCmoBlock = previousHasLs.zip(isMemNotCmo).map(x => !x._1 && !x._2)
  private val canAccept = !hasValidInstr || !hasSpecialInstr && io.enqRob.canAccept && dqCanAccept && (cmoBlockState === s_idle || cmoBlockState === s_cmoBlock && notCmoBlock.reduce(_ && _))

  val isWaitForwardOrBlockBackward = isWaitForward.asUInt.orR || isBlockBackward.asUInt.orR
  io.toRenameAllFire := !isWaitForwardOrBlockBackward && io.enqRob.canAccept && dqCanAccept && (cmoBlockState === s_idle || (cmoBlockState === s_cmoBlock && notCmoBlock.reduce(_ && _)))
  for (i <- 0 until RenameWidth) {
    io.fromRename(0)(i).ready := thisCanActualOut(i) && io.enqRob.canAccept && dqCanAccept && (cmoBlockState === s_idle || (cmoBlockState === s_cmoBlock && notCmoBlock(i)))
    io.fromRename(1)(i).ready := thisCanActualOut(i) && io.enqRob.canAccept && dqCanAccept && (cmoBlockState === s_idle || (cmoBlockState === s_cmoBlock && notCmoBlock(i)))
    io.fromRename(2)(i).ready := thisCanActualOut(i) && io.enqRob.canAccept && dqCanAccept && (cmoBlockState === s_idle || (cmoBlockState === s_cmoBlock && notCmoBlock(i)))
    io.fromRename(3)(i).ready := thisCanActualOut(i) && io.enqRob.canAccept && dqCanAccept && (cmoBlockState === s_idle || (cmoBlockState === s_cmoBlock && notCmoBlock(i)))

    io.allocPregs(i).isInt := io.fromRename(3)(i).valid && io.fromRename(3)(i).bits.rfWen && !io.fromRename(3)(i).bits.eliminatedMove
    io.allocPregs(i).isFp := false.B
    io.allocPregs(i).isVec := io.fromRename(3)(i).valid && io.fromRename(3)(i).bits.vecWen
    io.allocPregs(i).isV0 := io.fromRename(3)(i).valid && io.fromRename(3)(i).bits.v0Wen
    io.allocPregs(i).isVl := io.fromRename(3)(i).valid && io.fromRename(3)(i).bits.vlWen
    io.allocPregs(i).preg  := io.fromRename(3)(i).bits.pdest
  }
  io.dispatchIsInBlock := cmoBlockState === s_cmoBlock && !(io.cmoFinish && (blockStartCnt === 0.U) && (blockEndCnt === 0.U))
  val renameFireCnt = PopCount(io.fromRename(3).map(_.fire))
  val enqFireCnt = PopCount(io.toIntDq.req.map(_.valid && io.toIntDq.canAccept)) +
    PopCount(io.toVecDq.req.map(_.valid && io.toVecDq.canAccept)) +
    PopCount(io.toLsDq.req.map(_.valid && io.toLsDq.canAccept))
  dontTouch(renameFireCnt)
  dontTouch(enqFireCnt)
  XSError(enqFireCnt > renameFireCnt, "enqFireCnt should not be greater than renameFireCnt\n")

  val stall_rob = hasValidInstr && !io.enqRob.canAccept && dqCanAccept && (cmoBlockState === s_idle || cmoBlockState === s_cmoBlock && !hasMem)
  val stall_int_dq = hasValidInstr && io.enqRob.canAccept && !toIntDqCanAccept && io.toVecDq.canAccept && io.toLsDq.canAccept && (cmoBlockState === s_idle || cmoBlockState === s_cmoBlock && !hasMem)
  val stall_int_dq0 = hasValidInstr && io.enqRob.canAccept && !io.toIntDq.canAccept && io.toVecDq.canAccept && io.toLsDq.canAccept && (cmoBlockState === s_idle || cmoBlockState === s_cmoBlock && !hasMem)
  val stall_fp_dq = hasValidInstr && io.enqRob.canAccept && toIntDqCanAccept && !io.toVecDq.canAccept && io.toLsDq.canAccept && (cmoBlockState === s_idle || cmoBlockState === s_cmoBlock && !hasMem)
  val stall_ls_dq = hasValidInstr && io.enqRob.canAccept && toIntDqCanAccept && io.toVecDq.canAccept && !io.toLsDq.canAccept && (cmoBlockState === s_idle || cmoBlockState === s_cmoBlock && !hasMem)

  XSPerfAccumulate("in_valid_count", PopCount(io.fromRename(3).map(_.valid)))
  XSPerfAccumulate("in_fire_count", PopCount(io.fromRename(3).map(_.fire)))
  XSPerfAccumulate("in_valid_not_ready_count", PopCount(io.fromRename(3).map(x => x.valid && !x.ready)))
  XSPerfAccumulate("wait_cycle", !io.fromRename(3).head.valid && allResourceReady)

  XSPerfAccumulate("stall_cycle_rob", stall_rob)
  XSPerfAccumulate("stall_cycle_int_dq0", stall_int_dq0)
  XSPerfAccumulate("stall_cycle_fp_dq", stall_fp_dq)
  XSPerfAccumulate("stall_cycle_ls_dq", stall_ls_dq)

  val notIssue = !io.debugTopDown.fromRob.robHeadLsIssue
  val tlbReplay = io.debugTopDown.fromCore.fromMem.robHeadTlbReplay
  val tlbMiss = io.debugTopDown.fromCore.fromMem.robHeadTlbMiss
  val vioReplay = io.debugTopDown.fromCore.fromMem.robHeadLoadVio
  val mshrReplay = io.debugTopDown.fromCore.fromMem.robHeadLoadMSHR
  val l1Miss = io.debugTopDown.fromCore.fromMem.robHeadMissInDCache
  val l2Miss = io.debugTopDown.fromCore.l2MissMatch
  val l3Miss = io.debugTopDown.fromCore.l3MissMatch

  val ldReason = Mux(l3Miss, TopDownCounters.LoadMemStall.id.U,
  Mux(l2Miss, TopDownCounters.LoadL3Stall.id.U,
  Mux(l1Miss, TopDownCounters.LoadL2Stall.id.U,
  Mux(notIssue, TopDownCounters.MemNotReadyStall.id.U,
  Mux(tlbMiss, TopDownCounters.LoadTLBStall.id.U,
  Mux(tlbReplay, TopDownCounters.LoadTLBStall.id.U,
  Mux(mshrReplay, TopDownCounters.LoadMSHRReplayStall.id.U,
  Mux(vioReplay, TopDownCounters.LoadVioReplayStall.id.U,
  TopDownCounters.LoadL1Stall.id.U))))))))

  val decodeReason = RegNextN(io.stallReason.reason, 2)
  val renameReason = RegNext(io.stallReason.reason)

  val stallReason = Wire(chiselTypeOf(io.stallReason.reason))
  val firedVec = io.fromRename(3).map(_.fire)
  io.stallReason.backReason.valid := !canAccept
  io.stallReason.backReason.bits := TopDownCounters.OtherCoreStall.id.U
  stallReason.zip(io.stallReason.reason).zip(firedVec).zipWithIndex.map { case (((update, in), fire), idx) =>
    val headIsInt = FuType.isInt(io.robHead.getDebugFuType)  && io.robHeadNotReady
    val headIsFp  = FuType.isFArith(io.robHead.getDebugFuType)   && io.robHeadNotReady
    val headIsDiv = FuType.isDivSqrt(io.robHead.getDebugFuType) && io.robHeadNotReady
    val headIsLd  = io.robHead.getDebugFuType === FuType.ldu.id.U && io.robHeadNotReady || !io.lqCanAccept
    val headIsSt  = io.robHead.getDebugFuType === FuType.stu.id.U && io.robHeadNotReady || !io.sqCanAccept
    val headIsAmo = io.robHead.getDebugFuType === FuType.mou.id.U && io.robHeadNotReady
    val headIsLs  = headIsLd || headIsSt
    val robLsFull = io.robFull || !io.lqCanAccept || !io.sqCanAccept

    import TopDownCounters._
    update := MuxCase(OtherCoreStall.id.U, Seq(
      // fire
      (fire                                              ) -> NoStall.id.U          ,
      // dispatch not stall / core stall from decode or rename
      (in =/= OtherCoreStall.id.U && in =/= NoStall.id.U ) -> in                    ,
      // dispatch queue stall
      (!toIntDqCanAccept && !headIsInt && !io.robFull) -> IntDqStall.id.U       ,
      (!io.toVecDq.canAccept  && !headIsFp  && !io.robFull) -> FpDqStall.id.U        ,
      (!io.toLsDq.canAccept  && !headIsLs  && !robLsFull ) -> LsDqStall.id.U        ,
      // rob stall
      (headIsAmo                                         ) -> AtomicStall.id.U      ,
      (headIsSt                                          ) -> StoreStall.id.U       ,
      (headIsLd                                          ) -> ldReason              ,
      (headIsDiv                                         ) -> DivStall.id.U         ,
      (headIsInt                                         ) -> IntNotReadyStall.id.U ,
      (headIsFp                                          ) -> FPNotReadyStall.id.U  ,
      (renameReason(idx) =/= NoStall.id.U                ) -> renameReason(idx)     ,
      (decodeReason(idx) =/= NoStall.id.U                ) -> decodeReason(idx)     ,
    ))
  }

  TopDownCounters.values.foreach(ctr => XSPerfAccumulate(ctr.toString(), PopCount(stallReason.map(_ === ctr.id.U))))

  val robTrueCommit = io.debugTopDown.fromRob.robTrueCommit
  TopDownCounters.values.foreach(ctr => XSPerfRolling("td_"+ctr.toString(), PopCount(stallReason.map(_ === ctr.id.U)),
                                                      robTrueCommit, 1000, clock, reset))

  XSPerfHistogram("slots_fire", PopCount(thisActualOut), true.B, 0, RenameWidth+1, 1)
  // Explaination: when out(0) not fire, PopCount(valid) is not meaningfull
  XSPerfHistogram("slots_valid_pure", PopCount(io.enqRob.req.map(_.valid)), thisActualOut(0), 0, RenameWidth+1, 1)
  XSPerfHistogram("slots_valid_rough", PopCount(io.enqRob.req.map(_.valid)), true.B, 0, RenameWidth+1, 1)

  val perfEvents = Seq(
    ("dispatch_in",                 PopCount(io.fromRename(3).map(_.valid & io.fromRename(3)(0).ready))                  ),
    ("dispatch_empty",              !hasValidInstr                                                                 ),
    ("dispatch_utili",              PopCount(io.fromRename(3).map(_.valid))                                           ),
    ("dispatch_waitinstr",          PopCount(io.fromRename(3).map(!_.valid && canAccept))                 ),
    ("dispatch_stall_cycle_rob",    stall_rob                                                                      ),
    ("dispatch_stall_cycle_int_dq", stall_int_dq                                                                   ),
    ("dispatch_stall_cycle_fp_dq",  stall_fp_dq                                                                    ),
    ("dispatch_stall_cycle_ls_dq",  stall_ls_dq                                                                    )
  )
  generatePerfEvent()
}
