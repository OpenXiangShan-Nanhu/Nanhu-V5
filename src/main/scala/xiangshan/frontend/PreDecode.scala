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

package xiangshan.frontend

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.rocket.{RVCDecoder, ExpandedInstruction}
import chisel3.{util, _}
import chisel3.util._
import utils._
import xs.utils._
import xs.utils.perf._
import xiangshan._
import xiangshan.frontend.icache._
import xiangshan.backend.decode.isa.predecode.PreDecodeInst
import xiangshan.backend.fu.NewCSR.TriggerUtil
import java.lang.reflect.Parameter
import xiangshan.backend.fu.util.SdtrigExt

trait HasPdConst extends HasXSParameter with HasICacheParameters with HasIFUConst{
  def isRVC(inst: UInt) = (inst(1,0) =/= 3.U)
  def isLink(reg:UInt) = reg === 1.U || reg === 5.U
  def brInfo(instr: UInt) = {
    val brType::Nil = ListLookup(instr, List(BrType.notCFI), PreDecodeInst.brTable)
    val rd = Mux(isRVC(instr), instr(12), instr(11,7))
    val rs = Mux(isRVC(instr), Mux(brType === BrType.jal, 0.U, instr(11, 7)), instr(19, 15))
    val isCall = (brType === BrType.jal && !isRVC(instr) || brType === BrType.jalr) && isLink(rd) // Only for RV64
    val isRet = brType === BrType.jalr && isLink(rs) && !isCall
    List(brType, isCall, isRet)
  }
  def jal_offset(inst: UInt, rvc: Bool): UInt = {
    val rvc_offset = Cat(inst(12), inst(8), inst(10, 9), inst(6), inst(7), inst(2), inst(11), inst(5, 3), 0.U(1.W))
    val rvi_offset = Cat(inst(31), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W))
    val max_width = rvi_offset.getWidth
    SignExt(Mux(rvc, SignExt(rvc_offset, max_width), SignExt(rvi_offset, max_width)), jalOffsetWidth)
  }
  def br_offset(inst: UInt, rvc: Bool): UInt = {
    val rvc_offset = Cat(inst(12), inst(6, 5), inst(2), inst(11, 10), inst(4, 3), 0.U(1.W))
    val rvi_offset = Cat(inst(31), inst(7), inst(30, 25), inst(11, 8), 0.U(1.W))
    val max_width = rvi_offset.getWidth
    SignExt(Mux(rvc, SignExt(rvc_offset, max_width), SignExt(rvi_offset, max_width)), jalOffsetWidth)
  }

  def NOP = "h4501".U(16.W)
}

object BrType {
  def notCFI   = "b00".U
  def branch  = "b01".U
  def jal     = "b10".U
  def jalr    = "b11".U
  def apply() = UInt(2.W)
}

object ExcType {  //TODO:add exctype
  def notExc = "b000".U
  def apply() = UInt(3.W)
}

class PreDecodeInfo extends Bundle {  // 8 bit
  val valid   = Bool()
  val isRVC   = Bool()
  val brType  = UInt(2.W)
  val isCall  = Bool()
  val isRet   = Bool()
  //val excType = UInt(3.W)
  def isBr    = brType === BrType.branch
  def isJal   = brType === BrType.jal
  def isJalr  = brType === BrType.jalr
  def notCFI  = brType === BrType.notCFI
}

class PreDecodeResp(implicit p: Parameters) extends XSBundle with HasPdConst {
  val pd = Vec(PredictWidth, new PreDecodeInfo)
  val hasHalfValid = Vec(PredictWidth, Bool())
  //val expInstr = Vec(PredictWidth, UInt(32.W))
  val instr      = Vec(PredictWidth, UInt(32.W))
  val jumpOffset = Vec(PredictWidth, UInt(jalOffsetWidth.W))
//  val hasLastHalf = Bool()
  val triggered    = Vec(PredictWidth, TriggerAction())
}

class PreDecode(implicit p: Parameters) extends XSModule with HasPdConst{
  val io = IO(new Bundle() {
    val in = Input(ValidIO(new IfuToPreDecode))
    val out = Output(new PreDecodeResp)
  })

  val data          = io.in.bits.data
//  val lastHalfMatch = io.in.lastHalfMatch
  val validStart, validEnd = Wire(Vec(PredictWidth, Bool()))
  val h_validStart, h_validEnd = Wire(Vec(PredictWidth, Bool()))

  val validStart_half, validEnd_half = Wire(Vec(PredictWidth, Bool()))
  val h_validStart_half, h_validEnd_half = Wire(Vec(PredictWidth, Bool()))

  val validStart_halfPlus1, validEnd_halfPlus1 = Wire(Vec(PredictWidth, Bool()))
  val h_validStart_halfPlus1, h_validEnd_halfPlus1 = Wire(Vec(PredictWidth, Bool()))

  val validStart_diff, validEnd_diff = Wire(Vec(PredictWidth, Bool()))
  val h_validStart_diff, h_validEnd_diff = Wire(Vec(PredictWidth, Bool()))

  val currentIsRVC = Wire(Vec(PredictWidth, Bool()))

  validStart_half.map(_ := false.B)
  validEnd_half.map(_ := false.B)
  h_validStart_half.map(_ := false.B)
  h_validEnd_half.map(_ := false.B)

  validStart_halfPlus1.map(_ := false.B)
  validEnd_halfPlus1.map(_ := false.B)
  h_validStart_halfPlus1.map(_ := false.B)
  h_validEnd_halfPlus1.map(_ := false.B)

  val rawInsts = if (HasCExtension) VecInit((0 until PredictWidth).map(i => Cat(data(i+1), data(i))))
  else         VecInit((0 until PredictWidth).map(i => data(i)))

  for (i <- 0 until PredictWidth) {
    val inst           = WireInit(rawInsts(i))
    //val expander       = Module(new RVCExpander)
    currentIsRVC(i)   := isRVC(inst)
    val currentPC      = io.in.bits.pc(i)
    //expander.io.in             := inst

    val brType::isCall::isRet::Nil = brInfo(inst)
    val jalOffset = jal_offset(inst, currentIsRVC(i))
    val brOffset  = br_offset(inst, currentIsRVC(i))

    io.out.hasHalfValid(i)        := h_validStart(i)

    io.out.triggered(i)   := DontCare//VecInit(Seq.fill(10)(false.B))


    io.out.pd(i).valid         := validStart(i)
    io.out.pd(i).isRVC         := currentIsRVC(i)

    // for diff purpose only
    io.out.pd(i).brType        := brType
    io.out.pd(i).isCall        := isCall
    io.out.pd(i).isRet         := isRet

    //io.out.expInstr(i)         := expander.io.out.bits
    io.out.instr(i)              :=inst
    io.out.jumpOffset(i)       := Mux(io.out.pd(i).isBr, brOffset, jalOffset)
  }

  // the first half is always reliable
  for (i <- 0 until PredictWidth / 2) {
    val lastIsValidEnd =   if (i == 0) { true.B } else { validEnd(i-1) || !HasCExtension.B }
    validStart(i)   := (lastIsValidEnd || !HasCExtension.B)
    validEnd(i)     := validStart(i) && currentIsRVC(i) || !validStart(i) || !HasCExtension.B

    //prepared for last half match
    val h_lastIsValidEnd = if (i == 0) { false.B } else { h_validEnd(i-1) || !HasCExtension.B }
    h_validStart(i)   := (h_lastIsValidEnd || !HasCExtension.B)
    h_validEnd(i)     := h_validStart(i) && currentIsRVC(i) || !h_validStart(i) || !HasCExtension.B
  }

  for (i <- 0 until PredictWidth) {
    val lastIsValidEnd =   if (i == 0) { true.B } else { validEnd_diff(i-1) || !HasCExtension.B }
    validStart_diff(i)   := (lastIsValidEnd || !HasCExtension.B)
    validEnd_diff(i)     := validStart_diff(i) && currentIsRVC(i) || !validStart_diff(i) || !HasCExtension.B

    //prepared for last half match
    val h_lastIsValidEnd = if (i == 0) { false.B } else { h_validEnd_diff(i-1) || !HasCExtension.B }
    h_validStart_diff(i)   := (h_lastIsValidEnd || !HasCExtension.B)
    h_validEnd_diff(i)     := h_validStart_diff(i) && currentIsRVC(i) || !h_validStart_diff(i) || !HasCExtension.B
  }

  // assume PredictWidth / 2 is a valid start
  for (i <- PredictWidth / 2 until PredictWidth) {
    val lastIsValidEnd =   if (i == PredictWidth / 2) { true.B } else { validEnd_half(i-1) || !HasCExtension.B }
    validStart_half(i)   := (lastIsValidEnd || !HasCExtension.B)
    validEnd_half(i)     := validStart_half(i) && currentIsRVC(i) || !validStart_half(i) || !HasCExtension.B

    //prepared for last half match
    val h_lastIsValidEnd = if (i == PredictWidth / 2) { true.B } else { h_validEnd_half(i-1) || !HasCExtension.B }
    h_validStart_half(i)   := (h_lastIsValidEnd || !HasCExtension.B)
    h_validEnd_half(i)     := h_validStart_half(i) && currentIsRVC(i) || !h_validStart_half(i) || !HasCExtension.B
  }

  // assume PredictWidth / 2 + 1 is a valid start (and PredictWidth / 2 is last half of RVI)
  for (i <- PredictWidth / 2 + 1 until PredictWidth) {
    val lastIsValidEnd =   if (i == PredictWidth / 2 + 1) { true.B } else { validEnd_halfPlus1(i-1) || !HasCExtension.B }
    validStart_halfPlus1(i)   := (lastIsValidEnd || !HasCExtension.B)
    validEnd_halfPlus1(i)     := validStart_halfPlus1(i) && currentIsRVC(i) || !validStart_halfPlus1(i) || !HasCExtension.B

    //prepared for last half match
    val h_lastIsValidEnd = if (i == PredictWidth / 2 + 1) { true.B } else { h_validEnd_halfPlus1(i-1) || !HasCExtension.B }
    h_validStart_halfPlus1(i)   := (h_lastIsValidEnd || !HasCExtension.B)
    h_validEnd_halfPlus1(i)     := h_validStart_halfPlus1(i) && currentIsRVC(i) || !h_validStart_halfPlus1(i) || !HasCExtension.B
  }
  validStart_halfPlus1(PredictWidth / 2) := false.B // could be true but when true we select half, not halfPlus1
  validEnd_halfPlus1(PredictWidth / 2) := true.B

  // assume h_PredictWidth / 2 is an end
  h_validStart_halfPlus1(PredictWidth / 2) := false.B // could be true but when true we select half, not halfPlus1
  h_validEnd_halfPlus1(PredictWidth / 2) := true.B

  // if PredictWidth / 2 - 1 is a valid end, PredictWidth / 2 is a valid start
  for (i <- PredictWidth / 2 until PredictWidth) {
    validStart(i) := Mux(validEnd(PredictWidth / 2 - 1), validStart_half(i), validStart_halfPlus1(i))
    validEnd(i) := Mux(validEnd(PredictWidth / 2 - 1), validEnd_half(i), validEnd_halfPlus1(i))
    h_validStart(i) := Mux(h_validEnd(PredictWidth / 2 - 1), h_validStart_half(i), h_validStart_halfPlus1(i))
    h_validEnd(i) := Mux(h_validEnd(PredictWidth / 2 - 1), h_validEnd_half(i), h_validEnd_halfPlus1(i))
  }

  val validStartMismatch = Wire(Bool())
  val validEndMismatch = Wire(Bool())
  val validH_ValidStartMismatch = Wire(Bool())
  val validH_ValidEndMismatch = Wire(Bool())

  validStartMismatch := validStart.zip(validStart_diff).map{case(a,b) => a =/= b}.reduce(_||_)
  validEndMismatch := validEnd.zip(validEnd_diff).map{case(a,b) => a =/= b}.reduce(_||_)
  validH_ValidStartMismatch := h_validStart.zip(h_validStart_diff).map{case(a,b) => a =/= b}.reduce(_||_)
  validH_ValidEndMismatch := h_validEnd.zip(h_validEnd_diff).map{case(a,b) => a =/= b}.reduce(_||_)

  XSError(io.in.valid && validStartMismatch, p"validStart mismatch\n")
  XSError(io.in.valid && validEndMismatch, p"validEnd mismatch\n")
  XSError(io.in.valid && validH_ValidStartMismatch, p"h_validStart mismatch\n")
  XSError(io.in.valid && validH_ValidEndMismatch, p"h_validEnd mismatch\n")

//  io.out.hasLastHalf := !io.out.pd(PredictWidth - 1).isRVC && io.out.pd(PredictWidth - 1).valid

  for (i <- 0 until PredictWidth) {
    XSDebug(true.B,
      p"instr ${Hexadecimal(io.out.instr(i))}, " +
        p"validStart ${Binary(validStart(i))}, " +
        p"validEnd ${Binary(validEnd(i))}, " +
        p"isRVC ${Binary(io.out.pd(i).isRVC)}, " +
        p"brType ${Binary(io.out.pd(i).brType)}, " +
        p"isRet ${Binary(io.out.pd(i).isRet)}, " +
        p"isCall ${Binary(io.out.pd(i).isCall)}\n"
    )
  }
}

class IfuToF3PreDecode(implicit p: Parameters) extends XSBundle with HasPdConst {
  val instr      = Vec(PredictWidth, UInt(32.W))
}

class F3PreDecodeResp(implicit p: Parameters) extends XSBundle with HasPdConst {
  val pd = Vec(PredictWidth, new PreDecodeInfo)
}
class F3Predecoder(implicit p: Parameters) extends XSModule with HasPdConst {
  val io = IO(new Bundle() {
    val in = Input(new IfuToF3PreDecode)
    val out = Output(new F3PreDecodeResp)
  })
  io.out.pd.zipWithIndex.map{ case (pd,i) =>
    pd.valid := DontCare
    pd.isRVC := DontCare
    pd.brType := brInfo(io.in.instr(i))(0)
    pd.isCall := brInfo(io.in.instr(i))(1)
    pd.isRet := brInfo(io.in.instr(i))(2)
  }

}

class RVCExpander(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val in      = Input(UInt(32.W))
    val fsIsOff = Input(Bool())
    val out     = Output(new ExpandedInstruction)
    val ill     = Output(Bool())
  })

  val decoder = new RVCDecoder(io.in, io.fsIsOff, XLEN, fLen, useAddiForMv = true)

  if (HasCExtension) {
    io.out := decoder.decode
    io.ill := decoder.ill
  } else {
    io.out := decoder.passthrough
    io.ill := false.B
  }
}

/* ---------------------------------------------------------------------
 * Predict result check
 *
 * ---------------------------------------------------------------------
 */

object FaultType {
  def noFault      = "b000".U
  def jalFault     = "b001".U    //not CFI taken or invalid instruction taken
  def retFault     = "b010".U    //not CFI taken or invalid instruction taken
  def targetFault  = "b011".U
  def notCFIFault  = "b100".U    //not CFI taken or invalid instruction taken
  def invalidTaken = "b101".U
  def jalrFault    = "b110".U
  def apply() = UInt(3.W)
}

class CheckInfo extends Bundle {  // 8 bit
  val value  = UInt(3.W)
  def isjalFault        = value === FaultType.jalFault
  def isjalrFault       = value === FaultType.jalrFault
  def isRetFault        = value === FaultType.retFault
  def istargetFault     = value === FaultType.targetFault
  def invalidTakenFault = value === FaultType.invalidTaken
  def notCFIFault       = value === FaultType.notCFIFault
}

class PredCheckerResp(implicit p: Parameters) extends XSBundle with HasPdConst {
  //to Ibuffer write port  (stage 1)
  val stage1Out = new Bundle{
    val fixedRange  = Vec(PredictWidth, Bool())
    val fixedTaken  = Vec(PredictWidth, Bool())
  }
  //to Ftq write back port (stage 2)
  val stage2Out = new Bundle{
    val fixedTarget = Vec(PredictWidth, UInt(VAddrBits.W))
    val jalOffset = Vec(PredictWidth, UInt(jalOffsetWidth.W))
    val fixedMissPred = Vec(PredictWidth,  Bool())
    val faultType   = Vec(PredictWidth, new CheckInfo)
  }
}


class PredChecker(implicit p: Parameters) extends XSModule with HasPdConst {
  val io = IO( new Bundle{
    val in = Input(new IfuToPredChecker)
    val out = Output(new PredCheckerResp)
  })

  val (takenIdx, predTaken)     = (io.in.ftqOffset.bits, io.in.ftqOffset.valid)
  val predTarget                = (io.in.target)
  val (instrRange, instrValid)  = (io.in.instrRange, io.in.instrValid)
  val (pds, pc, jumpOffset)     = (io.in.pds, io.in.pc, io.in.jumpOffset)

  val jalFaultVec, jalrFaultVec, retFaultVec, targetFault, notCFITaken, invalidTaken = Wire(Vec(PredictWidth, Bool()))

  /** remask fault may appear together with other faults, but other faults are exclusive
    * so other f ault mast use fixed mask to keep only one fault would be found and redirect to Ftq
    * we first detecct remask fault and then use fixedRange to do second check
    **/

  //Stage 1: detect remask fault
  /** first check: remask Fault */
  jalFaultVec  := VecInit(pds.zipWithIndex.map { case (pd, i) =>
    pd.isJal && instrRange(i) && instrValid(i) && (takenIdx > i.U && predTaken || !predTaken)
  })
  jalrFaultVec := VecInit(pds.zipWithIndex.map { case (pd, i) =>
    pd.isJalr && !pd.isRet && instrRange(i) && instrValid(i) && (takenIdx > i.U && predTaken || !predTaken)
  })
  retFaultVec := VecInit(pds.zipWithIndex.map { case (pd, i) =>
    pd.isRet && instrRange(i) && instrValid(i) && (takenIdx > i.U && predTaken || !predTaken)
  })
  val remaskFault = VecInit((0 until PredictWidth).map(i => jalFaultVec(i) || jalrFaultVec(i) || retFaultVec(i)))
  val remaskIdx   = ParallelPriorityEncoder(remaskFault.asUInt)
  val needRemask  = ParallelOR(remaskFault)
  val fixedRange  = instrRange.asUInt & (Fill(PredictWidth, !needRemask) | Fill(PredictWidth, 1.U(1.W)) >> ~remaskIdx)

  io.out.stage1Out.fixedRange := fixedRange.asTypeOf((Vec(PredictWidth, Bool())))

  io.out.stage1Out.fixedTaken := VecInit(pds.zipWithIndex.map{ case(pd, i) =>
    instrValid (i) && fixedRange(i) && (pd.isRet || pd.isJal || pd.isJalr || takenIdx === i.U && predTaken && !pd.notCFI)
  })

  /** second check: faulse prediction fault and target fault */
  notCFITaken  := VecInit(pds.zipWithIndex.map{case(pd, i) => fixedRange(i) && instrValid(i) && i.U === takenIdx && pd.notCFI && predTaken })
  invalidTaken := VecInit(pds.zipWithIndex.map{case(pd, i) => fixedRange(i) && !instrValid(i)  && i.U === takenIdx  && predTaken })

  val jumpTargets          = VecInit(pds.zipWithIndex.map{case(pd,i) => (pc(i) + SignExt(jumpOffset(i), VAddrBits)).asTypeOf(UInt(VAddrBits.W))})
  val seqTargets = VecInit((0 until PredictWidth).map(i => pc(i) + Mux(pds(i).isRVC || !instrValid(i), 2.U, 4.U ) ))

  //Stage 2: detect target fault
  /** target calculation: in the next stage  */
  val fixedRangeNext = RegEnable(fixedRange, io.in.fire_in)
  val instrValidNext = RegEnable(instrValid, io.in.fire_in)
  val takenIdxNext   = RegEnable(takenIdx, io.in.fire_in)
  val predTakenNext  = RegEnable(predTaken, io.in.fire_in)
  val predTargetNext = RegEnable(predTarget, io.in.fire_in)
  val targetDiffNext = jumpTargets.map( t => RegEnable(predTarget =/= t, io.in.fire_in) )
  val jumpTargetsNext = RegEnable(jumpTargets, io.in.fire_in)
  val jumpOffsetNext = RegEnable(jumpOffset, io.in.fire_in)
  val seqTargetsNext = RegEnable(seqTargets, io.in.fire_in)
  val pdsNext = RegEnable(pds, io.in.fire_in)
  val jalFaultVecNext = RegEnable(jalFaultVec, io.in.fire_in)
  val jalrFaultVecNext = RegEnable(jalrFaultVec, io.in.fire_in)
  val retFaultVecNext = RegEnable(retFaultVec, io.in.fire_in)
  val notCFITakenNext = RegEnable(notCFITaken, io.in.fire_in)
  val invalidTakenNext = RegEnable(invalidTaken, io.in.fire_in)

  // targetFault      := VecInit(pdsNext.zipWithIndex.map{case(pd,i) => fixedRangeNext(i) && instrValidNext(i) && (pd.isJal || pd.isBr) && takenIdxNext === i.U && predTakenNext  && (predTargetNext =/= jumpTargetsNext(i))})
  targetFault      := VecInit(pdsNext.zipWithIndex.map{case(pd,i) => fixedRangeNext(i) && instrValidNext(i) && (pd.isJal || pd.isBr) && takenIdxNext === i.U && predTakenNext  && targetDiffNext(i)})
  when(RegNext(io.in.fire_in)){
    targetDiffNext.zipWithIndex.foreach{ case (diff, i) =>
      XSError(diff =/= (predTargetNext =/= jumpTargetsNext(i)), "predecoder retiming error")
    }
  }

  io.out.stage2Out.faultType.zipWithIndex.foreach { case (faultType, i) =>
    faultType.value := MuxCase(FaultType.noFault, Seq(
      jalFaultVecNext(i) -> FaultType.jalFault,
      jalrFaultVecNext(i) -> FaultType.jalrFault,
      retFaultVecNext(i) -> FaultType.retFault,
      targetFault(i) -> FaultType.targetFault,
      notCFITakenNext(i) -> FaultType.notCFIFault,
      invalidTakenNext(i) -> FaultType.invalidTaken
    ))
  }

  io.out.stage2Out.fixedMissPred.zipWithIndex.foreach { case(missPred, i ) =>
    missPred := jalFaultVecNext(i) || jalrFaultVecNext(i) || retFaultVecNext(i) || notCFITakenNext(i) ||
      invalidTakenNext(i) || targetFault(i)
  }
  io.out.stage2Out.fixedTarget.zipWithIndex.foreach { case(target, i) =>
    target := Mux(jalFaultVecNext(i) || targetFault(i), jumpTargetsNext(i), seqTargetsNext(i))
  }
  io.out.stage2Out.jalOffset.zipWithIndex.foreach { case(offset, i) =>
    offset := jumpOffsetNext(i)
  }
}

class FrontendTrigger(implicit p: Parameters) extends XSModule with SdtrigExt {
  val io = IO(new Bundle(){
    val frontendTrigger = Input(new FrontendTdataDistributeIO)
    val triggered     = Output(Vec(PredictWidth, TriggerAction()))

    val pds           = Input(Vec(PredictWidth, new PreDecodeInfo))
    val pc            = Input(Vec(PredictWidth, UInt(VAddrBits.W)))
    val data          = if(HasCExtension) Input(Vec(PredictWidth + 1, UInt(16.W)))
                        else Input(Vec(PredictWidth, UInt(32.W)))
  })

  val data          = io.data

  val rawInsts = if (HasCExtension) VecInit((0 until PredictWidth).map(i => Cat(data(i+1), data(i))))
  else         VecInit((0 until PredictWidth).map(i => data(i)))

  val tdataVec = RegInit(VecInit(Seq.fill(TriggerNum)(0.U.asTypeOf(new MatchTriggerIO))))
  when(io.frontendTrigger.tUpdate.valid) {
    tdataVec(io.frontendTrigger.tUpdate.bits.addr) := io.frontendTrigger.tUpdate.bits.tdata
  }
  val triggerEnableVec = RegInit(VecInit(Seq.fill(TriggerNum)(false.B))) // From CSR, controlled by priv mode, etc.
  triggerEnableVec := io.frontendTrigger.tEnableVec
  XSDebug(triggerEnableVec.asUInt.orR, "Debug Mode: At least one frontend trigger is enabled\n")

  val triggerTimingVec = VecInit(tdataVec.map(_.timing))
  val triggerChainVec = VecInit(tdataVec.map(_.chain))

  for (i <- 0 until TriggerNum) { PrintTriggerInfo(triggerEnableVec(i), tdataVec(i)) }

  val debugMode = io.frontendTrigger.debugMode
  val triggerCanRaiseBpExp = io.frontendTrigger.triggerCanRaiseBpExp
  //val triggerHitVec = Wire(Vec(PredictWidth, Vec(TriggerNum, Bool())))
  val triggerHitVec = (0 until TriggerNum).map(j =>
      TriggerCmpConsecutive(io.pc, tdataVec(j).tdata2, tdataVec(j).matchType, triggerEnableVec(j)).map(
        hit => hit && !tdataVec(j).select && !debugMode)
  ).transpose

  for (i <- 0 until PredictWidth) {
    val triggerCanFireVec = Wire(Vec(TriggerNum, Bool()))
    TriggerCheckCanFire(TriggerNum, triggerCanFireVec, VecInit(triggerHitVec(i)), triggerTimingVec, triggerChainVec)

    val actionVec = VecInit(tdataVec.map(_.action))
    val triggerAction = Wire(TriggerAction())
    TriggerUtil.triggerActionGen(triggerAction, triggerCanFireVec, actionVec, triggerCanRaiseBpExp)

    // Priority may select last when no trigger fire.
    io.triggered(i) := triggerAction
    XSDebug(triggerCanFireVec.asUInt.orR, p"Debug Mode: Predecode Inst No. ${i} has trigger action vec ${triggerCanFireVec.asUInt.orR}\n")
  }
}
