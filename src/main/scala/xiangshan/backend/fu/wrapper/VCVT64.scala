package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import xs.utils.perf.{XSError}
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.{Mgu64, VecPipedFuncUnit}
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.ExceptionNO
import xiangshan.FuOpType
import yunsuan.VfpuType
import yunsuan.vector.VectorConvert.VectorCvt
import yunsuan.util._


class VCVT64(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VfpuType.dummy, "Vfcvt OpType not supported")

  // params alias
  private val dataWidth = cfg.destDataBits

  // io alias
  private val opcode = fuOpType(8, 0)
  private val sew = vsew

  private val isRtz = opcode(2) & opcode(1)
  private val isRod = opcode(2) & !opcode(1) & opcode(0)
  private val isFrm = !isRtz && !isRod
  private val vfcvtRm = Mux1H(
    Seq(isRtz, isRod, isFrm),
    Seq(1.U, 6.U, rm)
  )

  private val lmul = vlmul // -3->3 => 1/8 ->8

  val widen = opcode(4, 3) // 0->single 1->widen 2->norrow => width of result
  val isSingleCvt = !widen(1) & !widen(0)
  val isWidenCvt = !widen(1) & widen(0)
  val isNarrowCvt = widen(1) & !widen(0)
  val fire = io.in.valid
  val fireReg = GatedValidRegNext(fire)

  // output width 8， 16， 32， 64
  val output1H = Wire(UInt(4.W))
  output1H := chisel3.util.experimental.decode.decoder(
    widen ## sew,
    TruthTable(
      Seq(
        BitPat("b00_01") -> BitPat("b0010"), // 16
        BitPat("b00_10") -> BitPat("b0100"), // 32
        BitPat("b00_11") -> BitPat("b1000"), // 64

        BitPat("b01_00") -> BitPat("b0010"), // 16
        BitPat("b01_01") -> BitPat("b0100"), // 32
        BitPat("b01_10") -> BitPat("b1000"), // 64

        BitPat("b10_00") -> BitPat("b0001"), // 8
        BitPat("b10_01") -> BitPat("b0010"), // 16
        BitPat("b10_10") -> BitPat("b0100"), // 32
      ),
      BitPat.N(4)
    )
  )
  if(backendParams.debugEn) {
    dontTouch(output1H)
  }
  val outputWidth1H = output1H
  val outIs32bits = RegNext(RegNext(outputWidth1H(2)))
  val outIs16bits = RegNext(RegNext(outputWidth1H(1)))
  val outIsInt = !outCtrl.fuOpType(6)

  // May be useful in the future.
  val outIsMvInst = outCtrl.fuOpType === FuOpType.FMVXF

  val outEew = RegEnable(RegEnable(Mux1H(output1H, Seq(0,1,2,3).map(i => i.U)), fire), fireReg)
  private val needNoMask = outVecCtrl.fpu.isFpToVecInst
  val maskToMgu = Mux(needNoMask, allMaskTrue, outSrcMask)

  // modules
  private val vfcvt = Module(new VectorCvt64Top(XLEN))
  private val mguOpt: Option[Mgu64] = if(cfg.VecNeedSharedMgu) None else Some(Module(new Mgu64(VLEN)))
  /**
   * [[vfcvt]]'s in connection
   */
  vfcvt.fire          := fire
  vfcvt.uopIdx        := vuopIdx(0)
  vfcvt.src           := Mux(vecCtrl.fpu.isFpToVecInst, vs1(63, 0), vs2(63, 0))
  vfcvt.opType        := opcode(7,0)
  vfcvt.sew           := sew
  vfcvt.rm            := vfcvtRm
  vfcvt.outputWidth1H := outputWidth1H
  vfcvt.isWiden       := isWidenCvt
  vfcvt.isNarrow      := isNarrowCvt
  vfcvt.isFpToVecInst := vecCtrl.fpu.isFpToVecInst
  vfcvt.isLo          := io.in.bits.ctrl.vfWenL.getOrElse(false.B) ||
                          io.in.bits.ctrl.v0WenL.getOrElse(false.B) ||
                          io.in.bits.ctrl.vpu.get.fpu.isFpToVecInst

  /** fflags:
   */
  val eNum1H = chisel3.util.experimental.decode.decoder(sew ## (isWidenCvt || isNarrowCvt),
    TruthTable(
      Seq(                     // 8, 4, 2, 1
        BitPat("b001") -> BitPat("b1000"), //8
        BitPat("b010") -> BitPat("b1000"), //8
        BitPat("b011") -> BitPat("b0100"), //4
        BitPat("b100") -> BitPat("b0100"), //4
        BitPat("b101") -> BitPat("b0010"), //2
        BitPat("b110") -> BitPat("b0010"), //2
      ),
      BitPat.N(4)
    )
  )
  val eNum1HEffect = Mux(isWidenCvt || isNarrowCvt, eNum1H << 1, eNum1H)
  when(io.in.valid) {
    assert(!eNum1H(0).asBool,"fp128 is forbidden now")
  }
  // calculate eNum in the condition whether lmul is negetive or positive.
  val eNumMax1H = Mux(lmul.head(1).asBool, eNum1HEffect >> ((~lmul.tail(1)).asUInt + 1.U), eNum1HEffect << lmul.tail(1)).asUInt(6, 0)
  val eNumMax = Mux1H(eNumMax1H, Seq(1,2,4,8,16,32,64).map(i => i.U)) //only for cvt intr, don't exist 128 in cvt
  val vlForFflags = Mux(vecCtrl.fpu.isFpToVecInst, 1.U, vl)
  val eNumEffectIdx = Mux(vlForFflags > eNumMax, eNumMax, vlForFflags)

  val writeHigh = io.in.bits.ctrl.vfWenH.getOrElse(false.B) || io.in.bits.ctrl.v0WenH.getOrElse(false.B)
  val eNum = Mux1H(eNum1H, Seq(1, 2, 4, 8).map(num => num.U)) // element Number per Vreg
  val eStart = vuopIdx * eNum
  val maskForFflags = Mux(vecCtrl.fpu.isFpToVecInst, allMaskTrue, srcMask)
  // shift eStart bits and get the mask of current Vreg
  val maskPart = maskForFflags >> eStart
  val eleMask = Mux1H(
    Seq(
      (eNum1H === 1.U) -> Cat(0.U(3.W), maskPart(0)), // don't exist
      (eNum1H === 2.U) -> Cat(0.U(3.W), Mux(writeHigh, maskPart(1), maskPart(0))),
      (eNum1H === 4.U) -> Cat(0.U(2.W), Mux(writeHigh, maskPart(3, 2), maskPart(1, 0))),
      (eNum1H === 8.U) -> Mux(writeHigh, maskPart(7, 4), maskPart(3, 0))
    )
  )
  val fflagsEn = Wire(Vec(4, Bool()))
  val eStartPositionInVreg = Mux(writeHigh, eNum >> 1.U, 0.U)
  fflagsEn := eleMask.asBools.zipWithIndex.map{case(m, i) => m & (eNumEffectIdx > eStart + eStartPositionInVreg + i.U) }

  val fflagsEnCycle2 = RegEnable(RegEnable(fflagsEn, fire), fireReg)
  val fflagsAll = Wire(Vec(4, UInt(5.W)))
  fflagsAll := vfcvt.io.fflags.asTypeOf(fflagsAll)
  val fflags = fflagsEnCycle2.zip(fflagsAll).map{case(en, fflag) => Mux(en, fflag, 0.U(5.W))}.reduce(_ | _)
  io.out.bits.res.fflags.get := Mux(outIsMvInst, 0.U, fflags)

  /**
   * [[mgu]]'s in connection
   */
  val resultDataUInt = Wire(UInt(dataWidth.W))
  resultDataUInt := vfcvt.io.result

  private val narrow = RegEnable(RegEnable(isNarrowCvt, fire), fireReg)
  private val outNarrowVd = Wire(UInt(64.W))
  outNarrowVd := 0.U(32.W) ## resultDataUInt(31, 0)

  mguOpt match {
    case Some(mgu) =>{
      mgu.io.in.vd := Mux(narrow, outNarrowVd, resultDataUInt)
      mgu.io.in.oldVd := outOldVd
      mgu.io.in.mask := maskToMgu
      mgu.io.in.info.ta := outVecCtrl.vta
      mgu.io.in.info.ma := outVecCtrl.vma
      mgu.io.in.info.vl := Mux(outVecCtrl.fpu.isFpToVecInst, 1.U, outVl)
      mgu.io.in.info.vlmul := outVecCtrl.vlmul
      mgu.io.in.info.valid := io.out.valid
      mgu.io.in.info.vstart := Mux(outVecCtrl.fpu.isFpToVecInst, 0.U, outVecCtrl.vstart)
      mgu.io.in.info.eew := outEew
      mgu.io.in.info.vsew := outVecCtrl.vsew
      mgu.io.in.info.vdIdx := outVecCtrl.vuopIdx
      mgu.io.in.info.narrow := narrow
      mgu.io.in.info.dstMask := outVecCtrl.isDstMask
      mgu.io.in.isIndexedVls := false.B
      mgu.io.in.isLo := outCtrl.vfWenL.getOrElse(false.B) || outCtrl.v0WenL.getOrElse(false.B) || vecCtrl.fpu.isFpToVecInst
      // for scalar f2i cvt inst
      val isFp2VecForInt = outVecCtrl.fpu.isFpToVecInst && outIs32bits && outIsInt
      // for f2i mv inst
      val result = Mux(outIsMvInst, RegNext(RegNext(vs2.tail(64))), mgu.io.out.vd)
      io.out.bits.res.data := Mux(isFp2VecForInt,
        Fill(32, result(31)) ## result(31, 0),
        result)
      io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr) := mgu.io.out.illegal
    }
    case None =>{
      // for scalar f2i cvt inst
      val isFp2VecForInt = outVecCtrl.fpu.isFpToVecInst && outIs32bits && outIsInt
      val needBoxedSigle = outVecCtrl.fpu.isFpToVecInst && outIs32bits && !outIsInt
      val needBoxedHalf = outVecCtrl.fpu.isFpToVecInst && outIs16bits && !outIsInt
      // for f2i mv inst
      val result = Mux(outIsMvInst, RegNext(RegNext(vs1.tail(64))), Mux(narrow, outNarrowVd, resultDataUInt))
      when(isFp2VecForInt) {
        io.out.bits.res.data := Fill(32, result(31)) ## result(31, 0)
      }.elsewhen(needBoxedSigle) {
        io.out.bits.res.data := Fill(32, 1.U(1.W)) ## result(31, 0)
      }.elsewhen(needBoxedHalf) {
        io.out.bits.res.data := Fill(48, 1.U(1.W)) ## result(15, 0)
      }.otherwise {
        io.out.bits.res.data := result
      }
      io.mguEew.foreach(x => x:= outEew)
      io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr) := false.B
      io.out.bits.ctrl.vpu.foreach(_ := outVecCtrl)
      io.out.bits.ctrl.vpu.foreach(_.vmask := maskToMgu)
      io.out.bits.ctrl.vpu.foreach(_.veew := Mux(outVecCtrl.fpu.isFpToVecInst, VSew.e64, outEew))
			io.out.bits.ctrl.vpu.foreach(_.vl := Mux(outVecCtrl.fpu.isFpToVecInst, 2.U, outVl))
			io.out.bits.ctrl.vpu.foreach(_.vstart := Mux(outVecCtrl.fpu.isFpToVecInst, 0.U, outVecCtrl.vstart))
    }
  }
}


class VectorCvtTop64IO extends Bundle{
  val fire = Input(Bool())
  val uopIdx = Input(Bool())
  val src = Input(UInt(64.W))
  val opType = Input(UInt(8.W))
  val sew = Input(UInt(2.W))
  val rm = Input(UInt(3.W))
  val outputWidth1H = Input(UInt(4.W))
  val isWiden = Input(Bool())
  val isNarrow = Input(Bool())
  val isFpToVecInst = Input(Bool())
  val isLo = Input(Bool())

  val result = Output(UInt(64.W))
  val fflags = Output(UInt(20.W))
}

//according to uopindex, 1: high64 0:low64
class VectorCvt64Top(xlen: Int) extends Module{
  val io = IO(new VectorCvtTop64IO)

  val (fire, uopIdx, src, opType, sew, rm, outputWidth1H, isWiden, isNarrow, isFpToVecInst, isLo) = (
    io.fire, io.uopIdx, io.src, io.opType, io.sew, io.rm, io.outputWidth1H, io.isWiden, io.isNarrow, io.isFpToVecInst, io.isLo
  )
  val fireReg = GatedValidRegNext(fire)

  val in0 = Mux(isWiden && !isFpToVecInst,
                  Mux(uopIdx, src.head(32), src.tail(32)),
                  src
                )

  val vectorCvt0 = Module(new VectorCvt(xlen))
  vectorCvt0.fire := fire
  vectorCvt0.src := in0
  vectorCvt0.opType := opType
  vectorCvt0.sew := sew
  vectorCvt0.rm := rm
  vectorCvt0.isFpToVecInst := isFpToVecInst
  vectorCvt0.isFround := 0.U
  vectorCvt0.isFcvtmod := false.B

  val isNarrowCycle2 = RegEnable(RegEnable(isNarrow, fire), fireReg)
  val outputWidth1HCycle2 = RegEnable(RegEnable(outputWidth1H, fire), fireReg)

  //cycle2
  io.result := Mux(isNarrowCycle2, vectorCvt0.io.result.tail(32), vectorCvt0.io.result)

  io.fflags := Mux1H(outputWidth1HCycle2, Seq(
    vectorCvt0.io.fflags,
    Mux(isNarrowCycle2, vectorCvt0.io.fflags.tail(10), vectorCvt0.io.fflags),
    Mux(isNarrowCycle2, vectorCvt0.io.fflags(4,0), vectorCvt0.io.fflags.tail(10)),
    vectorCvt0.io.fflags(4,0)
  ))
}
