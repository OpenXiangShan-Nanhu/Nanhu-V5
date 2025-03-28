package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import xs.utils.perf.{XSError}
import xs.utils.RegNextN
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.fu.vector.{Mgu, VecPipedFuncUnit, Mgu64}
import xiangshan.ExceptionNO
import yunsuan.VfpuType
import yunsuan.VfmaType
import yunsuan.vector.VectorFloatFMA
import yunsuan.util._


class VFMA64(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
	XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VfpuType.dummy, "Vfalu OpType not supported")
	// io alias
	private val opcode  = fuOpType(3,0)
	private val resWiden  = fuOpType(4)

	// modules
	private val vfma = Module(new VectorFloatFMA)
    private val mguOpt: Option[Mgu64] = if(cfg.VecNeedSharedMgu) None else Some(Module(new Mgu64(VLEN)))

	private val resultData = Wire(UInt(64.W))
	private val fflagsData = Wire(UInt(20.W))
	val fp_aIsFpCanonicalNAN = Wire(Bool())
	val fp_bIsFpCanonicalNAN = Wire(Bool())
	val fp_cIsFpCanonicalNAN = Wire(Bool())
	vfma.io.fire         := io.in.valid
	vfma.io.fp_a         := vs2(63, 0)
	vfma.io.fp_b         := vs1(63, 0)
	vfma.io.fp_c         := oldVd(63, 0)
	vfma.io.widen_a      := vs2 //TODO
	vfma.io.widen_b      := vs1 //TODO
	vfma.io.frs1         := 0.U     // already vf -> vv
	vfma.io.is_frs1      := false.B // already vf -> vv
	vfma.io.uop_idx      := vuopIdx(0)
	vfma.io.is_vec       := !vecCtrl.fpu.isFpToVecInst
	vfma.io.round_mode   := rm
	vfma.io.fp_format    := Mux(resWiden, vsew + 1.U, vsew)
	vfma.io.res_widening := resWiden
	vfma.io.op_code      := opcode
	resultData := vfma.io.fp_result
	fflagsData := vfma.io.fflags
	fp_aIsFpCanonicalNAN := vecCtrl.fpu.isFpToVecInst & (
		((vsew === VSew.e32) & (!vs2(63, 32).andR)) |
			((vsew === VSew.e16) & (!vs2(63, 16).andR))
		)
	fp_bIsFpCanonicalNAN := vecCtrl.fpu.isFpToVecInst & (
		((vsew === VSew.e32) & (!vs1(63, 32).andR)) |
			((vsew === VSew.e16) & (!vs1(63, 16).andR))
		)
	fp_cIsFpCanonicalNAN := !(opcode === VfmaType.vfmul) & vecCtrl.fpu.isFpToVecInst & (
		((vsew === VSew.e32) & (!oldVd(63, 32).andR)) |
			((vsew === VSew.e16) & (!oldVd(63, 16).andR))
		)
	vfma.io.fp_aIsFpCanonicalNAN := fp_aIsFpCanonicalNAN
	vfma.io.fp_bIsFpCanonicalNAN := fp_bIsFpCanonicalNAN
	vfma.io.fp_cIsFpCanonicalNAN := fp_cIsFpCanonicalNAN

	val outFuOpType = outCtrl.fuOpType
	val outWiden = outCtrl.fuOpType(4)
	val outNarrow = outVecCtrl.isNarrow
	val outEew = Mux(outWiden, outVecCtrl.vsew + 1.U, outVecCtrl.vsew)
	val outVuopidx = outVecCtrl.vuopIdx(2, 0)
	val needNoMask = outVecCtrl.fpu.isFpToVecInst
	val maskToMgu = Mux(needNoMask, allMaskTrue, outSrcMask)
	
	/** fflags: */
	val inWiden = inCtrl.fuOpType(4)
	val inNarrow = vecCtrl.isNarrow
  val eNum1H = chisel3.util.experimental.decode.decoder(vsew ## (inWiden || inNarrow),
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
  val eNum1HEffect = Mux(inWiden || inNarrow, eNum1H << 1, eNum1H)
  when(io.in.valid) {
    assert(!eNum1H(0).asBool,"fp128 is forbidden now")
  }
  // calculate eNum in the condition whether lmul is negetive or positive.
  val eNumMax1H = Mux(vlmul.head(1).asBool, eNum1HEffect >> ((~vlmul.tail(1)).asUInt + 1.U), eNum1HEffect << vlmul.tail(1)).asUInt(6, 0)
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
  fflagsEn := RegNextN(VecInit(eleMask.asBools.zipWithIndex.map{case(m, i) => m & (eNumEffectIdx > eStart + eStartPositionInVreg + i.U) }), cfg.latency.latencyVal.get)
  val fflagsAll = Wire(Vec(4, UInt(5.W)))
  fflagsAll := vfma.io.fflags.asTypeOf(fflagsAll)
  val fflags = fflagsEn.zip(fflagsAll).map{case(en, fflag) => Mux(en, fflag, 0.U(5.W))}.reduce(_ | _)
  io.out.bits.res.fflags.get := Mux(outCtrl.vpu.get.fpu.isFpToVecInst, fflagsAll(0), fflags)

  val resultDataUInt = resultData.asUInt
  mguOpt match {
		case Some(mgu) => {
			mgu.io.in.vd := resultDataUInt
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
			mgu.io.in.info.narrow := outVecCtrl.isNarrow
			mgu.io.in.info.dstMask := outVecCtrl.isDstMask
			mgu.io.in.isIndexedVls := false.B
			mgu.io.in.isLo := (outCtrl.vfWenL.getOrElse(false.B) || outCtrl.v0WenL.getOrElse(false.B)) || vecCtrl.fpu.isFpToVecInst
			io.out.bits.res.data := mgu.io.out.vd
		}
		case None => { 
			io.out.bits.res.data := resultDataUInt
			io.mguEew.foreach(x => x:= outEew)
			io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr) := false.B
			io.out.bits.ctrl.vpu.foreach(_.vmask := maskToMgu)
			io.out.bits.ctrl.vpu.foreach(_.veew := Mux(outVecCtrl.fpu.isFpToVecInst, VSew.e64, outEew))
			io.out.bits.ctrl.vpu.foreach(_.vl := Mux(outVecCtrl.fpu.isFpToVecInst, 2.U, outVl))
			io.out.bits.ctrl.vpu.foreach(_.vstart := Mux(outVecCtrl.fpu.isFpToVecInst, 0.U, outVecCtrl.vstart))
		}
  }
  io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr) := false.B //mgu.io.out.illegal


}
