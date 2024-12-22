package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xs.utils.perf.{XSError}
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.fu.vector.{Mgu, VecPipedFuncUnit}
import xiangshan.ExceptionNO
import yunsuan.VfpuType
import yunsuan.VfmaType
import yunsuan.vector.VectorFloatFMA

class VFMA64(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
	XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VfpuType.dummy, "Vfalu OpType not supported")
	// io alias
	private val opcode  = fuOpType(3,0)
	private val resWiden  = fuOpType(4)

	// modules
	private val vfma = Module(new VectorFloatFMA)

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
	vfma.io.is_vec       := true.B // Todo
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
	val outEew = Mux(outWiden, outVecCtrl.vsew + 1.U, outVecCtrl.vsew)
	val outVuopidx = outVecCtrl.vuopIdx(2, 0)
	val vlMax = ((VLEN / 8).U >> outEew).asUInt
	val outVlmulFix = Mux(outWiden, outVecCtrl.vlmul + 1.U, outVecCtrl.vlmul)
	val lmulAbs = Mux(outVlmulFix(2), (~outVlmulFix(1, 0)).asUInt + 1.U, outVlmulFix(1, 0))
	val outVlFix = Mux(outVecCtrl.fpu.isFpToVecInst, 1.U, outVl)
	val vlMaxAllUop = Wire(outVl.cloneType)
	vlMaxAllUop := Mux(outVecCtrl.vlmul(2), vlMax >> lmulAbs, vlMax << lmulAbs).asUInt
	val vlMaxThisUop = Mux(outVecCtrl.vlmul(2), vlMax >> lmulAbs, vlMax).asUInt
	val vlSetThisUop = Mux(outVlFix > outVuopidx * vlMaxThisUop, outVlFix - outVuopidx * vlMaxThisUop, 0.U)
	val vlThisUop = Wire(UInt(3.W))
	vlThisUop := Mux(vlSetThisUop < vlMaxThisUop, vlSetThisUop, vlMaxThisUop)
	val vlMaskRShift = Wire(UInt(4.W))
	vlMaskRShift := Fill(4, 1.U(1.W)) >> 4.U - vlThisUop

	private val needNoMask = outVecCtrl.fpu.isFpToVecInst
	val maskToMgu = Mux(needNoMask, allMaskTrue, outSrcMask)
	val allFFlagsEn = Wire(Vec(4, Bool()))
	val outSrcMaskRShift = Wire(UInt(4.W))
	outSrcMaskRShift := (maskToMgu >> (outVecCtrl.vuopIdx(2, 0) * vlMax))(3, 0)
	val f16FFlagsEn = outSrcMaskRShift
	val f32FFlagsEn = Wire(UInt(4.W))
	val f64FFlagsEn = Wire(UInt(4.W))
	val f16VlMaskEn = vlMaskRShift
	val f32VlMaskEn = Wire(UInt(4.W))
	val f64VlMaskEn = Wire(UInt(4.W))
	f32FFlagsEn := Cat(Fill(2, 0.U), outSrcMaskRShift(1, 0))
	f64FFlagsEn := Cat(Fill(3, 0.U), outSrcMaskRShift(0))
	f32VlMaskEn := Cat(Fill(2, 0.U), vlMaskRShift(1, 0))
	f64VlMaskEn := Cat(Fill(3, 0.U), vlMaskRShift(0))
	val fflagsEn = Mux1H(
		Seq(
			(outEew === 1.U) -> f16FFlagsEn.asUInt,
			(outEew === 2.U) -> f32FFlagsEn.asUInt,
			(outEew === 3.U) -> f64FFlagsEn.asUInt
		)
	)
	val vlMaskEn = Mux1H(
		Seq(
			(outEew === 1.U) -> f16VlMaskEn.asUInt,
			(outEew === 2.U) -> f32VlMaskEn.asUInt,
			(outEew === 3.U) -> f64VlMaskEn.asUInt
		)
	)
	allFFlagsEn := (fflagsEn & vlMaskEn).asTypeOf(allFFlagsEn)

	val allFFlags = fflagsData.asTypeOf(Vec(4, UInt(5.W)))
	val outFFlags = allFFlagsEn.zip(allFFlags).map {
		case (en, fflags) => Mux(en, fflags, 0.U(5.W))
	}.reduce(_ | _)
	io.out.bits.res.fflags.get := outFFlags

//	val resultDataUInt = resultData.asUInt
//	mgu.io.in.vd := resultDataUInt
//	mgu.io.in.oldVd := outOldVd
//	mgu.io.in.mask := maskToMgu
//	mgu.io.in.info.ta := outVecCtrl.vta
//	mgu.io.in.info.ma := outVecCtrl.vma
//	mgu.io.in.info.vl := Mux(outVecCtrl.fpu.isFpToVecInst, 1.U, outVl)
//	mgu.io.in.info.vlmul := outVecCtrl.vlmul
//	mgu.io.in.info.valid := io.out.valid
//	mgu.io.in.info.vstart := Mux(outVecCtrl.fpu.isFpToVecInst, 0.U, outVecCtrl.vstart)
//	mgu.io.in.info.eew := outEew
//	mgu.io.in.info.vsew := outVecCtrl.vsew
//	mgu.io.in.info.vdIdx := outVecCtrl.vuopIdx
//	mgu.io.in.info.narrow := outVecCtrl.isNarrow
//	mgu.io.in.info.dstMask := outVecCtrl.isDstMask
//	mgu.io.in.isIndexedVls := false.B
	io.out.bits.res.data := resultData.asUInt //mgu.io.out.vd
	io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr) := false.B //mgu.io.out.illegal
}
