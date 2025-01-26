package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import xs.utils.perf.{XSError}
import xs.utils.RegNextN
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.Bundles.{VLmul, VSew, VConfig}
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.fu.vector.{Mgu64, Mgtu, VecInfo, VecPipedFuncUnit}
import xiangshan.ExceptionNO
import yunsuan.{VfaluType, VfpuType}
import yunsuan.vector.VectorFloatAdder
import yunsuan.util._

class VFAlu64(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
	XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VfpuType.dummy, "Vfalu OpType not supported")

	// io alias
	private val opcode  = fuOpType(4,0)
	private val resWiden  = fuOpType(5)
	private val opbWiden  = fuOpType(6)

	// modules
	private val vfalu = Module(new VectorFloatAdder)
	private val mguOpt: Option[Mgu64] = if(cfg.VecNeedSharedMgu) None else Some(Module(new Mgu64(VLEN)))
	private val mgtuOpt: Option[Mgtu] = if(cfg.VecNeedSharedMgu) None else Some(Module(new Mgtu(VLEN)))

	private val resultData = Wire(UInt(64.W))
	private val fflagsData = Wire(UInt(20.W))
	private val srcMaskRShiftForReduction = Wire(UInt(16.W))

	val isScalarMove = (fuOpType === VfaluType.vfmv_f_s) || (fuOpType === VfaluType.vfmv_s_f)
	val srcMaskRShift = Wire(UInt(8.W))
	val maskRshiftWidth = Wire(UInt(6.W))
	maskRshiftWidth := Mux1H(
		Seq(
			(vsew === VSew.e16) -> (vuopIdx(2,0) << 3),
			(vsew === VSew.e32) -> (vuopIdx(2,0) << 2),
			(vsew === VSew.e64) -> (vuopIdx(2,0) << 1),
		)
	)
	srcMaskRShift := (srcMask >> maskRshiftWidth)(7, 0)
	val fp_aIsFpCanonicalNAN = Wire(Bool())
	val fp_bIsFpCanonicalNAN = Wire(Bool())

	// for reduction
  val isFirstGroupUop = vuopIdx === 0.U ||
    (vuopIdx === 1.U && (vlmul === VLmul.m4 || vlmul === VLmul.m8)) ||
    ((vuopIdx === 2.U || vuopIdx === 3.U) && vlmul === VLmul.m8)
  val maskRshiftWidthForReduction = Wire(UInt(6.W))
  maskRshiftWidthForReduction := Mux(fuOpType === VfaluType.vfredosum || fuOpType === VfaluType.vfwredosum,
    vuopIdx,
    Mux1H(Seq(
      (vsew === VSew.e16) -> (vuopIdx(1, 0) << 4),
      (vsew === VSew.e32) -> (vuopIdx(1, 0) << 3),
      (vsew === VSew.e64) -> (vuopIdx(1, 0) << 2),
    ))
  )
  val vlMaskForReduction = (~(Fill(VLEN, 1.U) << vl)).asUInt
  srcMaskRShiftForReduction := ((srcMask & vlMaskForReduction) >> maskRshiftWidthForReduction)(15, 0)
  val existMask = (srcMask & vlMaskForReduction).orR
  val existMaskReg = RegEnable(existMask, io.in.fire)

	def genMaskForReduction(inmask: UInt, sew: UInt, isLo: Bool): UInt = {
    val f64MaskNum = 4
    val f32MaskNum = 8
    val f16MaskNum = 16
    val f64Mask = inmask(f64MaskNum - 1, 0)
    val f32Mask = inmask(f32MaskNum - 1, 0)
    val f16Mask = inmask(f16MaskNum - 1, 0)
    // vs2 reordered, so mask use high bits
    val f64FirstFoldMaskUnorder = Mux1H(
      Seq(
        vecCtrl.fpu.isFoldTo1_2 -> Cat(0.U(3.W), f64Mask(0), 0.U(3.W), f64Mask(1)),
      )
    )
    val f64FirstFoldMaskOrder = Mux1H(
      Seq(
        vecCtrl.fpu.isFoldTo1_2 -> Cat(0.U(3.W), f64Mask(1), 0.U(3.W), f64Mask(0))
      )
    )
    val f32FirstFoldMaskUnorder = Mux1H(
      Seq(
        vecCtrl.fpu.isFoldTo1_2 -> Cat(0.U(2.W), f32Mask(1), f32Mask(0), 0.U(2.W), f32Mask(3), f32Mask(2)),
        vecCtrl.fpu.isFoldTo1_4 -> Cat(0.U(3.W), f32Mask(0), 0.U(3.W), f32Mask(1)),
      )
    )
    val f32FirstFoldMaskOrder = Mux1H(
      Seq(
        vecCtrl.fpu.isFoldTo1_2 -> Cat(0.U(2.W), f32Mask(3), f32Mask(2), 0.U(2.W), f32Mask(1), f32Mask(0)),
        vecCtrl.fpu.isFoldTo1_4 -> Cat(0.U(3.W), f32Mask(1), 0.U(3.W), f32Mask(0)),
      )
    )
    val f16FirstFoldMaskUnorder = Mux1H(
      Seq(
        vecCtrl.fpu.isFoldTo1_2 -> Cat(f16Mask(7,4), f16Mask(3,0)),
        vecCtrl.fpu.isFoldTo1_4 -> Cat(0.U(2.W), f16Mask(1), f16Mask(0), 0.U(2.W), f16Mask(3), f16Mask(2)),
        vecCtrl.fpu.isFoldTo1_8 -> Cat(0.U(3.W), f16Mask(0), 0.U(3.W), f16Mask(1)),
      )
    )
    val f16FirstFoldMaskOrder = Mux1H(
      Seq(
        vecCtrl.fpu.isFoldTo1_2 -> Cat(f16Mask(7,4), f16Mask(3,0)),
        vecCtrl.fpu.isFoldTo1_4 -> Cat(0.U(2.W), f16Mask(3), f16Mask(2), 0.U(2.W), f16Mask(1), f16Mask(0)),
        vecCtrl.fpu.isFoldTo1_8 -> Cat(0.U(3.W), f16Mask(1), 0.U(3.W), f16Mask(0)),
      )
    )
    val f64FoldMask = Mux1H(
      Seq(
        vecCtrl.fpu.isFoldTo1_2 -> "b00010001".U,
      )
    )
    val f32FoldMask = Mux1H(
      Seq(
        vecCtrl.fpu.isFoldTo1_2 -> "b00110011".U,
        vecCtrl.fpu.isFoldTo1_4 -> "b00010001".U,
      )
    )
    val f16FoldMask = Mux1H(
      Seq(
        vecCtrl.fpu.isFoldTo1_2 -> "b11111111".U,
        vecCtrl.fpu.isFoldTo1_4 -> "b00110011".U,
        vecCtrl.fpu.isFoldTo1_8 -> "b00010001".U,
      )
    )
    // low 4 bits for vs2(fp_a), high 4 bits for vs1(fp_b),
    val isFold = vecCtrl.fpu.isFoldTo1_2 || vecCtrl.fpu.isFoldTo1_4 || vecCtrl.fpu.isFoldTo1_8
    val f64FirstNotFoldMask = Cat(0.U(3.W), Mux(isLo, f64Mask(2), f64Mask(3)), 0.U(3.W), Mux(isLo, f64Mask(0), f64Mask(1)))
    val f32FirstNotFoldMask = Cat(0.U(2.W), Mux(isLo, f32Mask(5, 4), f32Mask(7, 6)), 0.U(2.W), Mux(isLo, f32Mask(1, 0), f32Mask(3, 2)))
    val f16FirstNotFoldMask = Cat(Mux(isLo, f16Mask(11, 8), f16Mask(15, 12)), f16Mask(3, 0), f16Mask(7, 4))
    val f64MaskI = Mux(fuOpType === VfaluType.vfredosum || fuOpType === VfaluType.vfwredosum,
      Mux(isFold, f64FirstFoldMaskOrder, f64FirstNotFoldMask),
      Mux(isFirstGroupUop,
        Mux(isFold, f64FirstFoldMaskUnorder, f64FirstNotFoldMask),
        Mux(isFold, f64FoldMask, Fill(8, 1.U))))
    val f32MaskI = Mux(fuOpType === VfaluType.vfredosum || fuOpType === VfaluType.vfwredosum,
      Mux(isFold, f32FirstFoldMaskOrder, f32FirstNotFoldMask),
      Mux(isFirstGroupUop,
        Mux(isFold, f32FirstFoldMaskUnorder, f32FirstNotFoldMask),
        Mux(isFold, f32FoldMask, Fill(8, 1.U))))
    val f16MaskI = Mux(fuOpType === VfaluType.vfredosum || fuOpType === VfaluType.vfwredosum,
      Mux(isFold, f16FirstFoldMaskOrder, f16FirstNotFoldMask),
      Mux(isFirstGroupUop,
        Mux(isFold, f16FirstFoldMaskUnorder, f16FirstNotFoldMask),
        Mux(isFold, f16FoldMask, Fill(8, 1.U))))
    val outMask = Mux1H(
      Seq(
        (sew === 3.U) -> f64MaskI,
        (sew === 2.U) -> f32MaskI,
        (sew === 1.U) -> f16MaskI,
      )
    )
    Mux(fuOpType === VfaluType.vfredosum || fuOpType === VfaluType.vfwredosum, outMask(0),outMask)
  }

	val inIsFold = Wire(UInt(3.W))
	inIsFold := Cat(vecCtrl.fpu.isFoldTo1_8, vecCtrl.fpu.isFoldTo1_4, vecCtrl.fpu.isFoldTo1_2)

	val isLo = inCtrl.vfWenL.getOrElse(false.B) || inCtrl.v0WenL.getOrElse(false.B) || vecCtrl.fpu.isFpToVecInst
	val writeHigh = io.in.bits.ctrl.vfWenH.getOrElse(false.B) || io.in.bits.ctrl.v0WenH.getOrElse(false.B)

	def genMaskForMerge(inmask: UInt, sew: UInt, isLo: Bool): UInt = {
		val f64Mask = Mux(isLo, inmask(0), inmask(1))
		val f32Mask = Mux(isLo, inmask(1, 0), inmask(3, 2))
		val f16Mask = Mux(isLo, inmask(3, 0), inmask(7, 4))
		val f64MaskI = Cat(0.U(3.W), f64Mask)
		val f32MaskI = Cat(0.U(2.W), f32Mask)
		val f16MaskI = f16Mask
		val outMask = Mux1H(
			Seq(
				(sew === 3.U) -> f64MaskI,
				(sew === 2.U) -> f32MaskI,
				(sew === 1.U) -> f16MaskI,
			)
		)
		outMask
	}

	val is_vfwredosum = inCtrl.fuOpType === VfaluType.vfwredosum
	vfalu.io.fire             := io.in.valid
	vfalu.io.fp_a             := Mux(is_vfwredosum, Mux(writeHigh, vs2(127, 64), vs2(63, 0)), vs2(63, 0))
	vfalu.io.fp_b             := Mux(is_vfwredosum, Mux(writeHigh, vs1(127, 64), vs1(63, 0)), vs1(63, 0))
	vfalu.io.widen_a          := Mux(is_vfwredosum, Mux(writeHigh, vs2(127, 96)##vs2(63, 32), vs2(95, 64)##vs2(31, 0)) ,vs2(63, 0))
	vfalu.io.widen_b          := Mux(is_vfwredosum, Mux(writeHigh, vs1(127, 96)##vs1(63, 32), vs1(95, 64)##vs1(31, 0)), vs1(63, 0))
	vfalu.io.frs1             := 0.U     // already vf -> vv
	vfalu.io.is_frs1          := false.B // already vf -> vv
	vfalu.io.mask             := Mux(isScalarMove, !vuopIdx.orR, genMaskForMerge(inmask = srcMaskRShift, sew = vsew, isLo = isLo))
	vfalu.io.maskForReduction := genMaskForReduction(inmask = srcMaskRShiftForReduction, sew = vsew, isLo = isLo)
	vfalu.io.uop_idx          := vuopIdx(0)
	vfalu.io.is_vec           := !vecCtrl.fpu.isFpToVecInst
	vfalu.io.round_mode       := rm
	vfalu.io.fp_format        := Mux(resWiden, vsew + 1.U, vsew)
	vfalu.io.opb_widening     := opbWiden
	vfalu.io.res_widening     := resWiden
	vfalu.io.op_code          := opcode
	vfalu.io.is_vfwredosum    := fuOpType === VfaluType.vfwredosum
	vfalu.io.is_fold          := inIsFold
	vfalu.io.vs2_fold         := vs2      // for better timing
	resultData           			:= vfalu.io.fp_result
	fflagsData           			:= vfalu.io.fflags
	fp_aIsFpCanonicalNAN := vecCtrl.fpu.isFpToVecInst & (
		((vsew === VSew.e32) & ~(vs2(63, 32).andR)) |
			((vsew === VSew.e16) & ~(vs2(63, 48).andR))
		)
	fp_bIsFpCanonicalNAN := vecCtrl.fpu.isFpToVecInst & (
		((vsew === VSew.e32) & ~(vs1(63, 32).andR)) |
			((vsew === VSew.e16) & ~(vs1(63, 48).andR))
		)
	vfalu.io.fp_aIsFpCanonicalNAN := fp_aIsFpCanonicalNAN
	vfalu.io.fp_bIsFpCanonicalNAN := fp_bIsFpCanonicalNAN
	val outVuopidx = outVecCtrl.vuopIdx(2, 0)
	val numOfUopVFRED = Wire(UInt(4.W))
	val numofUopVFREDReg = RegEnable(numOfUopVFRED, io.in.fire)
	val vs1Reg = RegEnable(vs1, io.in.fire)
	val outIsVfRedUnordered = outCtrl.fuOpType === VfaluType.vfredusum ||
		outCtrl.fuOpType === VfaluType.vfredmax ||
		outCtrl.fuOpType === VfaluType.vfredmin
	val outIsVfRedUnComp = outCtrl.fuOpType === VfaluType.vfredmax ||
		outCtrl.fuOpType === VfaluType.vfredmin
	val outIsVfRedUnSum = outCtrl.fuOpType === VfaluType.vfredusum
	val outIsVfRedOrdered = outCtrl.fuOpType === VfaluType.vfredosum ||
		outCtrl.fuOpType === VfaluType.vfwredosum

	val isLastUopRed = outIsVfRedUnordered && outLastUop
	val resultDataUInt = Mux(isLastUopRed && !existMaskReg, vs1Reg, resultData.asUInt)
	val cmpResultWidth = 4
	val cmpResult = Wire(Vec(cmpResultWidth, Bool()))
	for (i <- 0 until cmpResultWidth) {
		if(i == 0) {
			cmpResult(i) := resultDataUInt(0)
		}
		else if(i < 1) {
			cmpResult(i) := Mux1H(
				Seq(
					(outVecCtrl.vsew === 1.U) -> resultDataUInt(i*16),
					(outVecCtrl.vsew === 2.U) -> resultDataUInt(i*32),
					(outVecCtrl.vsew === 3.U) -> resultDataUInt(i*64)
				)
			)
		}
		else if(i < 2) {
			cmpResult(i) := Mux1H(
				Seq(
					(outVecCtrl.vsew === 1.U) -> resultDataUInt(i * 16),
					(outVecCtrl.vsew === 2.U) -> resultDataUInt(i * 32),
					(outVecCtrl.vsew === 3.U) -> false.B
				)
			)
		}
		else if(i <  4) {
			cmpResult(i) := Mux(outVecCtrl.vsew === 1.U, resultDataUInt(i*16), false.B)
		}
	}
	val outCtrl_s0 = ctrlVec.head
	val outVecCtrl_s0 = ctrlVec.head.vpu.get
	val outEew_s0 = Mux(resWiden, outVecCtrl_s0.vsew + 1.U, outVecCtrl_s0.vsew)
	val outWiden = RegEnable(resWiden, io.in.fire)
	val outNarrow = outVecCtrl.isNarrow
	val outEew = Mux(outWiden, outVecCtrl.vsew + 1.U, outVecCtrl.vsew)
	val vlMax_s0 = ((VLEN/8).U >> outEew_s0).asUInt
	val vlMax = ((VLEN/8).U >> outEew).asUInt
	val outVlmulFix = Mux(outWiden, outVecCtrl.vlmul + 1.U, outVecCtrl.vlmul)
	val lmulAbs = Mux(outVlmulFix(2), (~outVlmulFix(1,0)).asUInt + 1.U, outVlmulFix(1,0))
	//  vfmv_f_s need vl=1, reduction last uop need vl=1, other uop need vl=vlmax
	numOfUopVFRED := {
		// addTime include add frs1
		val addTime = MuxLookup(outVecCtrl_s0.vlmul, 1.U(4.W))(Seq(
			VLmul.m2 -> 2.U,
			VLmul.m4 -> 4.U,
			VLmul.m8 -> 8.U,
		))
		val foldLastVlmul = MuxLookup(outVecCtrl_s0.vsew, "b000".U)(Seq(
			VSew.e16 -> VLmul.mf8,
			VSew.e32 -> VLmul.mf4,
			VSew.e64 -> VLmul.mf2,
		))
		// lmul < 1, foldTime = vlmul - foldFastVlmul
		// lmul >= 1, foldTime = 0.U - foldFastVlmul
		val foldTime = Mux(outVecCtrl_s0.vlmul(2), outVecCtrl_s0.vlmul, 0.U) - foldLastVlmul
		addTime + foldTime
	}
	val reductionVl = Mux((outVecCtrl_s0.vuopIdx ===  numOfUopVFRED - 1.U) || (outCtrl_s0.fuOpType === VfaluType.vfredosum || outCtrl_s0.fuOpType === VfaluType.vfwredosum), 1.U, vlMax_s0)
	val outIsResuction = outCtrl.fuOpType === VfaluType.vfredusum ||
		outCtrl.fuOpType === VfaluType.vfredmax ||
		outCtrl.fuOpType === VfaluType.vfredmin ||
		outCtrl.fuOpType === VfaluType.vfredosum ||
		outCtrl.fuOpType === VfaluType.vfwredosum
	val outIsResuction_s0 = outCtrl_s0.fuOpType === VfaluType.vfredusum ||
		outCtrl_s0.fuOpType === VfaluType.vfredmax ||
		outCtrl_s0.fuOpType === VfaluType.vfredmin ||
		outCtrl_s0.fuOpType === VfaluType.vfredosum ||
		outCtrl_s0.fuOpType === VfaluType.vfwredosum
	val outVConfig_s0  = if(!cfg.vconfigWakeUp) outVecCtrl_s0.vconfig else dataVec.head.getSrcVConfig.asTypeOf(new VConfig)
	val outVl_s0       = outVConfig_s0.vl
	val outVlFix_s0 = Mux(
		outVecCtrl_s0.fpu.isFpToVecInst || (outCtrl_s0.fuOpType === VfaluType.vfmv_f_s),
		1.U,
		Mux(
			outCtrl_s0.fuOpType === VfaluType.vfmv_s_f,
			outVl_s0.orR,
			Mux(outIsResuction_s0, reductionVl, outVl_s0)
		)
	)
	val outVlFix = RegEnable(outVlFix_s0, io.in.fire)

	val vlMaxAllUop = Wire(outVl.cloneType)
	vlMaxAllUop := Mux(outVecCtrl.vlmul(2), vlMax >> lmulAbs, vlMax << lmulAbs).asUInt
	val vlMaxThisUop = Mux(outVecCtrl.vlmul(2), vlMax >> lmulAbs, vlMax).asUInt
	val vlSetThisUop = Mux(outVlFix > outVuopidx*vlMaxThisUop, outVlFix - outVuopidx*vlMaxThisUop, 0.U)
	val vlThisUop = Wire(UInt(3.W))
	vlThisUop := Mux(vlSetThisUop < vlMaxThisUop, vlSetThisUop, vlMaxThisUop)
	val vlMaskRShift = Wire(UInt(8.W))
	vlMaskRShift := Fill(8, 1.U(1.W)) >> (8.U - vlThisUop)

	val outIsFisrtGroup = outVuopidx === 0.U ||
		(outVuopidx === 1.U && (outVlmul === VLmul.m4 || outVlmul === VLmul.m8)) ||
		((outVuopidx === 2.U || outVuopidx === 3.U) && outVlmul === VLmul.m8)
	val firstNeedFFlags = outIsFisrtGroup  && outIsVfRedUnComp
	val lastNeedFFlags = outVecCtrl.lastUop && outIsVfRedUnComp
	private val needNoMask = outCtrl.fuOpType === VfaluType.vfmerge ||
		outCtrl.fuOpType === VfaluType.vfmv_s_f ||
		outIsResuction ||
		outVecCtrl.fpu.isFpToVecInst
	val maskToMgu = Mux(needNoMask, allMaskTrue, outSrcMask)
	val allFFlagsEn = Wire(Vec(4, Bool()))
	val outSrcMaskRShift = Wire(UInt(8.W))
	outSrcMaskRShift := (maskToMgu >> (outVecCtrl.vuopIdx(2,0) * vlMax))(7, 0)

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

	def genMaskForRedFFlag(sew: UInt): UInt = {
    val default = "b11111111".U
    val f64FoldMask = Mux(outVecCtrl.fpu.isFoldTo1_2, "b00000001".U, default)
    val f32Fold = outVecCtrl.fpu.isFoldTo1_2 || outVecCtrl.fpu.isFoldTo1_4
    val f32FoldMask = Mux1H(
      Seq(
        outVecCtrl.fpu.isFoldTo1_2 -> "b00000011".U,
        outVecCtrl.fpu.isFoldTo1_4 -> "b00000001".U,
      )
    )
    val f16Fold = outVecCtrl.fpu.isFoldTo1_2 || outVecCtrl.fpu.isFoldTo1_4 || outVecCtrl.fpu.isFoldTo1_8
    val f16FoldMask = Mux1H(
      Seq(
        outVecCtrl.fpu.isFoldTo1_2 -> "b00001111".U,
        outVecCtrl.fpu.isFoldTo1_4 -> "b00000011".U,
        outVecCtrl.fpu.isFoldTo1_8 -> "b00000001".U,
      )
    )
    Mux1H(
      Seq(
        (sew === 3.U) -> f64FoldMask,
        (sew === 2.U) -> Mux(f32Fold, f32FoldMask, default),
        (sew === 1.U) -> Mux(f16Fold, f16FoldMask, default),
      )
    )
  }

	val writeHigh_s1 = RegEnable(writeHigh, io.in.fire)
	val f16VlMaskEn = Mux(writeHigh_s1, vlMaskRShift(7, 4), vlMaskRShift(3, 0))
  val f32VlMaskEn = Wire(UInt(4.W))
  val f64VlMaskEn = Wire(UInt(4.W))
	f32VlMaskEn := Cat(Fill(2, 0.U), Mux(writeHigh_s1, vlMaskRShift(3, 2), vlMaskRShift(1, 0)))
	f64VlMaskEn := Cat(Fill(3, 0.U), Mux(writeHigh_s1, vlMaskRShift(1), vlMaskRShift(0)))
	val vlMaskEn = Mux1H(
		Seq(
			(outEew === 1.U) -> f16VlMaskEn.asUInt,
			(outEew === 2.U) -> f32VlMaskEn.asUInt,
			(outEew === 3.U) -> f64VlMaskEn.asUInt
		)
  )

	fflagsEn := RegNextN(VecInit(eleMask.asBools.zipWithIndex.map{case(m, i) => m & (eNumEffectIdx > eStart + eStartPositionInVreg + i.U) }), cfg.latency.latencyVal.get).zip(vlMaskEn.asBools).map(x => x._1 & x._2)

	val maskForRed = genMaskForRedFFlag(outVecCtrl.vsew)
	allFFlagsEn := (Mux(outIsResuction, Cat(Fill(7, firstNeedFFlags || outIsVfRedUnSum) & maskForRed(7, 1).asUInt,
    lastNeedFFlags || firstNeedFFlags || outIsVfRedOrdered || outIsVfRedUnSum), fflagsEn.asUInt & vlMaskEn) >> Mux(writeHigh_s1, 4.U, 0.U)).asTypeOf(allFFlagsEn)

	val allFFlags = fflagsData.asTypeOf(Vec(4, UInt(5.W)))
	val outFFlags = allFFlagsEn.zip(allFFlags).map{
		case(en,fflags) => Mux(en, fflags, 0.U(5.W))
	}.reduce(_ | _)

	if (backendParams.debugEn) {
		dontTouch(allFFlagsEn)
	}

	val cmpResultOldVd = Wire(UInt(cmpResultWidth.W))
	val cmpResultOldVdRshiftWidth = Wire(UInt(6.W))
	cmpResultOldVdRshiftWidth := Mux1H(
		Seq(
			(outVecCtrl.vsew === VSew.e16) -> (outVecCtrl.vuopIdx(2, 0) << 3),
			(outVecCtrl.vsew === VSew.e32) -> (outVecCtrl.vuopIdx(2, 0) << 2),
			(outVecCtrl.vsew === VSew.e64) -> (outVecCtrl.vuopIdx(2, 0) << 1),
		)
	)
	cmpResultOldVd := (outOldVd >> cmpResultOldVdRshiftWidth)(3, 0)
	val cmpResultForMgu = Wire(Vec(cmpResultWidth, Bool()))
	private val maxVdIdx = 8
	private val elementsInOneUop = Mux1H(
		Seq(
			(outEew === 1.U) -> (cmpResultWidth).U(4.W),
			(outEew === 2.U) -> (cmpResultWidth / 2).U(4.W),
			(outEew === 3.U) -> (cmpResultWidth / 4).U(4.W),
		)
	)
	private val vdIdx = outVecCtrl.vuopIdx(2, 0)
	private val elementsComputed = Mux1H(Seq.tabulate(maxVdIdx)(i => (vdIdx === i.U) -> (elementsInOneUop * i.U)))
	for (i <- 0 until cmpResultWidth) {
		val cmpResultWithVmask = Mux(outSrcMaskRShift(i), cmpResult(i), Mux(outVecCtrl.vma, true.B, cmpResultOldVd(i)))
		cmpResultForMgu(i) := Mux(elementsComputed +& i.U >= outVl, true.B, cmpResultWithVmask)
	}
	val outIsFold = outVecCtrl.fpu.isFoldTo1_2 || outVecCtrl.fpu.isFoldTo1_4 || outVecCtrl.fpu.isFoldTo1_8
	val outOldVdForREDO = Mux1H(Seq(
		(outVecCtrl.vsew === VSew.e16) -> (outOldVd >> 16),
		(outVecCtrl.vsew === VSew.e32) -> (outOldVd >> 32),
		(outVecCtrl.vsew === VSew.e64) -> (outOldVd >> 64),
	))
	val outOldVdForWREDO = Mux(
		!outIsFold,
		Mux(outVecCtrl.vsew === VSew.e16, Cat(outOldVd(VLEN-1-16,16), 0.U(32.W)), Cat(outOldVd(VLEN-1-32,32), 0.U(64.W))),
		Mux(outVecCtrl.vsew === VSew.e16,
			// Divide vuopIdx by 8 and the remainder is 1
			Mux(outVecCtrl.vuopIdx(2,0) === 1.U, outOldVd, outOldVd >> 16),
			// Divide vuopIdx by 4 and the remainder is 1
			Mux(outVecCtrl.vuopIdx(1,0) === 1.U, outOldVd, outOldVd >> 32)
		),
	)
	val outOldVdForRED = Mux(outCtrl.fuOpType === VfaluType.vfredosum, outOldVdForREDO, outOldVdForWREDO)
	val numOfUopVFREDOSUM = {
		val uvlMax = MuxLookup(outVecCtrl.vsew, 0.U)(Seq(
			VSew.e16 -> 8.U,
			VSew.e32 -> 4.U,
			VSew.e64 -> 2.U,
		))
		val vlMax = Mux(outVecCtrl.vlmul(2), uvlMax >> (-outVecCtrl.vlmul)(1, 0), uvlMax << outVecCtrl.vlmul(1, 0)).asUInt
		vlMax
	}
	val isLastUopForREDO = outVecCtrl.lastUop
	val isOutOldVdForREDO = ((outCtrl.fuOpType === VfaluType.vfredosum && outIsFold) || outCtrl.fuOpType === VfaluType.vfwredosum) && !isLastUopForREDO
	val taIsFalseForVFREDO = ((outCtrl.fuOpType === VfaluType.vfredosum) || (outCtrl.fuOpType === VfaluType.vfwredosum)) && (outVecCtrl.vuopIdx =/= numOfUopVFREDOSUM - 1.U)
	// outVecCtrl.fpu.isFpToVecInst means the instruction is float instruction, not vector float instruction
	val notUseVl = outVecCtrl.fpu.isFpToVecInst || (outCtrl.fuOpType === VfaluType.vfmv_f_s)
	val notModifyVd = !notUseVl && (outVl === 0.U)
	io.out.bits.res.fflags.get := Mux(notModifyVd, 0.U(5.W), outFFlags)
	mguOpt match {
		case Some(mgu) =>{
			mgu.io.in.vd := Mux(outVecCtrl.isDstMask, Cat(0.U((VLEN / 16 * 15).W), cmpResultForMgu.asUInt), resultDataUInt)
			mgu.io.in.oldVd := Mux(isOutOldVdForREDO, outOldVdForRED, outOldVd)
			mgu.io.in.mask := maskToMgu
			mgu.io.in.info.ta := Mux(outCtrl.fuOpType === VfaluType.vfmv_f_s, true.B , Mux(taIsFalseForVFREDO, false.B, outVecCtrl.vta))
			mgu.io.in.info.ma := Mux(outCtrl.fuOpType === VfaluType.vfmv_s_f, true.B , outVecCtrl.vma)
			mgu.io.in.info.vl := outVlFix
			mgu.io.in.info.vstart := outVecCtrl.vstart
			mgu.io.in.info.vlmul := outVecCtrl.vlmul
			mgu.io.in.info.valid := Mux(notModifyVd, false.B, io.in.valid)
			mgu.io.in.info.vstart := Mux(outVecCtrl.fpu.isFpToVecInst, 0.U, outVecCtrl.vstart)
			mgu.io.in.info.eew :=  RegEnable(outEew_s0,io.in.fire)
			mgu.io.in.info.vsew := outVecCtrl.vsew
			mgu.io.in.info.vdIdx := RegEnable(Mux(outIsResuction_s0, 0.U, outVecCtrl_s0.vuopIdx), io.in.fire)
			mgu.io.in.info.narrow := outVecCtrl.isNarrow
			mgu.io.in.info.dstMask := outVecCtrl.isDstMask
			mgu.io.in.isIndexedVls := false.B
			mgu.io.in.isLo := (outCtrl.vfWenL.getOrElse(false.B) || outCtrl.v0WenL.getOrElse(false.B)) || vecCtrl.fpu.isFpToVecInst
			mgtuOpt.get.io.in.vd := Mux(outVecCtrl.isDstMask, mgu.io.out.vd, resultDataUInt)
			mgtuOpt.get.io.in.vl := outVl
			val resultFpMask = Wire(UInt(VLEN.W))
			val isFclass = outVecCtrl.fpu.isFpToVecInst && (outCtrl.fuOpType === VfaluType.vfclass)
			val fpCmpFuOpType = Seq(VfaluType.vfeq, VfaluType.vflt, VfaluType.vfle)
			val isCmp = outVecCtrl.fpu.isFpToVecInst && (fpCmpFuOpType.map(_ === outCtrl.fuOpType).reduce(_|_))
			resultFpMask := Mux(isFclass || isCmp, Fill(16, 1.U(1.W)), Fill(VLEN, 1.U(1.W)))
			// when dest is mask, the result need to be masked by mgtu
			io.out.bits.res.data := Mux(notModifyVd, outOldVd,
				Mux(outVecCtrl.isDstMask, mgtuOpt.get.io.out.vd, mgu.io.out.vd) & resultFpMask)
			io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr) := mgu.io.out.illegal
		}
		case None =>{
			io.out.bits.res.data := Mux(notModifyVd, outOldVd,
				Mux(outVecCtrl.isDstMask, Cat(0.U((VLEN / 16 * 15).W), cmpResultForMgu.asUInt),
					resultDataUInt))
			io.mguEew.foreach(x => x:= outEew)
      io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr) := false.B
      io.out.bits.ctrl.vpu.foreach(_.vmask := maskToMgu)
      io.out.bits.ctrl.vpu.foreach(_.veew := outEew)
      io.out.bits.ctrl.vpu.foreach(_.vl := outVlFix)
			io.out.bits.ctrl.vpu.foreach(_.vta := Mux(outCtrl.fuOpType === VfaluType.vfmv_f_s, true.B , Mux(taIsFalseForVFREDO, false.B, outVecCtrl.vta)))
			io.out.bits.ctrl.vpu.foreach(_.vma := Mux(outCtrl.fuOpType === VfaluType.vfmv_s_f, true.B , outVecCtrl.vma))
			io.out.bits.ctrl.vpu.foreach(_.is_reduction := RegEnable(outIsResuction_s0, io.in.fire))
			io.out.bits.ctrl.vpu.foreach(_.is_vfredosum := outCtrl.fuOpType === VfaluType.vfredosum || outCtrl.fuOpType === VfaluType.vfwredosum)
			io.out.bits.ctrl.vpu.foreach(_.is_fold := outIsFold)
		}
	}
}
