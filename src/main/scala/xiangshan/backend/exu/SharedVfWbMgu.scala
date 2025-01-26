package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xs.utils._
import xs.utils.perf._
import xiangshan.backend.Bundles.{ExuInput, ExuOutput, MemExuInput, MemExuOutput}
import xiangshan.{AddrTransType, FPUCtrlSignals, HasXSParameter, Redirect, XSBundle, XSModule}
import xiangshan.backend.datapath.WbConfig.{PregWB, _}
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.vector.Bundles.{VType, Vxrm}
import xiangshan.backend.fu.fpu.Bundles.Frm
import xiangshan.backend.fu.wrapper.{CSRInput, CSRToDecode}
import xiangshan.backend.fu.vector.Mgu
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.Bundles.VPUCtrlSignals
import xiangshan.backend.fu.vector.Bundles.VSew
import chisel3.util.experimental.decode.TruthTable
import freechips.rocketchip.util.SeqToAugmentedSeq

class SharedVfWbMgu(params: ExeUnitParams, name: String)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val ins = Vec(2, Flipped(DecoupledIO(new ExuOutput(params))))
    val outs = Vec(2, DecoupledIO(new ExuOutput(params)))
  })
  override val desiredName = name
  val sharedNarrow = io.ins.head.bits.shareVpuCtrl.get.isNarrow
  val resData_hi = io.ins.last.bits.data
  val resData_lo = io.ins.head.bits.data
  io.outs <> io.ins

  val vpuCtrl = io.ins.head.bits.shareVpuCtrl.get
  val is_vfredosum = vpuCtrl.is_vfredosum && !vpuCtrl.isWiden
  val is_vfwredosum = vpuCtrl.is_vfredosum && vpuCtrl.isWiden
  val oldVdData = Cat(io.ins(1).bits.oldVd.getOrElse(0.U)(63, 0), io.ins(0).bits.oldVd.getOrElse(0.U)(63, 0))
  val outOldVdForREDO = Mux1H(Seq(
    (vpuCtrl.vsew === VSew.e16) -> (oldVdData >> 16),
    (vpuCtrl.vsew === VSew.e32) -> (oldVdData >> 32),
    (vpuCtrl.vsew === VSew.e64) -> (oldVdData >> 64),
  ))
  val outOldVdForWREDO = Mux(
    !vpuCtrl.is_fold,
    Mux(vpuCtrl.vsew === VSew.e16, Cat(oldVdData(VLEN-1-16, 16), 0.U(32.W)), Cat(oldVdData(VLEN-1-32, 32), 0.U(64.W))),
    Mux(vpuCtrl.vsew === VSew.e16,
      // Divide vuopIdx by 8 and the remainder is 1
      Mux(vpuCtrl.vuopIdx(2, 0) === 1.U, oldVdData, oldVdData >> 16),
      // Divide vuopIdx by 4 and the remainder is 1
      Mux(vpuCtrl.vuopIdx(1, 0) === 1.U, oldVdData, oldVdData >> 32)
    ),
  )
  val isOutOldVdForREDO = ((is_vfredosum && vpuCtrl.is_fold) || is_vfwredosum) && !vpuCtrl.lastUop
  val outOldVdForRED = Mux(is_vfredosum, outOldVdForREDO, outOldVdForWREDO)
  val wholeOldVd = Cat(io.ins(1).bits.oldVd.getOrElse(0.U)(63, 0), io.ins(0).bits.oldVd.getOrElse(0.U)(63, 0))
  val oldVd = Mux(isOutOldVdForREDO, outOldVdForRED, wholeOldVd)

  def useMgu(vd: UInt, ctrl: VPUCtrlSignals, idx: Int): UInt = {
    val mgu = Module(new Mgu(VLEN))
    val allMaskTrue = VecInit(Seq.fill(VLEN)(true.B)).asUInt
    val mask = Mux(ctrl.fpu.isFpToVecInst, allMaskTrue, ctrl.vmask)
    val vl = Mux(ctrl.fpu.isFpToVecInst, 1.U, ctrl.vl)
    val vstart = Mux(ctrl.fpu.isFpToVecInst, 0.U, ctrl.vstart)
    val notUseVl = ctrl.fpu.isFpToVecInst
    val notModifyVd = !notUseVl && (vl === 0.U)
    mgu.io.in.vd := vd
    mgu.io.in.oldVd := oldVd
    mgu.io.in.mask := mask
    mgu.io.in.info.ta := ctrl.vta
    mgu.io.in.info.ma := ctrl.vma
    mgu.io.in.info.vl := vl
    mgu.io.in.info.vlmul := ctrl.vlmul
    mgu.io.in.info.valid := Mux(notModifyVd, false.B, io.ins(idx).valid)
    mgu.io.in.info.vstart := vstart
    mgu.io.in.info.eew := ctrl.veew
    mgu.io.in.info.vsew := ctrl.vsew
    mgu.io.in.info.vdIdx := Mux(ctrl.is_reduction, 0.U, ctrl.vuopIdx)
    mgu.io.in.info.narrow := ctrl.isNarrow
    mgu.io.in.info.dstMask := ctrl.isDstMask
    mgu.io.in.isIndexedVls := false.B
    mgu.io.out.vd
  }

  def useTailAgnostic(vl: UInt, vd:UInt, destMask:Bool): UInt = {
    val mask = ((1.U << vl) - 1.U)
    Mux(destMask, (vd & mask) | (~mask), vd)
  }


  def cmpResultBitMask(result: UInt): UInt={
    // Computes the masked result based on high and low FU operations
    def getMaskData(result:UInt, mask:UInt, oldVd:UInt, vma:Bool, isHigh:Bool): UInt={
      val RshiftHalfWidth = chisel3.util.experimental.decode.decoder(vpuCtrl.vsew ## isHigh.asUInt,
        TruthTable(
          Seq(
            BitPat("b010") -> BitPat("b000"),// isLow
            BitPat("b100") -> BitPat("b000"),
            BitPat("b110") -> BitPat("b000"),
            BitPat("b011") -> BitPat("b100"),// isHigh, 16
            BitPat("b101") -> BitPat("b010"),// 32
            BitPat("b111") -> BitPat("b001"),// 64
          ),
          BitPat.N(3)
        )
      )
      val RshiftWidth = Wire(UInt(6.W))
      RshiftWidth := Mux1H(
        Seq(
          (vpuCtrl.vsew === VSew.e16) -> (vpuCtrl.vuopIdx(2, 0) << 3),
          (vpuCtrl.vsew === VSew.e32) -> (vpuCtrl.vuopIdx(2, 0) << 2),
          (vpuCtrl.vsew === VSew.e64) -> (vpuCtrl.vuopIdx(2, 0) << 1),
        )
      )
      // Apply bit shift to old value and mask, then compute the masked result
      val cmpOldVd = ((oldVd >> RshiftWidth)(7, 0) >> RshiftHalfWidth)(3, 0)
      val cmpMask  = ((mask  >> RshiftWidth)(7, 0) >> RshiftHalfWidth)(3, 0)
      val cmpResult = result(3,0)
      val cmpMaskedData = Wire(Vec(4, Bool()))
      for (i <- 0 until 4) {
        cmpMaskedData(i) := Mux(cmpMask(i), cmpResult(i), Mux(vma, true.B, cmpOldVd(i)))
      }
      cmpMaskedData.asUInt
    }
    // Get masked results for high and low parts
    val result_H = getMaskData(result(127,64), vpuCtrl.vmask, wholeOldVd, vpuCtrl.vma, true.B)
    val result_L = getMaskData(result(63,0),   vpuCtrl.vmask, wholeOldVd, vpuCtrl.vma, false.B)
    dontTouch(result_H);dontTouch(result_L)

    // Generate the tail mask for 3 different SEW conditions (16, 32, 64)
    val finalMaskedResult = VecInit(Seq.fill(4)(0.U(VLEN.W))) // sew is 16\32\64 three cases
    dontTouch(finalMaskedResult)
    // Consider different SEW(16, 32, 64) has different location to wirte result,
    // use for loop to generate all posible result and use SEW to select correct one
    for (i <- 0 until 3){
      val eew = 1 << (i+4) // eew = 16\32\64
      val elmtNum = (VLEN/eew) // element num = 8\4\2
      val elmtNumHalf = elmtNum/2 // element num = 4\2\1

      // currentUopIdxResult should LeftShift to the right position [uopIdx*elmtNum, (uopIdx+1)*elmtNum]
      val LshiftHalfWidth = vpuCtrl.vuopIdx(2, 0) << (3-i).U // Lshift = uopIdx*8\uopIdx*4\uopIdx*2
      // oldResultMask get the history uopIdx result [0, (uopIdx-1)*elmtNum-1]
      val oldResultMask = Wire(UInt(128.W))
      dontTouch(oldResultMask)
      oldResultMask := ((1.U << LshiftHalfWidth) - 1.U)
      // Only the lastUop should consider VTailAgonistc [vpuCtrl.vl, VLEN]
      val vTailMask = Cat(Seq.fill(VLEN - elmtNum)(1.U(1.W)).asUInt ,Seq.fill(elmtNum)(0.U(1.W)).asUInt) << LshiftHalfWidth
      val result = ZeroExt((Cat(result_H(elmtNumHalf-1,0), result_L(elmtNumHalf-1,0))), VLEN) << LshiftHalfWidth

      // finalResult need piece 3 parts together. finalMaskedResult= Cat(VTailAgnositc, currentUopIdxResult, lastUopIdxResut)
      finalMaskedResult(i) := Mux(vpuCtrl.lastUop ,
             (result|vTailMask)      |(wholeOldVd & oldResultMask),
      result|(wholeOldVd & vTailMask)|(wholeOldVd & oldResultMask))
    }
    // Select final masked result based on vsew (16, 32, 64)
    val finalSel = MuxCase(
        0.U(VLEN.W),
        Seq(
          (vpuCtrl.vsew === VSew.e16) -> finalMaskedResult(0),
          (vpuCtrl.vsew === VSew.e32) -> finalMaskedResult(1),
          (vpuCtrl.vsew === VSew.e64) -> finalMaskedResult(2),
        )
      )
    finalSel
  }


  resData_hi.zip(resData_lo).zipWithIndex.foreach {
    case ((hi, lo), i) => {
      val vdData = Wire(UInt(VLEN.W))
      val maskedData = Wire(UInt(VLEN.W))
      val resDataHi_31_0  = Wire(UInt(32.W))
      val resDataHi_63_32 = Wire(UInt(32.W))
      val resDataLo_31_0  = Wire(UInt(32.W))
      val resDataLo_63_32 = Wire(UInt(32.W))
      when(~vpuCtrl.vuopIdx(0)) {
        resDataHi_31_0  := Mux(sharedNarrow, io.ins(0).bits.oldVd.getOrElse(0.U)(95, 64), hi(31, 0))
        resDataHi_63_32 := Mux(sharedNarrow, io.ins(1).bits.oldVd.getOrElse(0.U)(127, 96), hi(63, 32))
        resDataLo_31_0  := lo(31, 0)
        resDataLo_63_32 := Mux(sharedNarrow, hi(31, 0), lo(63, 32))
      }.otherwise {
        resDataHi_31_0  := Mux(sharedNarrow, lo(31, 0), hi(31, 0))
        resDataHi_63_32 := Mux(sharedNarrow, hi(31, 0), hi(63, 32))
        resDataLo_31_0  := Mux(sharedNarrow, io.ins(0).bits.oldVd.getOrElse(0.U)(31, 0), lo(31, 0))
        resDataLo_63_32 := Mux(sharedNarrow, io.ins(0).bits.oldVd.getOrElse(0.U)(63, 32), lo(63, 32))
      }
      // Handles vd data ordering for narrow and widen cases.
      vdData := Cat(resDataHi_63_32, resDataHi_31_0, resDataLo_63_32, resDataLo_31_0)
      // Handles vd data mask.
      maskedData := Mux(vpuCtrl.isVFCmp, cmpResultBitMask(vdData), useMgu(vdData, vpuCtrl, 0))
      io.outs(0).bits.data(i) := maskedData
      io.outs(1).bits.data(i) := maskedData(127, 64)
    }
  }
}
