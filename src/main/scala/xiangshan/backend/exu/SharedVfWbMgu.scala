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
import xiangshan.backend.Bundles.VPUCtrlSignals

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
  def useMgu(vd: UInt, ctrl: VPUCtrlSignals, idx: Int): UInt = {
    val mgu = Module(new Mgu(VLEN))
    val allMaskTrue = VecInit(Seq.fill(VLEN)(true.B)).asUInt
    val mask = Mux(ctrl.fpu.isFpToVecInst, allMaskTrue, ctrl.vmask)
    val vl = Mux(ctrl.fpu.isFpToVecInst, 1.U, ctrl.vl)
    val vstart = Mux(ctrl.fpu.isFpToVecInst, 0.U, ctrl.vstart)
    val notUseVl = ctrl.fpu.isFpToVecInst
    val notModifyVd = !notUseVl && (vl === 0.U)
    mgu.io.in.vd := vd
    mgu.io.in.oldVd := Cat(io.ins(1).bits.oldVd.getOrElse(0.U)(63, 0), io.ins(0).bits.oldVd.getOrElse(0.U)(63, 0))
    mgu.io.in.mask := mask
    mgu.io.in.info.ta := ctrl.vta
    mgu.io.in.info.ma := ctrl.vma
    mgu.io.in.info.vl := vl
    mgu.io.in.info.vlmul := ctrl.vlmul
    mgu.io.in.info.valid := Mux(notModifyVd, false.B, io.ins(idx).valid)
    mgu.io.in.info.vstart := vstart
    mgu.io.in.info.eew := ctrl.veew
    mgu.io.in.info.vsew := ctrl.vsew
    mgu.io.in.info.vdIdx := ctrl.vuopIdx
    mgu.io.in.info.narrow := ctrl.isNarrow
    mgu.io.in.info.dstMask := ctrl.isDstMask
    mgu.io.in.isIndexedVls := false.B
    mgu.io.out.vd
  }

  def useTailAgnostic(vl: UInt, vd:UInt, destMask:Bool): UInt = {
    val mask = ((1.U << vl) - 1.U)
    Mux(destMask, (vd & mask) | (~mask), vd)
  }
  
  resData_hi.zip(resData_lo).zipWithIndex.foreach {
    case ((hi, lo), i) => {
      val vdData = Wire(UInt(VLEN.W))
      val maskedData = Wire(UInt(VLEN.W))
      val outData    = Wire(UInt(VLEN.W))
      val resDataHi_31_0  = Wire(UInt(32.W))
      val resDataHi_63_32 = Wire(UInt(32.W))
      val resDataLo_31_0  = Wire(UInt(32.W))
      val resDataLo_63_32 = Wire(UInt(32.W))
      val vpuCtrl = io.ins.head.bits.shareVpuCtrl.get
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
      vdData := Cat(resDataHi_63_32, resDataHi_31_0, resDataLo_63_32, resDataLo_31_0)
      maskedData := useMgu(vdData, vpuCtrl, 0)
      outData := useTailAgnostic(vpuCtrl.vl, maskedData, vpuCtrl.isDstMask)
      io.outs(0).bits.data(i) := outData
      io.outs(1).bits.data(i) := outData(127, 64)
    }
  }
}
