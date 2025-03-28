package xiangshan.backend.datapath

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}
import xs.utils.{GTimer, GatedValidRegNext, HasCircularQueuePtrHelper, SelectOne}
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.issue.EntryBundles._
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.DataSource
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.issue.IssueBlockParams

class VfCrossMatrix(iqBlkParam: IssueBlockParams, exuParam: ExeUnitParams)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in  = Vec(2, Flipped(DecoupledIO(new IssueQueueIssueBundle(iqBlkParam, exuParam))))
    val out = Vec(2, DecoupledIO(new IssueQueueIssueBundle(iqBlkParam, exuParam)))
    val cancel = Vec(2, Output(Bool()))
    val cross = Output(Bool())
  })

  val cross = WireInit(0.U.asTypeOf(io.in(0).bits))
  val in0_to_in1 = WireInit(0.U.asTypeOf(io.in(0).bits))
  val in1_to_in0 = WireInit(0.U.asTypeOf(io.in(1).bits))

  val in0IsAfterOfIn1 = io.in(0).bits.common.robIdx < io.in(1).bits.common.robIdx ||
                        (io.in(0).bits.common.robIdx === io.in(1).bits.common.robIdx &&
                          io.in(0).bits.common.vpu.get.vuopIdx < io.in(1).bits.common.vpu.get.vuopIdx &&
                          !io.in(0).bits.common.vpu.get.fpu.isFpToVecInst)
  
  val in0_cross_to_in1 = io.in(0).valid && !io.in(0).bits.common.vpu.get.fpu.isFpToVecInst && (~io.in(1).valid || (io.in(1).valid && in0IsAfterOfIn1))
  val in1_cross_to_in0 = io.in(1).valid && !io.in(1).bits.common.vpu.get.fpu.isFpToVecInst && (~io.in(0).valid || (io.in(0).valid && !in0IsAfterOfIn1))

  in0_to_in1.addrOH  := io.in(0).bits.addrOH
  in0_to_in1.immType := io.in(0).bits.immType
  in0_to_in1.srcType := io.in(0).bits.srcType
  in0_to_in1.rf      := io.in(0).bits.rf
  in0_to_in1.common  := io.in(0).bits.common
  // in0_to_in1.common.vfWenH.foreach(x => x := true.B)
  // in0_to_in1.common.v0WenH.foreach(x => x := true.B)
  // in0_to_in1.common.vfWenL.foreach(x => x := false.B)
  // in0_to_in1.common.v0WenL.foreach(x => x := false.B)

  in1_to_in0.addrOH  := io.in(1).bits.addrOH
  in1_to_in0.immType := io.in(1).bits.immType
  in1_to_in0.srcType := io.in(1).bits.srcType
  in1_to_in0.rf      := io.in(1).bits.rf
  in1_to_in0.common  := io.in(1).bits.common
  // in1_to_in0.common.vfWenH.foreach(x => x := true.B)
  // in1_to_in0.common.v0WenH.foreach(x => x := true.B)
  // in1_to_in0.common.vfWenL.foreach(x => x := false.B)
  // in1_to_in0.common.v0WenL.foreach(x => x := false.B)

  cross := Mux(in0_cross_to_in1, in0_to_in1, in1_to_in0)

  io.out(0).bits  := Mux(in1_cross_to_in0, in1_to_in0, io.in(0).bits)
  io.out(0).bits.crossed.foreach(_ := false.B)
  io.out(0).valid := io.in(0).valid || in1_cross_to_in0

  io.out(1).bits  := Mux(in0_cross_to_in1, cross, io.in(1).bits)
  io.out(0).bits.crossed.foreach(_ := in1_cross_to_in0)
  io.out(1).valid := io.in(1).valid || in0_cross_to_in1
  io.out(1).bits.crossed.foreach(_ := in0_cross_to_in1)

  io.cancel(0) := io.in(0).valid && (in1_cross_to_in0 || io.in(1).valid && !io.in(0).bits.common.vpu.get.fpu.isFpToVecInst && !in0IsAfterOfIn1)
  io.cancel(1) := io.in(1).valid && (in0_cross_to_in1 || io.in(0).valid && !io.in(1).bits.common.vpu.get.fpu.isFpToVecInst && in0IsAfterOfIn1)

  // io.in(0).ready := io.in(0).valid && (in1_cross_to_in0 || io.in(1).valid && !io.in(0).bits.common.vpu.get.fpu.isFpToVecInst && !in0IsAfterOfIn1)
  // io.in(1).ready := io.in(1).valid && (in0_cross_to_in1 || io.in(0).valid && !io.in(1).bits.common.vpu.get.fpu.isFpToVecInst && in0IsAfterOfIn1)
  io.in(0).ready := Mux(in1_cross_to_in0, true.B, io.out(0).ready)
  io.in(1).ready := Mux(in0_cross_to_in1, true.B, io.out(1).ready)

  io.cross := in1_cross_to_in0 || in0_cross_to_in1
}