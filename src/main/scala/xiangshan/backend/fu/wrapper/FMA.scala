package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xs.utils.perf.{XSError}
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.fu.fpu.FpPipedFuncUnit
import yunsuan.{VfmaType, VfpuType}
import yunsuan.fpu.FloatFMA

class FMA(cfg: FuConfig)(implicit p: Parameters) extends FpPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VfpuType.dummy, "fma OpType not supported")

  // io alias
  private val opcode = fuOpType(3, 0)
  private val src0 = inData.src(0)(63, 0)
  private val src1 = inData.src(1)(63, 0)
  private val src2 = inData.src(2)(63, 0)

  // modules
  private val fma = Module(new FloatFMA)

  val fp_aIsFpCanonicalNAN  = fp_fmt === VSew.e32 && !src1.head(32).andR ||
                              fp_fmt === VSew.e16 && !src1.head(48).andR
  val fp_bIsFpCanonicalNAN  = fp_fmt === VSew.e32 && !src0.head(32).andR ||
                              fp_fmt === VSew.e16 && !src0.head(48).andR
  val fp_cIsFpCanonicalNAN  = !(opcode === VfmaType.vfmul) && (fp_fmt === VSew.e32 && !src2.head(32).andR ||
                              fp_fmt === VSew.e16 && !src2.head(48).andR)

  fma.io.fire         := io.in.valid
  fma.io.fp_a         := src1
  fma.io.fp_b         := src0
  fma.io.fp_c         := src2
  fma.io.round_mode   := rm
  fma.io.fp_format    := fp_fmt
  fma.io.op_code      := opcode
  fma.io.fp_aIsFpCanonicalNAN := fp_aIsFpCanonicalNAN
  fma.io.fp_bIsFpCanonicalNAN := fp_bIsFpCanonicalNAN
  fma.io.fp_cIsFpCanonicalNAN := fp_cIsFpCanonicalNAN

  private val resultData = fma.io.fp_result
  private val fflagsData = fma.io.fflags

  io.out.bits.res.fflags.get := fflagsData
  io.out.bits.res.data       := resultData
}
