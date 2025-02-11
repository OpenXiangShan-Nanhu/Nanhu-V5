package nanhuv5.backend.fu.fpu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xs.utils.DataHoldBypass
import nanhuv5.backend.fu.{FuConfig, FuncUnit}

class FpNonPipedFuncUnit(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg)
  with FpFuncUnitAlias
{
  protected val outCtrl     = DataHoldBypass(io.in.bits.ctrl, io.in.fire)
  protected val outData     = DataHoldBypass(io.in.bits.data, io.in.fire)

  connectNonPipedCtrlSingal
}
