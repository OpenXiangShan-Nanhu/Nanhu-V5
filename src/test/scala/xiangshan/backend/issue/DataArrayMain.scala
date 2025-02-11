package nanhuv5.backend.issue

import chisel3._
import freechips.rocketchip.diplomacy.DisableMonitors
import top.{ArgParser, BaseConfig, Generator}
import nanhuv5.{XSCoreParameters, XSCoreParamsKey}

object DataArrayMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)

  val backendParams = config(XSCoreParamsKey).backendParams

  val iqParams: IssueBlockParams = backendParams.intSchdParams.get.issueBlockParams.head

  Generator.execute(
    firrtlOpts,
    // DataArray
    DisableMonitors(p =>
      new DataArray(Vec(iqParams.dataBitsMax, Bool()), iqParams.numDeq, iqParams.numEnq, iqParams.numEntries)(p))(config),
    firtoolOpts
  )
}