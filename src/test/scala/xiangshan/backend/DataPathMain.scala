package nanhuv5.backend

import freechips.rocketchip.diplomacy.LazyModule
import top.{ArgParser, BaseConfig, Generator}
import nanhuv5.backend.datapath.DataPath
import nanhuv5.{XSCoreParameters, XSCoreParamsKey}


object DataPathMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)

  val backendParams = config(XSCoreParamsKey).backendParams
  val dataPath = LazyModule(new DataPath(backendParams)(config))

  Generator.execute(
    firrtlOpts,
    dataPath.module,
    firtoolOpts
  )
}
