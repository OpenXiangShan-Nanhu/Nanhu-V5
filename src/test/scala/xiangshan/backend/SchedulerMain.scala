package nanhuv5.backend

import freechips.rocketchip.diplomacy.LazyModule
import top.{ArgParser, BaseConfig, Generator}
import nanhuv5.backend.issue.Scheduler
import nanhuv5.{XSCoreParameters, XSCoreParamsKey}

object SchedulerMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)

  val backendParams = config(XSCoreParamsKey).backendParams

  val schdParams = backendParams.intSchdParams.get
  val schd = LazyModule(new Scheduler(schdParams)(config))

  Generator.execute(
    firrtlOpts,
    schd.module,
    firtoolOpts
  )
}
