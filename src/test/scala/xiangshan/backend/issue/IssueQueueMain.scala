package nanhuv5.backend.issue

import freechips.rocketchip.diplomacy.LazyModule
import top.{ArgParser, BaseConfig, Generator}
import nanhuv5.{XSCoreParameters, XSCoreParamsKey}

object IssueQueueMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)

  val backendParams = config(XSCoreParamsKey).backendParams

  val iqParams: IssueBlockParams = backendParams.intSchdParams.get.issueBlockParams.head
  val iq: IssueQueue = LazyModule(new IssueQueue(iqParams)(config))

  Generator.execute(
    firrtlOpts,
    iq.module,
    firtoolOpts
  )
}
