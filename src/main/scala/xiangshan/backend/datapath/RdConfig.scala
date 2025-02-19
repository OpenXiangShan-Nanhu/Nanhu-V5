package xiangshan.backend.datapath

import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.regfile.regPort

object RdConfig {
  sealed abstract class RdConfig() extends regPort {
    val port: Int
    val priority: Int

    def getDataConfig: Set[DataConfig]
  }

  case class IntRD(port: Int = -1, priority: Int = Int.MaxValue) extends RdConfig() {
    override def getDataConfig = Set(IntData())
  }

  case class FpRD(port: Int = -1, priority: Int = Int.MaxValue) extends RdConfig() {
    override def getDataConfig = Set(FpData())
  }

  case class VfRD(port: Int = -1, priority: Int = Int.MaxValue) extends RdConfig() {
    override def getDataConfig = Set(FpData(), VecData())
  }

  case class V0RD(port: Int = -1, priority: Int = Int.MaxValue) extends RdConfig() {
    override def getDataConfig = Set(V0Data())
  }

  case class VlRD(port: Int = -1, priority: Int = Int.MaxValue) extends RdConfig() {
    override def getDataConfig = Set(VlData())
  }

  case class NoRD() extends RdConfig() {
    override val port: Int = -1

    override val priority: Int = Int.MaxValue

    override def getDataConfig: Set[DataConfig] = Set()
  }
}

