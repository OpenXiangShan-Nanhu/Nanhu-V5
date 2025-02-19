package xiangshan.backend.datapath

import chisel3.util.log2Up
import xiangshan.backend.BackendParams
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.regfile.regPort

object WbConfig {
  sealed abstract class WbConfig() {
    val port: Int
    def dataCfg: Set[DataConfig]
    def dataWidth: Int = dataCfg.map(_.dataWidth).max

    def writeInt = dataCfg.contains(IntData())
    // def writeFp = dataCfg == FpData()
    def writeVec = dataCfg.contains(VecData()) || dataCfg.contains(FpData())
    def writeV0 = dataCfg.contains(V0Data())
    def writeVl = dataCfg.contains(VlData())

    override def toString: String = {
      var res = this match {
        case _: IntWB => "INT"
        // case _: FpWB => "FP"
        case _: VfWB => "VF"
        case _: V0WB => "V0"
        case _: VlWB => "VL"
        case _: NoWB => "NO"
        case _ => "??"
      }
      res += s"($port)"
      res
    }
  }

  sealed abstract class ExuWB extends WbConfig

  sealed abstract class PregWB extends ExuWB with regPort {
    val priority: Int

    def numPreg(backendParams: BackendParams): Int

    def pregIdxWidth(backendParams: BackendParams) = log2Up(numPreg(backendParams))
  }

  case class IntWB(
    port    : Int = -1,
    priority: Int = Int.MaxValue,
  ) extends PregWB {

    def dataCfg: Set[DataConfig] = Set(IntData())

    def numPreg(backendParams: BackendParams): Int = backendParams.intPregParams.numEntries
  }

  // case class FpWB(
  //   port: Int = -1,
  //   priority: Int = Int.MaxValue,
  // ) extends PregWB {

  //   def dataCfg: DataConfig = FpData()

  //   def numPreg(backendParams: BackendParams): Int = backendParams.fpPregParams.numEntries
  // }

  case class VfWB(
    port    : Int = -1,
    priority: Int = Int.MaxValue,
  ) extends PregWB {

    def dataCfg: Set[DataConfig] = Set(FpData(), VecData())

    def numPreg(backendParams: BackendParams): Int = backendParams.vfPregParams.numEntries
  }

  case class V0WB(
    port    : Int = -1,
    priority: Int = Int.MaxValue,
  ) extends PregWB {

    def dataCfg: Set[DataConfig] = Set(V0Data())

    def numPreg(backendParams: BackendParams): Int = backendParams.v0PregParams.numEntries
  }

  case class VlWB(
    port    : Int = -1,
    priority: Int = Int.MaxValue,
  ) extends PregWB {

    def dataCfg: Set[DataConfig] = Set(VlData())

    def numPreg(backendParams: BackendParams): Int = backendParams.vlPregParams.numEntries
  }

  case class NoWB(
    port    : Int = -1,
    priority: Int = Int.MaxValue,
  ) extends PregWB {

    override def dataCfg: Set[DataConfig] = Set()

    override def numPreg(backendParams: BackendParams): Int = 0
  }

  case class CtrlWB(
    port: Int = -1,
  ) extends WbConfig {
    val priority: Int = Int.MaxValue
    override def dataCfg: Set[DataConfig] = Set()
  }

  case class FakeIntWB(
    port    : Int = -1,
    priority: Int = Int.MaxValue,
  ) extends PregWB {

    def dataCfg: Set[DataConfig] = Set(FakeIntData())

    def numPreg(backendParams: BackendParams): Int = 0
  }
}
