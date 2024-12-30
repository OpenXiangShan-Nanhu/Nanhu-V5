package xiangshan.backend.regfile

import chisel3.util.log2Up
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.RdConfig._
import xiangshan.backend.datapath.WbConfig._

trait regPort

abstract class PregParams {
  val numEntries: Int
  val numRead: Option[Int]
  val numWrite: Option[Int]
  val dataCfg: Set[DataConfig]
  val isFake: Boolean
  val rdType: RdConfig
  val wbType: PregWB

  def addrWidth = log2Up(numEntries)
}

case class IntPregParams(
  numEntries: Int,
  numRead   : Option[Int],
  numWrite  : Option[Int],
) extends PregParams {
  val dataCfg: Set[DataConfig] = Set(IntData())
  val isFake: Boolean = false
  val rdType: RdConfig = IntRD()
  val wbType: PregWB = IntWB()
}

// case class FpPregParams(
//                           numEntries: Int,
//                           numRead   : Option[Int],
//                           numWrite  : Option[Int],
//                         ) extends PregParams {

//   val dataCfg: DataConfig = FpData()
//   val isFake: Boolean = false
// }

case class VfPregParams(
  numEntries: Int,
  numRead   : Option[Int],
  numWrite  : Option[Int],
) extends PregParams {
  val dataCfg: Set[DataConfig] = Set(VecData(), FpData())
  val isFake: Boolean = false
  val rdType: RdConfig = VfRD()
  val wbType: PregWB = VfWB()
}

case class V0PregParams(
  numEntries: Int,
  numRead   : Option[Int],
  numWrite  : Option[Int],
) extends PregParams {

  val dataCfg: Set[DataConfig] = Set(V0Data())
  val isFake: Boolean = false
  val rdType: RdConfig = V0RD()
  val wbType: PregWB = V0WB()
}

case class VlPregParams(
  numEntries: Int,
  numRead   : Option[Int],
  numWrite  : Option[Int],
) extends PregParams {

  val dataCfg: Set[DataConfig] = Set(VlData())
  val isFake: Boolean = false
  val rdType: RdConfig = VlRD()
  val wbType: PregWB = VlWB()
}

case class NoPregParams() extends PregParams {
  val numEntries: Int = 0
  val numRead   : Option[Int] = None
  val numWrite  : Option[Int] = None

  val dataCfg: Set[DataConfig] = Set()
  val isFake: Boolean = false
  val rdType: RdConfig = NoRD()
  val wbType: PregWB = NoWB()
}

case class FakeIntPregParams(
  numEntries: Int,
  numRead   : Option[Int],
  numWrite  : Option[Int],
) extends PregParams {

  val dataCfg: Set[DataConfig] = Set(FakeIntData())
  val isFake: Boolean = true
  val rdType: RdConfig = NoRD()
  val wbType: PregWB = NoWB()
}
