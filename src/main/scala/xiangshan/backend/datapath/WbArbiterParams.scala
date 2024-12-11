package xiangshan.backend.datapath

import org.chipsalliance.cde.config.Parameters
import chisel3.Output
import chisel3.util.{DecoupledIO, MixedVec, ValidIO, log2Up}
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles.WriteBackBundle
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.regfile._

case class WbArbiterParams(
  wbCfgs    : Seq[PregWB],
  pregParams: PregParams,
  backendParams: BackendParams,
) {

  def numIn = wbCfgs.length

  def numOut = pregParams.numWrite.getOrElse(backendParams.getWbPortIndices(pregParams.wbType).size)

  def dataWidth = pregParams.dataCfg.map(_.dataWidth).max

  def addrWidth = log2Up(pregParams.numEntries)

  def genInput(implicit p: Parameters) = {
    MixedVec(wbCfgs.map(x => DecoupledIO(new WriteBackBundle(x, backendParams))))
  }

  def genOutput(implicit p: Parameters): MixedVec[ValidIO[WriteBackBundle]] = {
    Output(MixedVec(Seq.tabulate(numOut) {
      x =>
        ValidIO(new WriteBackBundle(
          wbCfgs.head match {
            case cfg: IntWB => IntWB(port = x)
            case cfg: VfWB  => VfWB(port = x)
            case cfg: V0WB  => V0WB(port = x)
            case cfg: VlWB  => VlWB(port = x)
            case _          => ???
          },
          backendParams
        )
        )
    }
    ))
  }
}
