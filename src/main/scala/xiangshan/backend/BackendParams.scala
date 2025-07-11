/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  ***************************************************************************************/

package xiangshan.backend

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.Bundles._
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.RdConfig._
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.datapath.{WakeUpConfig, WbArbiterParams}
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.issue._
import xiangshan.backend.regfile._
import xiangshan.XSCoreParamsKey
import xs.utils.perf.DebugOptionsKey


import scala.collection.mutable
import scala.reflect.{ClassTag, classTag}

case class BackendParams(
  schdParams : Map[SchedulerType, SchdBlockParams],
  pregParams : Seq[PregParams],
  iqWakeUpParams : Seq[WakeUpConfig],
) {

  def debugEn(implicit p: Parameters): Boolean = p(DebugOptionsKey).EnableDifftest

  def svaAssertEn(implicit p: Parameters): Boolean = false
  def svaCoverEn(implicit p: Parameters): Boolean = false
  def cvlLongSequence(implicit p: Parameters): Boolean = false

  def basicDebugEn(implicit p: Parameters): Boolean = p(DebugOptionsKey).AlwaysBasicDiff || debugEn

  val copyPdestInfo = mutable.HashMap[Int, (Int, Int)]()

  def updateCopyPdestInfo: Unit = allExuParams.filter(_.copyWakeupOut).map(x => getExuIdx(x.name) -> (x.copyDistance, -1)).foreach { x =>
    copyPdestInfo.addOne(x)
  }
  def isCopyPdest(exuIdx: Int): Boolean = {
    copyPdestInfo.contains(exuIdx)
  }
  def connectWakeup(exuIdx: Int): Unit = {
    println(s"[Backend] copyPdestInfo ${copyPdestInfo}")
    if (copyPdestInfo.contains(exuIdx)) {
      println(s"[Backend] exuIdx ${exuIdx} be connected, old info ${copyPdestInfo(exuIdx)}")
      val newInfo = exuIdx -> (copyPdestInfo(exuIdx)._1, copyPdestInfo(exuIdx)._2 + 1)
      copyPdestInfo.remove(exuIdx)
      copyPdestInfo += newInfo
      println(s"[Backend] exuIdx ${exuIdx} be connected, new info ${copyPdestInfo(exuIdx)}")
    }
  }
  def getCopyPdestIndex(exuIdx: Int): Int = {
    copyPdestInfo(exuIdx)._2 / copyPdestInfo(exuIdx)._1
  }
  def intSchdParams = schdParams.get(IntScheduler())
  // def fpSchdParams = schdParams.get(FpScheduler())
  def vfSchdParams = schdParams.get(VfScheduler())
  def memSchdParams = schdParams.get(MemScheduler())
  def allSchdParams: Seq[SchdBlockParams] =
    (Seq(intSchdParams) :+ vfSchdParams :+ memSchdParams)
    .filter(_.nonEmpty)
    .map(_.get)
  def allIssueParams: Seq[IssueBlockParams] =
    allSchdParams.map(_.issueBlockParams).flatten
  def allExuParams: Seq[ExeUnitParams] =
    allIssueParams.map(_.exuBlockParams).flatten

  // filter not fake exu unit
  def allRealExuParams =
    allExuParams.filterNot(_.fakeUnit)

  def intPregParams: IntPregParams = pregParams.collectFirst { case x: IntPregParams => x }.get
  def vfPregParams: VfPregParams = pregParams.collectFirst { case x: VfPregParams => x }.get
  def v0PregParams: V0PregParams = pregParams.collectFirst { case x: V0PregParams => x }.get
  def vlPregParams: VlPregParams = pregParams.collectFirst { case x: VlPregParams => x }.get
  def fakeIntPregParams: FakeIntPregParams = pregParams.collectFirst { case x: FakeIntPregParams => x }.get
  def getPregParams[T <: regPort](cfg : T): PregParams = {
    cfg match {
      case _: IntRD | _: IntWB => intPregParams
      // case _: FpRD | _: FpWB => fpPregParams
      case _: VfRD | _: VfWB => vfPregParams
      case _: V0RD | _: V0WB => v0PregParams
      case _: VlRD | _: VlWB => vlPregParams
      case _ => fakeIntPregParams
    }
  }

  def pregIdxWidth = pregParams.map(_.addrWidth).max

  def numSrc      : Int = allSchdParams.map(_.issueBlockParams.map(_.numSrc).max).max
  def numRegSrc   : Int = allSchdParams.map(_.issueBlockParams.map(_.numRegSrc).max).max
  def numIntRegSrc: Int = allSchdParams.map(_.issueBlockParams.map(_.numIntSrc).max).max
  def numVecRegSrc: Int = allSchdParams.map(_.issueBlockParams.map(_.numVecSrc).max).max


  def AluCnt = allSchdParams.map(_.AluCnt).sum
  def StaCnt = allSchdParams.map(_.StaCnt).sum
  def StdCnt = allSchdParams.map(_.StdCnt).sum
  def LduCnt = allSchdParams.map(_.LduCnt).sum
  def HyuCnt = allSchdParams.map(_.HyuCnt).sum
  def VlduCnt = allSchdParams.map(_.VlduCnt).sum
  def VstuCnt = allSchdParams.map(_.VstuCnt).sum
  def LsExuCnt = StaCnt + LduCnt + HyuCnt
  val LdExuCnt = LduCnt + HyuCnt
  val StaExuCnt = StaCnt + HyuCnt
  def JmpCnt = allSchdParams.map(_.JmpCnt).sum
  def BrhCnt = allSchdParams.map(_.BrhCnt).sum
  def CsrCnt = allSchdParams.map(_.CsrCnt).sum
  def IqCnt = allSchdParams.map(_.issueBlockParams.length).sum

  def numPcReadPort = allSchdParams.map(_.numPcReadPort).sum
  def numPcMemReadPort = allExuParams.filter(_.needPc).size
  def numTargetReadPort = allRealExuParams.count(x => x.needTarget)

  def numPregRd(pregParams: PregParams) = this.getRfReadSize(pregParams)
  def numPregWb(pregParams: PregParams) = this.getRfWriteSize(pregParams)

  def numNoDataWB = allSchdParams.map(_.numNoDataWB).sum
  def numExu = allSchdParams.map(_.numExu).sum

  def numException = allRealExuParams.count(_.exceptionOut.nonEmpty)

  def numRedirect = 1 // only for ahead info to frontend

  def numLoadDp = memSchdParams.get.issueBlockParams.filter(x => x.isLdAddrIQ || x.isHyAddrIQ).map(_.numEnq).sum

  def numStoreDp = memSchdParams.get.issueBlockParams.filter(x => x.isStAddrIQ || x.isHyAddrIQ).map(_.numEnq).sum

  def genIntIQValidNumBundle(implicit p: Parameters) = {
    this.intSchdParams.get.issueBlockParams.map(x => Vec(x.numDeq, UInt((x.numEntries).U.getWidth.W)))
  }

  // def genFpIQValidNumBundle(implicit p: Parameters) = {
  //   this.fpSchdParams.get.issueBlockParams.map(x => Vec(x.numDeq, UInt((x.numEntries).U.getWidth.W)))
  // }

  def genIntWriteBackBundle(implicit p: Parameters) = genWriteBackBundle(intPregParams)
  // def genFpWriteBackBundle(implicit p: Parameters) = genWriteBackBundle(fpPregParams)
  def genVfWriteBackBundle(implicit p: Parameters) = genWriteBackBundle(vfPregParams)
  def genV0WriteBackBundle(implicit p: Parameters) =  genWriteBackBundle(v0PregParams)
  def genVlWriteBackBundle(implicit p: Parameters) = genWriteBackBundle(vlPregParams)

  def genWriteBackBundle(pregParams: PregParams)(implicit p: Parameters): Seq[RfWritePortWithConfig] = {
    Seq.fill(this.getRfWriteSize(pregParams))(new RfWritePortWithConfig(pregParams))
  }

  def genWrite2CtrlBundles(implicit p: Parameters): MixedVec[ValidIO[ExuOutput]] = {
    MixedVec(allSchdParams.map(_.genExuOutputValidBundle.flatten).flatten)
  }

  def getIntWbArbiterParams: WbArbiterParams = {
    val intWbCfgs: Seq[IntWB] = allSchdParams.flatMap(_.getWbCfgs.flatten.flatten.filter(_.writeInt)).map(_.asInstanceOf[IntWB])
    datapath.WbArbiterParams(intWbCfgs, intPregParams, this)
  }

  def getVfWbArbiterParams: WbArbiterParams = {
    val vfWbCfgs: Seq[VfWB] = allSchdParams.flatMap(_.getWbCfgs.flatten.flatten.filter(x => x.writeVec)).map(_.asInstanceOf[VfWB])
    datapath.WbArbiterParams(vfWbCfgs, vfPregParams, this)
  }

  def getV0WbArbiterParams: WbArbiterParams = {
    val v0WbCfgs: Seq[V0WB] = allSchdParams.flatMap(_.getWbCfgs.flatten.flatten.filter(x => x.writeV0)).map(_.asInstanceOf[V0WB])
    datapath.WbArbiterParams(v0WbCfgs, v0PregParams, this)
  }

  def getVlWbArbiterParams: WbArbiterParams = {
    val vlWbCfgs: Seq[VlWB] = allSchdParams.flatMap(_.getWbCfgs.flatten.flatten.filter(x => x.writeVl)).map(_.asInstanceOf[VlWB])
    datapath.WbArbiterParams(vlWbCfgs, vlPregParams, this)
  }

  /**
    * Get regfile read port params
    *
    * @param dataCfg [[IntData]] or [[VecData]]
    * @return Seq[port->Seq[(exuIdx, priority)]
    */
  def getRdPortParams(cfg: RdConfig) = {
    // port -> Seq[exuIdx, priority]
    val cfgs: Seq[(Int, Seq[(Int, Int)])] = allRealExuParams
      .flatMap(x => x.rfrPortConfigs.flatten.map(xx => (xx, x.exuIdx)))
      .filter { x => x._1.getClass == cfg.getClass }
      .map(x => (x._1.port, (x._2, x._1.priority)))
      .groupBy(_._1)
      .map(x => (x._1, x._2.map(_._2).sortBy({ case (priority, _) => priority })))
      .toSeq
      .sortBy(_._1)
    cfgs
  }

  /**
    * Get regfile write back port params
    *
    * @param dataCfg [[IntData]] or [[VecData]]
    * @return Seq[port->Seq[(exuIdx, priority)]
    */
  def getWbPortParams(cfg: PregWB) = {
    val cfgs: Seq[(Int, Seq[(Int, Int)])] = allRealExuParams
      .flatMap(x => x.wbPortConfigs.map(xx => (xx, x.exuIdx)))
      .filter { x => x._1.getClass == cfg.getClass }
      .map(x => (x._1.port, (x._2, x._1.priority)))
      .groupBy(_._1)
      .map(x => (x._1, x._2.map(_._2)))
      .toSeq
      .sortBy(_._1)
    cfgs
  }

  def getRdPortIndices(cfg: RdConfig) = {
    this.getRdPortParams(cfg).map(_._1)
  }

  def getWbPortIndices(cfg: PregWB) = {
    this.getWbPortParams(cfg).map(_._1)
  }

  def getRdCfgs[T <: RdConfig](implicit tag: ClassTag[T]): Seq[Seq[Seq[RdConfig]]] = {
    val rdCfgs: Seq[Seq[Seq[RdConfig]]] = allIssueParams.map(
      _.exuBlockParams.map(
        _.rfrPortConfigs.map(
          _.collectFirst{ case x: T => x }
            .getOrElse(NoRD())
        )
      )
    )
    rdCfgs
  }

  def getAllWbCfgs: Seq[Seq[Set[PregWB]]] = {
    allIssueParams.map(_.exuBlockParams.map(_.wbPortConfigs.toSet))
  }

  def getWbCfgs[T <: PregWB](implicit tag: ClassTag[T]): Seq[Seq[PregWB]] = {
    val wbCfgs: Seq[Seq[PregWB]] = allIssueParams.map(_.exuBlockParams.map(_.wbPortConfigs.collectFirst{ case x: T => x }.getOrElse(NoWB())))
    wbCfgs
  }

  def getRfReadSize(pregParams: PregParams) = {
    pregParams.numRead.getOrElse(this.getRdPortIndices(pregParams.rdType).size)
  }

  def getRfWriteSize(pregParams: PregParams) = {
    pregParams.numWrite.getOrElse(this.getWbPortIndices(pregParams.wbType).size)
  }

  /**
    * Get size of read ports of int regcache
    */
  def getIntExuRCReadSize = {
    this.allExuParams.filter(x => x.isIntExeUnit).map(_.numIntSrc).reduce(_ + _)
  }

  def getMemExuRCReadSize = {
    this.allExuParams.filter(x => x.isMemExeUnit && x.readIntRf).map(_.numIntSrc).reduce(_ + _)
  }

  /**
    * Get size of write ports of int regcache
    */
  def getIntExuRCWriteSize = {
    this.allExuParams.filter(x => x.isIntExeUnit && x.isIQWakeUpSource).size
  }

  def getMemExuRCWriteSize = {
    this.allExuParams.filter(x => x.isMemExeUnit && x.isIQWakeUpSource && x.readIntRf).size
  }

  def getExuIdx(name: String): Int = {
    val exuParams = allRealExuParams
    if (name != "WB") {
      val foundExu = exuParams.find(_.name == name)
      require(foundExu.nonEmpty, s"exu $name not find")
      foundExu.get.exuIdx
    } else
      -1
  }

  def getExuName(idx: Int): String = {
    val exuParams = allRealExuParams
    exuParams(idx).name
  }

  def getExuParamByName(name: String): ExeUnitParams = {
    val exuParams = allExuParams
    exuParams.find(_.name == name).get
  }

  def getLdExuIdx(exu: ExeUnitParams): Int = {
    val ldExuParams = allRealExuParams.filter(x => x.hasHyldaFu || x.hasLoadFu)
    ldExuParams.indexOf(exu)
  }

  def getIntWBExeGroup: Map[Int, Seq[ExeUnitParams]] = allRealExuParams.groupBy(x => x.getIntWBPort.getOrElse(IntWB(port = -1)).port).filter(_._1 != -1)
  def getVfWBExeGroup: Map[Int, Seq[ExeUnitParams]] = allRealExuParams.groupBy(x => x.getVfWBPort.getOrElse(VfWB(port = -1)).port).filter(_._1 != -1)
  def getV0WBExeGroup: Map[Int, Seq[ExeUnitParams]] = allRealExuParams.groupBy(x => x.getV0WBPort.getOrElse(V0WB(port = -1)).port).filter(_._1 != -1)
  def getVlWBExeGroup: Map[Int, Seq[ExeUnitParams]] = allRealExuParams.groupBy(x => x.getVlWBPort.getOrElse(VlWB(port = -1)).port).filter(_._1 != -1)

  private def isContinuous(portIndices: Seq[Int]): Boolean = {
    val portIndicesSet = portIndices.toSet
    portIndicesSet.min == 0 && portIndicesSet.max == portIndicesSet.size - 1
  }

  def configChecks = {
    checkReadPortContinuous
    checkWritePortContinuous
    configCheck
  }

  def checkReadPortContinuous = {
    pregParams.filterNot(_.isFake).foreach { x =>
      if (x.numRead.isEmpty) {
        val portIndices: Seq[Int] = getRdPortIndices(x.rdType)
        require(isContinuous(portIndices),
          s"The read ports of ${x.getClass.getSimpleName} should be continuous, " +
            s"when numRead of ${x.getClass.getSimpleName} is None. The read port indices are $portIndices")
      }
    }
  }

  def checkWritePortContinuous = {
    pregParams.filterNot(_.isFake).foreach { x =>
      if (x.numWrite.isEmpty) {
        val portIndices: Seq[Int] = getWbPortIndices(x.wbType)
        require(
          isContinuous(portIndices),
          s"The write ports of ${x.getClass.getSimpleName} should be continuous, " +
            s"when numWrite of ${x.getClass.getSimpleName} is None. The write port indices are $portIndices"
        )
      }
    }
  }

  def configCheck = {
    // check 0
    val maxPortSource = 4

    allRealExuParams.map {
      case exuParam => exuParam.wbPortConfigs.collectFirst { case x: IntWB => x }
    }.filter(_.isDefined).groupBy(_.get.port).foreach {
      case (wbPort, priorities) => assert(priorities.size <= maxPortSource, "There has " + priorities.size + " exu's " + "Int WBport is " + wbPort + ", but the maximum is " + maxPortSource + ".")
    }
    allRealExuParams.map {
      case exuParam => exuParam.wbPortConfigs.collectFirst { case x: VfWB => x }
    }.filter(_.isDefined).groupBy(_.get.port).foreach {
      case (wbPort, priorities) => assert(priorities.size <= maxPortSource, "There has " + priorities.size + " exu's " + "Vf  WBport is " + wbPort + ", but the maximum is " + maxPortSource + ".")
    }

    // check 1
    // if some exus share the same wb port and rd ports, 
    // the exu with high priority at wb must also have high priority at rd.
    val wbTypes = Seq(IntWB(), VfWB())
    val rdTypes = Seq(IntRD(), VfRD())
    for(wbType <- wbTypes){
      for(rdType <- rdTypes){
        println(s"[BackendParams] wbType: ${wbType}, rdType: ${rdType}")
        allRealExuParams.map {
          case exuParam =>
            val wbPortConfigs = exuParam.wbPortConfigs
            val wbConfigs = wbType match{
              case _: IntWB => wbPortConfigs.collectFirst { case x: IntWB => x }
              case _: VfWB  => wbPortConfigs.collectFirst { case x: VfWB => x }
              case _        => None
            }
            val rfReadPortConfigs = exuParam.rfrPortConfigs
            val rdConfigs = rdType match{
              case _: IntRD => rfReadPortConfigs.flatten.filter(_.isInstanceOf[IntRD])
              case _: VfRD  => rfReadPortConfigs.flatten.filter(_.isInstanceOf[VfRD])
              case _        => Seq()
            }
            (wbConfigs, rdConfigs)
        }.filter(_._1.isDefined)
          .sortBy(_._1.get.priority)
          .groupBy(_._1.get.port).map { case (wbPort, intWbRdPairs) =>
            val rdCfgs = intWbRdPairs.map(_._2).flatten
            println(s"[BackendParams] wb port ${wbPort} rdcfgs: ${rdCfgs}")
            rdCfgs.groupBy(_.port).foreach { case (p, rdCfg) =>
              //println(s"[BackendParams] rdport: ${p}, cfgs: ${rdCfg}")
              rdCfg.zip(rdCfg.drop(1)).foreach { case (cfg0, cfg1) => assert(cfg0.priority <= cfg1.priority, s"an exu has high priority at ${wbType} wb port ${wbPort}, but has low priority at ${rdType} rd port ${p}") }
            }
        }
      }
    }
  }
}
