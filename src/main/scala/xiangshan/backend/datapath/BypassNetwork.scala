package xiangshan.backend.datapath

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xs.utils.{GatedValidRegNext, SignExt, ZeroExt}
import xiangshan.{XSBundle, XSModule}
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles.{ExuBypassBundle, ExuInput, ExuOH, ExuOutput, ExuVec, ImmInfo, VPUCtrlSignals}
import xiangshan.backend.issue.{ImmExtractor, IntScheduler, MemScheduler, VfScheduler}
import xiangshan.backend.datapath.DataConfig.RegDataMaxWidth
import xiangshan.backend.decode.ImmUnion
import xiangshan.backend.regcache._
import xiangshan.backend.fu.FuType
import yunsuan.VfaluType

class BypassNetworkIO()(implicit p: Parameters, params: BackendParams) extends XSBundle {
  // params
  private val intSchdParams = params.schdParams(IntScheduler())
  private val memSchdParams = params.schdParams(MemScheduler())

  val fromDataPath = new FromDataPath
  val toExus = new ToExus
  val fromExus = new FromExus

  class FromDataPath extends Bundle {
    val int: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = Flipped(intSchdParams.genExuInputBundle)
    val mem: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = Flipped(memSchdParams.genExuInputBundle)
    val immInfo: Vec[ImmInfo] = Input(Vec((int.flatten ++ mem.flatten).size, new ImmInfo))
    val rcData: MixedVec[MixedVec[Vec[UInt]]] = MixedVec(
      Seq(intSchdParams, memSchdParams).map(schd => schd.issueBlockParams.map(iq => 
        MixedVec(iq.exuBlockParams.map(exu => Input(Vec(exu.numRegSrc, UInt(exu.srcDataBitsMax.W)))))
      )).flatten
    )
  }

  class ToExus extends Bundle {
    val int: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = intSchdParams.genExuInputBundle
    val mem: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = memSchdParams.genExuInputBundle
  }

  class FromExus extends Bundle {
    val int: MixedVec[MixedVec[ValidIO[ExuBypassBundle]]] = Flipped(intSchdParams.genExuBypassValidBundle)
    val mem: MixedVec[MixedVec[ValidIO[ExuBypassBundle]]] = Flipped(memSchdParams.genExuBypassValidBundle)

    def connectExuOutput(
      getSinkVecN: FromExus => MixedVec[MixedVec[ValidIO[ExuBypassBundle]]]
    )(
      sourceVecN: MixedVec[MixedVec[DecoupledIO[ExuOutput]]]
    ): Unit = {
      getSinkVecN(this).zip(sourceVecN).foreach { case (sinkVec, sourcesVec) =>
        sinkVec.zip(sourcesVec).foreach { case (sink, source) =>
          sink.valid := source.valid
          sink.bits.intWen := source.bits.intWen.getOrElse(false.B)
          sink.bits.pdest := source.bits.pdest
          sink.bits.data := source.bits.data(0)
        }
      }
    }
  }

  val toDataPath: Vec[RCWritePort] = Vec(params.getIntExuRCWriteSize + params.getMemExuRCWriteSize, 
    Flipped(new RCWritePort(params.intSchdParams.get.rfDataWidth, RegCacheIdxWidth, params.intSchdParams.get.pregIdxWidth, params.debugEn)))
}

class BypassNetwork()(implicit p: Parameters, params: BackendParams) extends XSModule {
  val io: BypassNetworkIO = IO(new BypassNetworkIO)

  private val fromDPs: Seq[DecoupledIO[ExuInput]] = (io.fromDataPath.int ++ io.fromDataPath.mem).flatten.toSeq
  private val fromExus: Seq[ValidIO[ExuBypassBundle]] = (io.fromExus.int ++ io.fromExus.mem).flatten.toSeq
  private val toExus: Seq[DecoupledIO[ExuInput]] = (io.toExus.int ++ io.toExus.mem).flatten.toSeq
  private val fromDPsRCData: Seq[Vec[UInt]] = io.fromDataPath.rcData.flatten.toSeq
  private val immInfo = io.fromDataPath.immInfo

  private val intExuNum = params.intSchdParams.get.numExu
  private val memExuNum = params.memSchdParams.get.numExu

  println(s"[BypassNetwork] allExuNum: ${toExus.size} intExuNum: ${intExuNum} memExuNum: ${memExuNum}")
  println(s"[BypassNetwork] RCData num: ${fromDPsRCData.size}")

  // (exuIdx, srcIdx, bypassExuIdx)
  private val forwardOrBypassValidVec3: MixedVec[Vec[Vec[Bool]]] = MixedVecInit(
    fromDPs.map { (x: DecoupledIO[ExuInput]) =>
      val wakeUpSourceIdx = x.bits.params.iqWakeUpSinkPairs.map(x => x.source.getExuParam(params.allExuParams).exuIdx)
      val mask = Wire(chiselTypeOf(x.bits.l1ExuOH.getOrElse(VecInit(Seq.fill(x.bits.params.numRegSrc max 1)(VecInit(0.U(ExuVec.width.W).asBools))))))
      mask.map{ case m =>
        val vecMask = Wire(Vec(m.getWidth, Bool()))
        vecMask.zipWithIndex.map{ case(v, i) => if (wakeUpSourceIdx.contains(i)) v := true.B
                                                else v := false.B }
        m := vecMask
      }
      println(s"[BypassNetwork] ${x.bits.params.name} numRegSrc: ${x.bits.params.numRegSrc}")
      val validVec2 = VecInit(x.bits.l1ExuOH.getOrElse(
        // TODO: remove tmp max 1 for fake HYU1
        VecInit(Seq.fill(x.bits.params.numRegSrc max 1)(VecInit(0.U(ExuVec.width.W).asBools)))
      ).zip(mask).map{ case (l,m) =>
        VecInit(l.zip(m).map(x => x._1 && x._2))
      })
      VecInit(validVec2.map(v => VecInit(v.take(intExuNum) ++ v.takeRight(memExuNum))))
    }
  )

  private val forwardDataVec: Vec[UInt] = VecInit(
    fromExus.map(x => ZeroExt(x.bits.data, RegDataMaxWidth))
  )

  private val bypassDataVec = VecInit(
    fromExus.map(x => ZeroExt(RegEnable(x.bits.data, x.valid), RegDataMaxWidth))
  )

  private val forwardPdestVec: Vec[UInt] = VecInit(fromExus.map(_.bits.pdest))
  private val forwardValidVec: Vec[Bool] = VecInit(fromExus.map(_.valid))

  private val bypassPdestVec: Vec[UInt] = VecInit(fromExus.map(x => RegEnable(x.bits.pdest, x.valid)))
  private val bypassValidVec: Vec[Bool] = VecInit(fromExus.map(x => RegNext(x.valid)))

  toExus.zip(fromDPs).foreach { case (sink, source) =>
    sink <> source
  }

  toExus.zipWithIndex.foreach { case (exuInput, exuIdx) => {
      exuInput.bits.src.zipWithIndex.foreach { case (src, srcIdx) =>
        val imm = ImmExtractor(
          immInfo(exuIdx).imm,
          immInfo(exuIdx).immType,
          exuInput.bits.params.destDataBitsMax,
          exuInput.bits.params.immType.map(_.litValue)
        )
        val immLoadSrc0 = SignExt(ImmUnion.U.toImm32(immInfo(exuIdx).imm(immInfo(exuIdx).imm.getWidth - 1, ImmUnion.I.len)), XLEN)
        val exuParm = exuInput.bits.params
        val isIntScheduler = exuParm.isIntExeUnit
        val dataSource = exuInput.bits.dataSources(srcIdx)
        val isWakeUpSink = params.allIssueParams.filter(_.exuBlockParams.contains(exuParm)).head.exuBlockParams.map(_.isIQWakeUpSink).reduce(_ || _)
        val readForward = if (isWakeUpSink) dataSource.readForward else false.B
        val readBypass = if (isWakeUpSink) dataSource.readBypass else false.B
        val readZero = if (isIntScheduler) dataSource.readZero else false.B
        val readRegOH = exuInput.bits.dataSources(srcIdx).readRegOH
        val readRegCache = if (exuParm.needReadRegCache) exuInput.bits.dataSources(srcIdx).readRegCache else false.B
        val readImm = if (exuParm.immType.nonEmpty || exuParm.hasLoadExu) exuInput.bits.dataSources(srcIdx).readImm else false.B
        val forwardData = Mux1H(forwardOrBypassValidVec3(exuIdx)(srcIdx), forwardDataVec)
        val bypassData = Mux1H(forwardOrBypassValidVec3(exuIdx)(srcIdx), bypassDataVec)

        if(params.debugEn && exuInput.bits.params.isIQWakeUpSink) {
          when(readForward && exuInput.fire) {
            val forwardPdest = Mux1H(forwardOrBypassValidVec3(exuIdx)(srcIdx), forwardPdestVec)
            val forwardValid = Mux1H(forwardOrBypassValidVec3(exuIdx)(srcIdx), forwardValidVec)
            assert(forwardValid && forwardPdest === exuInput.bits.rfForAssert.get(srcIdx))
          }.elsewhen(readBypass && exuInput.fire) {
            val bypassPdest = Mux1H(forwardOrBypassValidVec3(exuIdx)(srcIdx), bypassPdestVec)
            val bypassValid = Mux1H(forwardOrBypassValidVec3(exuIdx)(srcIdx), bypassValidVec)
            assert(bypassValid && bypassPdest === exuInput.bits.rfForAssert.get(srcIdx))
          }
        }

        val srcData = Mux1H(
          Seq(
            readForward    -> forwardData,
            readBypass     -> bypassData,
            readZero       -> 0.U,
            readRegOH      -> fromDPs(exuIdx).bits.src(srcIdx),
            readRegCache   -> fromDPsRCData(exuIdx)(srcIdx),
            readImm        -> (if (exuParm.hasLoadExu && srcIdx == 0) immLoadSrc0 else imm)
          )
        )
        src := srcData
      }
    }
  }

  // to reg cache
  private val forwardIntWenVec = VecInit(
    fromExus.filter(_.bits.params.needWriteRegCache).map(x => x.valid && x.bits.intWen)
  )
  private val forwardTagVec = VecInit(
    fromExus.filter(_.bits.params.needWriteRegCache).map(x => x.bits.pdest)
  )

  private val bypassIntWenVec = VecInit(
    forwardIntWenVec.map(x => GatedValidRegNext(x))
  )
  private val bypassTagVec = VecInit(
    forwardTagVec.zip(forwardIntWenVec).map(x => RegEnable(x._1, x._2))
  )
  private val bypassRCDataVec = VecInit(
    fromExus.zip(bypassDataVec).filter(_._1.bits.params.needWriteRegCache).map(_._2)
  )

  println(s"[BypassNetwork] WriteRegCacheExuNum: ${forwardIntWenVec.size}")

  io.toDataPath.zipWithIndex.foreach{ case (x, i) => 
    x.wen := bypassIntWenVec(i)
    x.addr := DontCare
    x.data := bypassRCDataVec(i)
    x.tag.foreach(_ := bypassTagVec(i))
  }
}
