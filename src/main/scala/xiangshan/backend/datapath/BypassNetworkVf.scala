package xiangshan.backend.datapath

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xs.utils.{SignExt, ZeroExt}
import xiangshan._
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles.{VPUCtrlSignals, ImmInfo, ExuVec}
import xiangshan.backend.decode.ImmUnion
import xiangshan.backend.issue.{VfScheduler, ImmExtractor}
import xiangshan.backend.fu.FuType
import yunsuan.VfaluType

class BypassNetworkVf(implicit p: Parameters, params: BackendParams) extends XSModule {
  private val vfSchdParams = params.schdParams(VfScheduler())

  val io = IO(new Bundle {
    val flush = Flipped(ValidIO(new Redirect))
    val fromDataPath = new Bundle {
      val uop = Flipped(vfSchdParams.genExuInputBundle)
      val imm = Input(Vec(vfSchdParams.issueBlockParams.map(_.exuBlockParams).flatten.size, new ImmInfo))
    }
    val toVfExu = vfSchdParams.genExuInputBundle
    val fromVfExu = Flipped(vfSchdParams.genExuBypassValidBundle)
  })
  
  private val intExuNum = params.intSchdParams.get.numExu
  private val memExuNum = params.memSchdParams.get.numExu
  private val vfExuNum = params.vfSchdParams.get.numExu

  private val forwardOrBypassValidVec3: MixedVec[Vec[Vec[Bool]]] = MixedVecInit(
    io.fromDataPath.uop.flatten.map { x =>
      val wakeUpSourceIdx = x.bits.params.iqWakeUpSinkPairs.map(x => x.source.getExuParam(params.allExuParams).exuIdx)
      val mask = Wire(chiselTypeOf(x.bits.l1ExuOH.getOrElse(VecInit(Seq.fill(x.bits.params.numRegSrc max 1)(VecInit(0.U(ExuVec.width.W).asBools))))))
      mask.map{ case m =>
        val vecMask = Wire(Vec(m.getWidth, Bool()))
        vecMask.zipWithIndex.map{ case(v, i) => if (wakeUpSourceIdx.contains(i)) v := true.B else v := false.B }
        m := vecMask
      }
      println(s"[BypassNetwork] ${x.bits.params.name} numRegSrc: ${x.bits.params.numRegSrc}")
      val validVec2 = VecInit(x.bits.l1ExuOH.getOrElse(
        // TODO: remove tmp max 1 for fake HYU1
        VecInit(Seq.fill(x.bits.params.numRegSrc max 1)(VecInit(0.U(ExuVec.width.W).asBools)))
      ).zip(mask).map{ case (l,m) =>
        VecInit(l.zip(m).map(x => x._1 && x._2))
      })
      RegNext(VecInit(validVec2.map(v => VecInit(v.drop(intExuNum).take(vfExuNum)))))
    }
  )

  private val forwardDataVec: Vec[UInt] = VecInit(
    io.fromVfExu.flatten.map(x => x.bits.data)
  )

  val validVec = io.fromVfExu.flatten.map(x => x.valid)
  private val bypassDataVec = VecInit(
    forwardDataVec.zipWithIndex.map{ case (x, i) => RegEnable(x, validVec(i)) }
  )

  val validVecBp2 = io.fromVfExu.flatten.map(x => RegNext(x.valid))
  val bypass2DataVec = VecInit(
    bypassDataVec.zipWithIndex.map{ case (x, i) => RegEnable(x, validVecBp2(i)) }
  )

  val og2Uop = io.fromDataPath.uop.flatten.map(x => RegEnable(x.bits, x.valid))
  io.fromDataPath.uop.flatten.zip(io.toVfExu.flatten).foreach { case (uop, exu) =>
    if(!uop.bits.params.latencyCertain) {
      println(s"vf uncertain latency ${uop.bits.params.name}")
      uop.ready := uop.valid && exu.ready && !exu.valid
    } else {
      uop.ready := true.B
    }
  }

  io.toVfExu.flatten.zip(io.fromDataPath.uop.flatten).foreach {
    case (exu, uop) => {
      val og2Valid = if(!uop.bits.params.latencyCertain) RegNext(uop.valid && exu.ready && !exu.valid && !(uop.bits.robIdx.needFlush(io.flush))) else RegNext(uop.valid && !(uop.bits.robIdx.needFlush(io.flush)))
      exu.valid := og2Valid
    }
  }
  io.toVfExu.flatten.zip(og2Uop).zip(io.fromDataPath.uop.flatten).zip(io.fromDataPath.imm).zipWithIndex.foreach {
    case ((((exuIn, og2), uop), immInfo), idx) => {
      exuIn.bits := og2
      val isSharedVf = FuType.FuTypeOrR(og2.fuType, FuType.sharedVf)
      val needReadHi = isSharedVf && ((og2.vecWen.getOrElse(false.B) &&
                        og2.vfWenH.getOrElse(false.B) && !og2.vfWenL.getOrElse(false.B)) ||
                        (og2.v0Wen.getOrElse(false.B) && og2.v0WenH.getOrElse(false.B) &&
                        !og2.v0WenL.getOrElse(false.B)))
      val isWidenF_VV = isSharedVf && og2.vpu.getOrElse(0.U.asTypeOf(new VPUCtrlSignals)).isWiden &&
                        og2.fuType =/= FuType.f2v.id.U && og2.fuType =/= FuType.i2v.id.U &&
                        !(og2.fuType === FuType.vfalu.id.U && og2.fuOpType(6))
      val isWidenF_WV = isSharedVf && og2.vpu.getOrElse(0.U.asTypeOf(new VPUCtrlSignals)).isWiden &&
                        og2.fuType =/= FuType.f2v.id.U && og2.fuType =/= FuType.i2v.id.U &&
                        !(og2.fuType === FuType.vfalu.id.U && !og2.fuOpType(6))
      val isWidenF_REDOSUM = isSharedVf && og2.vpu.getOrElse(0.U.asTypeOf(new VPUCtrlSignals)).isWiden &&
                            og2.fuType === FuType.vfalu.id.U && og2.fuOpType === VfaluType.vfwredosum
      exuIn.bits.src.zipWithIndex.foreach { case (src, srcIdx) =>
        val imm = ImmExtractor(
          immInfo.imm,
          immInfo.immType,
          uop.bits.params.destDataBitsMax,
          uop.bits.params.immType.map(_.litValue)
        )
        val immLoadSrc0 = SignExt(ImmUnion.U.toImm32(immInfo.imm(immInfo.imm.getWidth - 1, ImmUnion.I.len)), XLEN)
        val exuParm = exuIn.bits.params
        val isIntScheduler = exuParm.isIntExeUnit
        val isReadVfRf= exuParm.readVfRf
        val isReadV0Rf= exuParm.readV0Rf
        val dataSource = og2.dataSources(srcIdx)
        val isWakeUpSink = params.allIssueParams.filter(_.exuBlockParams.contains(exuParm)).head.exuBlockParams.map(_.isIQWakeUpSink).reduce(_ || _)
        val readForward = if (isWakeUpSink) dataSource.readForward else false.B
        val readBypass = if (isWakeUpSink) dataSource.readBypass else false.B
        val readBypass2 = if (isWakeUpSink) dataSource.readBypass2 else false.B
        val readV0 = if (srcIdx < 3 && isReadVfRf) dataSource.readV0 else false.B
        val readRegOH = og2.dataSources(srcIdx).readRegOH
        val readImm = if (exuParm.immType.nonEmpty || exuParm.hasLoadExu) og2.dataSources(srcIdx).readImm else false.B
        val forwardData = Mux1H(forwardOrBypassValidVec3(idx)(srcIdx), forwardDataVec)
        val bypassData = Mux1H(forwardOrBypassValidVec3(idx)(srcIdx), bypassDataVec)
        val bypass2Data = Mux1H(forwardOrBypassValidVec3(idx)(srcIdx), bypass2DataVec)
        val srcData = Mux1H(
          Seq(
            readForward    -> forwardData,
            readBypass     -> bypassData,
            readBypass2    -> bypass2Data,
            readRegOH      -> RegNext(uop.bits.src(srcIdx)),
            readImm        -> RegNext(imm)
          )
        ) 
        if(srcIdx == 0) {
          when(isWidenF_REDOSUM) {
            src := srcData
          }.elsewhen(isWidenF_VV || isWidenF_WV) {
            val widenDataHi = srcData(127, 96) ## srcData(63, 32)
            val widenDataLo = srcData(95, 64) ## srcData(31, 0)
            src := Mux(needReadHi, widenDataHi, widenDataLo)
          }.elsewhen(isSharedVf) {
            src := Mux(needReadHi, srcData(127, 64), srcData)
          }.otherwise {
            src := srcData
          }
        } else if(srcIdx == 1) {
          when(isWidenF_REDOSUM) {
            src := srcData
          }.elsewhen(isWidenF_VV) {
            val widenDataHi = srcData(127, 96) ## srcData(63, 32)
            val widenDataLo = srcData(95, 64) ## srcData(31, 0)
            src := Mux(needReadHi, widenDataHi, widenDataLo)
          }.elsewhen(isSharedVf) {
            src := Mux(needReadHi, srcData(127, 64), srcData)
          }.otherwise {
            src := srcData
          }
        } else if(srcIdx == 2) {
          when(isSharedVf) {
            src := Mux(needReadHi, srcData(127, 64), srcData)
          }.otherwise {
            src := srcData
          }
          
        } else {
          src := srcData
        }
      }
    }
  }
}
