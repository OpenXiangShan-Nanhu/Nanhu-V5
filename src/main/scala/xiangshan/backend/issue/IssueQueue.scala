package xiangshan.backend.issue

import chisel3._
import chisel3.util._
import chisel3.ltl.{Sequence, AssertProperty, Delay, SequenceAtom}
import chisel3.ltl.Sequence.BoolSequence
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}
import xs.utils.{GTimer, GatedValidRegNext, HasCircularQueuePtrHelper, SelectOne}
import xs.utils.perf.{XSPerfAccumulate, XSPerfHistogram}
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.issue.EntryBundles._
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.DataSource
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.rob.RobPtr
import xiangshan.mem.{LqPtr, MemWaitUpdateReq, SqPtr}

class IssueQueue(params: IssueBlockParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false

  implicit val iqParams: IssueBlockParams = params

  lazy val module: IssueQueueImp = iqParams.schdType match {
    case IntScheduler() => new IssueQueueIntImp(this)
    case VfScheduler()  => new IssueQueueVfImp(this)
    case MemScheduler() =>
      if (iqParams.StdCnt == 0 && !iqParams.isVecMemIQ) new IssueQueueMemAddrImp(this)
      else if (iqParams.isVecMemIQ) new IssueQueueVecMemImp(this)
      else new IssueQueueIntImp(this)
    case _ => null
  }
}

class IssueQueueStatusBundle(numEnq: Int, numEntries: Int) extends Bundle {
  val empty = Output(Bool())
  val full = Output(Bool())
  val validCnt = Output(UInt(log2Ceil(numEntries + 1).W))
  val leftVec = Output(Vec(numEnq + 1, Bool()))
}

class IssueQueueDeqRespBundle(implicit p:Parameters, params: IssueBlockParams) extends EntryDeqRespBundle

class IssueQueueIO()(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
  // Inputs
  val flush = Flipped(ValidIO(new Redirect))
  val enq = Vec(params.numEnq, Flipped(DecoupledIO(new DynInst)))

  val og0Resp = Vec(params.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle)))
  val og1Resp = Vec(params.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle)))
  val finalIssueResp = Option.when(params.LdExuCnt > 0 || params.VlduCnt > 0)(Vec(params.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle))))
  val memAddrIssueResp = Option.when(params.LdExuCnt > 0)(Vec(params.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle))))
  val vecLoadIssueResp = Option.when(params.VlduCnt > 0)(Vec(params.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle))))

  val og0Cancel = Input(ExuVec())
  val og1Cancel = Input(ExuVec())
  val ldCancel = Vec(backendParams.LduCnt + backendParams.HyuCnt, Flipped(new LoadCancelIO))

  val wbBusyTableRead: MixedVec[WbFuBusyTableReadBundle] = Input(params.genWbFuBusyTableReadBundle)
  val wbBusyTableWrite: MixedVec[WbFuBusyTableWriteBundle] = Output(params.genWbFuBusyTableWriteBundle)

  val wakeupFromWB: MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = Flipped(params.genWBWakeUpSinkValidBundle)
  val wakeupFromIQ: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(params.genIQWakeUpSinkValidBundle)

  val vlFromIntIsZero = Input(Bool())
  val vlFromIntIsVlmax = Input(Bool())
  val vlFromVfIsZero = Input(Bool())
  val vlFromVfIsVlmax = Input(Bool())

  val replaceRCIdx = Option.when(params.needWriteRegCache)(Vec(params.numDeq, Input(UInt(RegCacheIdxWidth.W))))

  val wakeupToIQ: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = params.genIQWakeUpSourceValidBundle
  val status = Output(new IssueQueueStatusBundle(params.numEnq, params.numEntries))
  val validCntDeqVec = Output(Vec(params.numDeq,UInt(params.numEntries.U.getWidth.W)))

  val deqDelay: MixedVec[DecoupledIO[IssueQueueIssueBundle]] = params.genIssueDecoupledBundle

  def allWakeUp = wakeupFromWB ++ wakeupFromIQ
}

class IssueQueueImp(override val wrapper: IssueQueue)(implicit p: Parameters, val params: IssueBlockParams)
  extends LazyModuleImp(wrapper) with HasXSParameter {

  override def desiredName: String = s"${params.getIQName}"

  println(s"[IssueQueueImp] ${params.getIQName}:" +
    s"\n\twakeupFromWB(${io.wakeupFromWB.size})," +
    s"\n\twakeup exu in(${params.wakeUpInExuSources.size}): ${params.wakeUpInExuSources.map(_.name).mkString("{",", ","}")}," +
    s"\n\twakeup exu out(${params.wakeUpOutExuSources.size}): ${params.wakeUpOutExuSources.map(_.name).mkString("{",", ","}")}," +
    s"\n\tnumEntries: ${params.numEntries}, numRegSrc: ${params.numRegSrc}," +
    s"\n\tnumEnq: ${params.numEnq}, numSimp: ${params.numSimp}, numComp: ${params.numComp}, numDeq: ${params.numDeq}," +
    s"\n\tisAllSimp: ${params.isAllSimp}, isAllComp: ${params.isAllComp}")

  require(params.numExu <= 2, "IssueQueue has not supported more than 2 deq ports")
  require(params.numEnq <= 2, "IssueQueue has not supported more than 2 enq ports")
  require(params.numSimp == 0 || params.numSimp >= params.numEnq, "numSimp should be 0 or at least not less than numEnq")
  require(params.numComp == 0 || params.numComp >= params.numEnq, "numComp should be 0 or at least not less than numEnq")

  val deqFuCfgs     : Seq[Seq[FuConfig]] = params.exuBlockParams.map(_.fuConfigs)
  val allDeqFuCfgs  : Seq[FuConfig] = params.exuBlockParams.flatMap(_.fuConfigs)
  val fuCfgsCnt     : Map[FuConfig, Int] = allDeqFuCfgs.groupBy(x => x).map { case (cfg, cfgSeq) => (cfg, cfgSeq.length) }
  val commonFuCfgs  : Seq[FuConfig] = fuCfgsCnt.filter(_._2 > 1).keys.toSeq
  val wakeupFuLatencyMaps : Seq[Map[FuType.Value, Int]] = params.exuBlockParams.map(x => x.wakeUpFuLatencyMap)

  println(s"[IssueQueueImp] ${params.getIQName} fuLatencyMaps: ${wakeupFuLatencyMaps}")
  println(s"[IssueQueueImp] ${params.getIQName} commonFuCfgs: ${commonFuCfgs.map(_.name)}")

  lazy val io = IO(new IssueQueueIO())

  val entries = Module(new Entries)
  val fuBusyTableWrite    = params.exuBlockParams.map { case x => Option.when(x.latencyValMax > 0)(Module(new FuBusyTableWrite(x.fuLatencyMap))) }
  val fuBusyTableRead     = params.exuBlockParams.map { case x => Option.when(x.latencyValMax > 0)(Module(new FuBusyTableRead(x.fuLatencyMap))) }
  val intWbBusyTableWrite = params.exuBlockParams.map { case x => Option.when(x.intLatencyCertain)(Module(new FuBusyTableWrite(x.intFuLatencyMap))) }
  val intWbBusyTableRead  = params.exuBlockParams.map { case x => Option.when(x.intLatencyCertain)(Module(new FuBusyTableRead(x.intFuLatencyMap))) }
  val vfWbBusyTableWrite  = params.exuBlockParams.map { case x => Option.when(x.vfLatencyCertain)(Module(new FuBusyTableWrite(x.vfFuLatencyMap))) }
  val vfWbBusyTableRead   = params.exuBlockParams.map { case x => Option.when(x.vfLatencyCertain)(Module(new FuBusyTableRead(x.vfFuLatencyMap))) }
  val v0WbBusyTableWrite  = params.exuBlockParams.map { case x => Option.when(x.v0LatencyCertain)(Module(new FuBusyTableWrite(x.v0FuLatencyMap))) }
  val v0WbBusyTableRead   = params.exuBlockParams.map { case x => Option.when(x.v0LatencyCertain)(Module(new FuBusyTableRead(x.v0FuLatencyMap))) }
  val vlWbBusyTableWrite  = params.exuBlockParams.map { case x => Option.when(x.vlLatencyCertain)(Module(new FuBusyTableWrite(x.vlFuLatencyMap))) }
  val vlWbBusyTableRead   = params.exuBlockParams.map { case x => Option.when(x.vlLatencyCertain)(Module(new FuBusyTableRead(x.vlFuLatencyMap))) }

  class WakeupQueueFlush extends Bundle {
    val redirect = ValidIO(new Redirect)
    val ldCancel = Vec(backendParams.LduCnt + backendParams.HyuCnt, new LoadCancelIO)
    val og0Fail = Output(Bool())
    val og1Fail = Output(Bool())
  }

  private def flushFunc(exuInput: ExuInput, flush: WakeupQueueFlush, stage: Int): Bool = {
    val redirectFlush = exuInput.robIdx.needFlush(flush.redirect)
    val loadDependencyFlush = LoadShouldCancel(exuInput.loadDependency, flush.ldCancel)
    val ogFailFlush = stage match {
      case 1 => flush.og0Fail
      case 2 => flush.og1Fail
      case _ => false.B
    }
    redirectFlush || loadDependencyFlush || ogFailFlush
  }

  private def modificationFunc(exuInput: ExuInput): ExuInput = {
    val newExuInput = WireDefault(exuInput)
    newExuInput.loadDependency match {
      case Some(deps) => deps.zip(exuInput.loadDependency.get).foreach(x => x._1 := x._2 << 1)
      case None =>
    }
    newExuInput
  }

  private def lastConnectFunc(exuInput: ExuInput, newInput: ExuInput): ExuInput = {
    val lastExuInput = WireDefault(exuInput)
    val newExuInput = WireDefault(newInput)
    newExuInput.elements.foreach { case (name, data) =>
      if (lastExuInput.elements.contains(name)) {
        data := lastExuInput.elements(name)
      }
    }
    if (newExuInput.pdestCopy.nonEmpty && !lastExuInput.pdestCopy.nonEmpty) {
      newExuInput.pdestCopy.get.foreach(_ := lastExuInput.pdest)
    }
    if (newExuInput.rfWenCopy.nonEmpty && !lastExuInput.rfWenCopy.nonEmpty) {
      newExuInput.rfWenCopy.get.foreach(_ := lastExuInput.rfWen.get)
    }
    if (newExuInput.fpWenCopy.nonEmpty && !lastExuInput.fpWenCopy.nonEmpty) {
      newExuInput.fpWenCopy.get.foreach(_ := lastExuInput.fpWen.get)
    }
    if (newExuInput.vecWenCopy.nonEmpty && !lastExuInput.vecWenCopy.nonEmpty) {
      newExuInput.vecWenCopy.get.foreach(_ := lastExuInput.vecWen.get)
    }
    if (newExuInput.v0WenCopy.nonEmpty && !lastExuInput.v0WenCopy.nonEmpty) {
      newExuInput.v0WenCopy.get.foreach(_ := lastExuInput.v0Wen.get)
    }
    if (newExuInput.vlWenCopy.nonEmpty && !lastExuInput.vlWenCopy.nonEmpty) {
      newExuInput.vlWenCopy.get.foreach(_ := lastExuInput.vlWen.get)
    }
    if (newExuInput.loadDependencyCopy.nonEmpty && !lastExuInput.loadDependencyCopy.nonEmpty) {
      newExuInput.loadDependencyCopy.get.foreach(_ := lastExuInput.loadDependency.get)
    }
    newExuInput
  }

  val wakeUpQueues: Seq[Option[MultiWakeupQueue[ExuInput, WakeupQueueFlush]]] = params.exuBlockParams.map {
    exu => Option.when(exu.isIQWakeUpSource && !exu.hasLoadExu)(Module(
      new MultiWakeupQueue( new ExuInput(exu),
                            new ExuInput(exu, exu.copyWakeupOut, exu.copyNum),
                            new WakeupQueueFlush,
                            exu.wakeUpFuLatancySet,
                            flushFunc,
                            modificationFunc,
                            lastConnectFunc)))
  }

  val deqBeforeDly = Wire(params.genIssueDecoupledBundle)

  val intWbBusyTableIn  = io.wbBusyTableRead.map(_.intWbBusyTable)
  val vfWbBusyTableIn   = io.wbBusyTableRead.map(_.vfWbBusyTable)
  val v0WbBusyTableIn   = io.wbBusyTableRead.map(_.v0WbBusyTable)
  val vlWbBusyTableIn   = io.wbBusyTableRead.map(_.vlWbBusyTable)

  val intWbBusyTableOut = io.wbBusyTableWrite.map(_.intWbBusyTable)
  val vfWbBusyTableOut  = io.wbBusyTableWrite.map(_.vfWbBusyTable)
  val v0WbBusyTableOut  = io.wbBusyTableWrite.map(_.v0WbBusyTable)
  val vlWbBusyTableOut  = io.wbBusyTableWrite.map(_.vlWbBusyTable)

  val intDeqRespSetOut  = io.wbBusyTableWrite.map(_.intDeqRespSet)
  val vfDeqRespSetOut   = io.wbBusyTableWrite.map(_.vfDeqRespSet)
  val v0DeqRespSetOut   = io.wbBusyTableWrite.map(_.v0DeqRespSet)
  val vlDeqRespSetOut   = io.wbBusyTableWrite.map(_.vlDeqRespSet)

  val fuBusyTableMask     = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))
  val intWbBusyTableMask  = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))
  val vfWbBusyTableMask   = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))
  val v0WbBusyTableMask   = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))
  val vlWbBusyTableMask   = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))

  val s0_enqValidVec = io.enq.map(_.valid)
  val s0_enqSelValidVec = Wire(Vec(params.numEnq, Bool()))
  val s0_enqNotFlush = !io.flush.valid
  val s0_enqBits = WireInit(VecInit(io.enq.map(_.bits)))
  val s0_doEnqSelValidVec = s0_enqSelValidVec.map(_ && s0_enqNotFlush) //enqValid && notFlush && enqReady

  val finalDeqSelValidVec = Wire(Vec(params.numDeq, Bool()))
  val finalDeqSelOHVec    = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))

  val validVec = VecInit(entries.io.valid.asBools)
  val issuedVec = VecInit(entries.io.issued.asBools)
  val requestForTrans = VecInit(validVec.zip(issuedVec).map(x => x._1 && !x._2))
  val canIssueVec = VecInit(entries.io.canIssue.asBools)
  val fpToVfVec = entries.io.fpToVec.getOrElse(0.U)
  dontTouch(canIssueVec)
  if(params.sharedVf) {
    dontTouch(fpToVfVec)
  }
  val deqFirstIssueVec = entries.io.isFirstIssue
  val dataSources: Vec[Vec[DataSource]] = entries.io.dataSources
  val finalDataSources: Vec[Vec[DataSource]] = VecInit(finalDeqSelOHVec.map(oh => Mux1H(oh, dataSources)))
  val loadDependency: Vec[Vec[UInt]] = entries.io.loadDependency
  val finalLoadDependency: IndexedSeq[Vec[UInt]] = VecInit(finalDeqSelOHVec.map(oh => Mux1H(oh, loadDependency)))
  // (entryIdx)(srcIdx)(exuIdx)
  val wakeUpL1ExuOH: Option[Vec[Vec[Vec[Bool]]]] = entries.io.srcWakeUpL1ExuOH
  // (deqIdx)(srcIdx)(exuIdx)
  val finalWakeUpL1ExuOH: Option[Vec[Vec[Vec[Bool]]]] = wakeUpL1ExuOH.map(x => VecInit(finalDeqSelOHVec.map(oh => Mux1H(oh, x))))

  val fuTypeVec = Wire(Vec(params.numEntries, FuType()))
  val deqEntryVec = Wire(Vec(params.numDeq, ValidIO(new EntryBundle)))
  val canIssueMergeAllBusy = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))
  val deqCanIssue = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))

  //deq
  val enqEntryOldestSel = Wire(Vec(params.numDeq, ValidIO(UInt(params.numEnq.W))))
  val simpEntryOldestSel = Option.when(params.hasCompAndSimp)(Wire(Vec(params.numDeq + params.numEnq, ValidIO(UInt(params.numSimp.W)))))
  val compEntryOldestSel = Option.when(params.hasCompAndSimp)(Wire(Vec(params.numDeq, ValidIO(UInt(params.numComp.W)))))
  val othersEntryOldestSel = Wire(Vec(params.numDeq, ValidIO(UInt((params.numEntries - params.numEnq).W))))
  
  val deqSelValidVec = Wire(Vec(params.numDeq, Bool()))
  val deqSelOHVec    = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))
  val cancelDeqVec = Wire(Vec(params.numDeq, Bool()))

  val subDeqSelValidVec = Option.when(params.deqFuSame)(Wire(Vec(params.numDeq, Bool())))
  val subDeqSelOHVec = Option.when(params.deqFuSame)(Wire(Vec(params.numDeq, UInt(params.numEntries.W))))
  val subDeqRequest = Option.when(params.deqFuSame)(Wire(UInt(params.numEntries.W)))

  //trans
  val simpEntryEnqSelVec = Option.when(params.hasCompAndSimp)(Wire(Vec(params.numEnq, UInt(params.numSimp.W))))
  val compEntryEnqSelVec = Option.when(params.hasCompAndSimp)(Wire(Vec(params.numEnq, UInt(params.numComp.W))))
  val othersEntryEnqSelVec = Option.when(params.isAllComp || params.isAllSimp)(Wire(Vec(params.numEnq, UInt((params.numEntries - params.numEnq).W))))
  val simpAgeDetectRequest = Option.when(params.hasCompAndSimp)(Wire(Vec(params.numDeq + params.numEnq, UInt(params.numSimp.W))))
  simpAgeDetectRequest.foreach(_ := 0.U.asTypeOf(simpAgeDetectRequest.get))

  // when vf exu (with og2) wake up int/mem iq (without og2), the wakeup signals should delay 1 cycle
  // as vf exu's min latency is 1, we do not need consider og0cancel
  val wakeupFromIQ = Wire(chiselTypeOf(io.wakeupFromIQ))
  wakeupFromIQ.zip(io.wakeupFromIQ).foreach { case (w, w_src) =>
    if (!params.inVfSchd && params.readVfRf && params.hasWakeupFromVf && w_src.bits.params.isVfExeUnit) {
      val noCancel = !LoadShouldCancel(Some(w_src.bits.loadDependency), io.ldCancel)
      w := RegNext(Mux(noCancel, w_src, 0.U.asTypeOf(w)))
      w.bits.loadDependency.zip(w_src.bits.loadDependency).foreach{ case (ld, ld_src) => ld := RegNext(Mux(noCancel, ld_src << 1, 0.U.asTypeOf(ld))) }
    } else {
      w := w_src
    }
  }

  /**
    * Connection of [[entries]]
    */
  entries.io match { case entriesIO: EntriesIO =>
    entriesIO.flush                                             := io.flush
    entriesIO.enq.zipWithIndex.foreach { case (enq, enqIdx) =>
      enq.valid                                                 := s0_doEnqSelValidVec(enqIdx)
      enq.bits.status.robIdx                                    := s0_enqBits(enqIdx).robIdx
      enq.bits.status.fuType                                    := s0_enqBits(enqIdx).fuType
      val numLsrc = s0_enqBits(enqIdx).srcType.size.min(enq.bits.status.srcStatus.map(_.srcType).size)
      for(j <- 0 until numLsrc) {
        enq.bits.status.srcStatus(j).psrc                       := s0_enqBits(enqIdx).psrc(j)
        enq.bits.status.srcStatus(j).srcType                    := s0_enqBits(enqIdx).srcType(j)
        enq.bits.status.srcStatus(j).srcState                   := (if (j < 3) {
                                                                      Mux(SrcType.isVp(s0_enqBits(enqIdx).srcType(j)) && (s0_enqBits(enqIdx).psrc(j) === 0.U), 
                                                                          SrcState.rdy, 
                                                                          s0_enqBits(enqIdx).srcState(j))
                                                                    } else {
                                                                      s0_enqBits(enqIdx).srcState(j)
                                                                    })
        enq.bits.status.srcStatus(j).dataSources.value          := (if (j < 3) {
                                                                      MuxCase(DataSource.reg, Seq(
                                                                        (SrcType.isXp(s0_enqBits(enqIdx).srcType(j)) && (s0_enqBits(enqIdx).psrc(j) === 0.U)) -> DataSource.zero,
                                                                        (SrcType.isNotReg(s0_enqBits(enqIdx).srcType(j)))                                     -> DataSource.imm,
                                                                        (SrcType.isVp(s0_enqBits(enqIdx).srcType(j)) && (s0_enqBits(enqIdx).psrc(j) === 0.U)) -> DataSource.v0,
                                                                      ))
                                                                    } else {
                                                                      MuxCase(DataSource.reg, Seq(
                                                                        SrcType.isNotReg(s0_enqBits(enqIdx).srcType(j))  -> DataSource.imm,
                                                                      ))
                                                                    })
        enq.bits.status.srcStatus(j).srcLoadDependency          := VecInit(s0_enqBits(enqIdx).srcLoadDependency(j).map(x => x << 1))
        if(params.hasIQWakeUp) {
          enq.bits.status.srcStatus(j).srcWakeUpL1ExuOH.get     := 0.U.asTypeOf(ExuVec())
        }
        enq.bits.status.srcStatus(j).useRegCache.foreach(_      := s0_enqBits(enqIdx).useRegCache(j))
        enq.bits.status.srcStatus(j).regCacheIdx.foreach(_      := s0_enqBits(enqIdx).regCacheIdx(j))
      }
      enq.bits.status.blocked                                   := false.B
      enq.bits.status.issued                                    := false.B
      enq.bits.status.firstIssue                                := false.B
      enq.bits.status.issueTimer                                := "b11".U
      enq.bits.status.deqPortIdx                                := 0.U
      enq.bits.imm.foreach(_                                    := s0_enqBits(enqIdx).imm)
      (enq.bits.payload: Data).waiveAll                                          :<= (s0_enqBits(enqIdx): Data).waiveAll
      enq.bits.payload.vpu.connectComplex(s0_enqBits(enqIdx).vpu)
    }
    entriesIO.og0Resp.zipWithIndex.foreach { case (og0Resp, i) =>
      og0Resp                                                   := io.og0Resp(i)
    }
    entriesIO.og1Resp.zipWithIndex.foreach { case (og1Resp, i) =>
      og1Resp                                                   := io.og1Resp(i)
    }
    if (params.isLdAddrIQ || params.isHyAddrIQ) {
      entriesIO.fromLoad.get.finalIssueResp.zipWithIndex.foreach { case (finalIssueResp, i) =>
        finalIssueResp                                          := io.finalIssueResp.get(i)
      }
      entriesIO.fromLoad.get.memAddrIssueResp.zipWithIndex.foreach { case (memAddrIssueResp, i) =>
        memAddrIssueResp                                        := io.memAddrIssueResp.get(i)
      }
    }
    if (params.isVecLduIQ) {
      entriesIO.vecLdIn.get.finalIssueResp.zipWithIndex.foreach { case (resp, i) =>
        resp := io.finalIssueResp.get(i)
      }
      entriesIO.vecLdIn.get.resp.zipWithIndex.foreach { case (resp, i) =>
        resp                                                    := io.vecLoadIssueResp.get(i)
      }
    }
    for(deqIdx <- 0 until params.numDeq) {
      entriesIO.deqReady(deqIdx)                                := deqBeforeDly(deqIdx).ready
      entriesIO.deqSelOH(deqIdx).valid := deqSelValidVec(deqIdx)
      entriesIO.deqSelOH(deqIdx).bits                           := deqSelOHVec(deqIdx)
      entriesIO.enqEntryOldestSel(deqIdx)                       := enqEntryOldestSel(deqIdx)
      entriesIO.simpEntryOldestSel.foreach(_(deqIdx)            := simpEntryOldestSel.get(deqIdx))
      entriesIO.compEntryOldestSel.foreach(_(deqIdx)            := compEntryOldestSel.get(deqIdx))
      entriesIO.othersEntryOldestSel.foreach(_(deqIdx)          := othersEntryOldestSel(deqIdx))
      entriesIO.subDeqRequest.foreach(_(deqIdx)                 := subDeqRequest.get)
      entriesIO.subDeqSelOH.foreach(_(deqIdx)                   := subDeqSelOHVec.get(deqIdx))
    }
    entriesIO.wakeUpFromWB                                      := io.wakeupFromWB
    entriesIO.wakeUpFromIQ                                      := wakeupFromIQ
    entriesIO.vlFromIntIsZero                                   := io.vlFromIntIsZero
    entriesIO.vlFromIntIsVlmax                                  := io.vlFromIntIsVlmax
    entriesIO.vlFromVfIsZero                                    := io.vlFromVfIsZero
    entriesIO.vlFromVfIsVlmax                                   := io.vlFromVfIsVlmax
    entriesIO.og0Cancel                                         := io.og0Cancel
    entriesIO.og1Cancel                                         := io.og1Cancel
    entriesIO.ldCancel                                          := io.ldCancel
    entriesIO.simpEntryDeqSelVec.foreach(_                      := VecInit(simpEntryOldestSel.get.takeRight(params.numEnq).map(_.bits)))
    //output
    fuTypeVec                                                   := entriesIO.fuType
    deqEntryVec                                                 := entriesIO.deqEntry
    cancelDeqVec                                                := entriesIO.cancelDeqVec
    simpEntryEnqSelVec.foreach(_                                := entriesIO.simpEntryEnqSelVec.get)
    compEntryEnqSelVec.foreach(_                                := entriesIO.compEntryEnqSelVec.get)
    othersEntryEnqSelVec.foreach(_                              := entriesIO.othersEntryEnqSelVec.get)
  }


  s0_enqSelValidVec := s0_enqValidVec.zip(io.enq).map{ case (enqValid, enq) => enqValid && enq.ready}

  protected val commonAccept: UInt = Cat(fuTypeVec.map(fuType =>
    FuType.FuTypeOrR(fuType, commonFuCfgs.map(_.fuType))
  ).reverse)

  // if deq port can accept the uop
  protected val canAcceptVec: Seq[UInt] = deqFuCfgs.map { fuCfgs: Seq[FuConfig] =>
    Cat(fuTypeVec.map(fuType =>
      FuType.FuTypeOrR(fuType, fuCfgs.map(_.fuType))
    ).reverse)
  }

  protected val deqCanAcceptVec: Seq[IndexedSeq[Bool]] = deqFuCfgs.map { fuCfgs: Seq[FuConfig] =>
    fuTypeVec.map(fuType =>
      FuType.FuTypeOrR(fuType, fuCfgs.map(_.fuType)))
  }

  canIssueMergeAllBusy.zipWithIndex.foreach { case (merge, i) =>
    val mergeFuBusy = {
      if (fuBusyTableWrite(i).nonEmpty) canIssueVec.asUInt & (~fuBusyTableMask(i))
      else canIssueVec.asUInt
    }
    val mergeIntWbBusy = {
      if (intWbBusyTableRead(i).nonEmpty) mergeFuBusy & (~intWbBusyTableMask(i))
      else mergeFuBusy
    }
    val mergeVfWbBusy = {
      if (vfWbBusyTableRead(i).nonEmpty) mergeIntWbBusy & (~vfWbBusyTableMask(i))
      else mergeIntWbBusy
    }
    val mergeV0WbBusy = {
      if (v0WbBusyTableRead(i).nonEmpty) mergeVfWbBusy & (~v0WbBusyTableMask(i))
      else mergeVfWbBusy
    }
    val mergeVlWbBusy = {
      if (vlWbBusyTableRead(i).nonEmpty) mergeV0WbBusy & (~vlWbBusyTableMask(i))
      else  mergeV0WbBusy
    }
    merge := mergeVlWbBusy
  }

  deqCanIssue.zipWithIndex.foreach { case (req, i) =>
    req := canIssueMergeAllBusy(i) & VecInit(deqCanAcceptVec(i)).asUInt
  }
  dontTouch(fuTypeVec)
  dontTouch(canIssueMergeAllBusy)
  dontTouch(deqCanIssue)

  if (params.numDeq == 2) {
    require(params.deqFuSame || params.deqFuDiff, "The 2 deq ports need to be identical or completely different")
  }

  if (params.numDeq == 2 && params.deqFuSame) {
    val subDeqPolicy = Module(new DeqPolicy())

    enqEntryOldestSel := DontCare

    if (params.isAllComp || params.isAllSimp) {
      othersEntryOldestSel(0) := AgeDetector(numEntries = params.numEntries - params.numEnq,
        enq = othersEntryEnqSelVec.get,
        canIssue = canIssueVec.asUInt(params.numEntries-1, params.numEnq)
      )
      othersEntryOldestSel(1) := DontCare

      subDeqPolicy.io.request := subDeqRequest.get
      subDeqSelValidVec.get := subDeqPolicy.io.deqSelOHVec.map(oh => oh.valid)
      subDeqSelOHVec.get := subDeqPolicy.io.deqSelOHVec.map(oh => oh.bits)
    } else {
      simpAgeDetectRequest.get(0) := canIssueVec.asUInt(params.numEnq + params.numSimp - 1, params.numEnq)
      simpAgeDetectRequest.get(1) := DontCare
      simpAgeDetectRequest.get(params.numDeq) := VecInit(requestForTrans.drop(params.numEnq).take(params.numSimp)).asUInt
      if (params.numEnq == 2) {
        simpAgeDetectRequest.get(params.numDeq + 1) := VecInit(requestForTrans.drop(params.numEnq).take(params.numSimp)).asUInt & ~simpEntryOldestSel.get(params.numDeq).bits
      }

      simpEntryOldestSel.get := AgeDetector(numEntries = params.numSimp,
        enq = simpEntryEnqSelVec.get,
        canIssue = simpAgeDetectRequest.get
      )
      
      compEntryOldestSel.get(0) := AgeDetector(numEntries = params.numComp,
        enq = compEntryEnqSelVec.get,
        canIssue = canIssueVec.asUInt(params.numEntries - 1, params.numEnq + params.numSimp)
      )
      compEntryOldestSel.get(1) := DontCare

      othersEntryOldestSel(0).valid := compEntryOldestSel.get(0).valid || simpEntryOldestSel.get(0).valid
      othersEntryOldestSel(0).bits := Cat(
        compEntryOldestSel.get(0).bits,
        Fill(params.numSimp, !compEntryOldestSel.get(0).valid) & simpEntryOldestSel.get(0).bits,
      )
      othersEntryOldestSel(1) := DontCare

      subDeqPolicy.io.request := Reverse(subDeqRequest.get)
      subDeqSelValidVec.get := subDeqPolicy.io.deqSelOHVec.map(oh => oh.valid)
      subDeqSelOHVec.get := subDeqPolicy.io.deqSelOHVec.map(oh => Reverse(oh.bits))
    }

    if(params.sharedVf) {
      subDeqRequest.get := canIssueVec.asUInt & ~Cat(othersEntryOldestSel(0).bits, 0.U((params.numEnq).W)) & fpToVfVec
    } else {
      subDeqRequest.get := canIssueVec.asUInt & ~Cat(othersEntryOldestSel(0).bits, 0.U((params.numEnq).W))
    }

    val deq0SelOH = Mux(othersEntryOldestSel(0).valid, Cat(othersEntryOldestSel(0).bits, 0.U((params.numEnq).W)), subDeqSelOHVec.get(1))
    val deq0SelFp = Mux1H(deq0SelOH, fpToVfVec)
    deqSelValidVec(0) := othersEntryOldestSel(0).valid || subDeqSelValidVec.get(1)
    deqSelValidVec(1) := subDeqSelValidVec.get(0) && !(deqSelValidVec(0) && !deq0SelFp)
    deqSelOHVec(0) := deq0SelOH & canIssueMergeAllBusy(0) & Mux(!deq0SelFp, canIssueMergeAllBusy(1), ~(0.U(params.numEntries.W)))
    deqSelOHVec(1) := subDeqSelOHVec.get(0) & canIssueMergeAllBusy(1)

    finalDeqSelValidVec.zip(finalDeqSelOHVec).zip(deqSelValidVec).zip(deqSelOHVec).zipWithIndex.foreach {
      case ((((selValid, selOH), deqValid), deqOH), i) =>
      selValid := deqValid && deqOH.orR
      selOH := deqOH
    }
  } else {
    enqEntryOldestSel := NewAgeDetector(numEntries = params.numEnq,
      enq = VecInit(s0_doEnqSelValidVec),
      canIssue = VecInit(deqCanIssue.map(_(params.numEnq - 1, 0)))
    )

    if (params.isAllComp || params.isAllSimp) {
      othersEntryOldestSel := AgeDetector(numEntries = params.numEntries - params.numEnq,
        enq = othersEntryEnqSelVec.get,
        canIssue = VecInit(deqCanIssue.map(_(params.numEntries - 1, params.numEnq)))
      )

      deqSelValidVec.zip(deqSelOHVec).zipWithIndex.foreach { case ((selValid, selOH), i) =>
        if (params.exuBlockParams(i).fuConfigs.contains(FuConfig.FakeHystaCfg)) {
          selValid := false.B
          selOH := 0.U.asTypeOf(selOH)
        } else {
          selValid := othersEntryOldestSel(i).valid || enqEntryOldestSel(i).valid
          selOH := Cat(othersEntryOldestSel(i).bits, Fill(params.numEnq, !othersEntryOldestSel(i).valid) & enqEntryOldestSel(i).bits)
        }
      }
    } else {
      othersEntryOldestSel := DontCare

      deqCanIssue.zipWithIndex.foreach { case (req, i) =>
        simpAgeDetectRequest.get(i) := req(params.numEnq + params.numSimp - 1, params.numEnq)
      }
      simpAgeDetectRequest.get(params.numDeq) := VecInit(requestForTrans.drop(params.numEnq).take(params.numSimp)).asUInt
      if (params.numEnq == 2) {
        simpAgeDetectRequest.get(params.numDeq + 1) := VecInit(requestForTrans.drop(params.numEnq).take(params.numSimp)).asUInt & ~simpEntryOldestSel.get(params.numDeq).bits
      }

      simpEntryOldestSel.get := AgeDetector(numEntries = params.numSimp,
        enq = simpEntryEnqSelVec.get,
        canIssue = simpAgeDetectRequest.get
      )

      compEntryOldestSel.get := AgeDetector(numEntries = params.numComp,
        enq = compEntryEnqSelVec.get,
        canIssue = VecInit(deqCanIssue.map(_(params.numEntries - 1, params.numEnq + params.numSimp)))
      )

      deqSelValidVec.zip(deqSelOHVec).zipWithIndex.foreach { case ((selValid, selOH), i) =>
        if (params.exuBlockParams(i).fuConfigs.contains(FuConfig.FakeHystaCfg)) {
          selValid := false.B
          selOH := 0.U.asTypeOf(selOH)
        } else {
          selValid := compEntryOldestSel.get(i).valid || simpEntryOldestSel.get(i).valid || enqEntryOldestSel(i).valid
          selOH := Cat(
            compEntryOldestSel.get(i).bits,
            Fill(params.numSimp, !compEntryOldestSel.get(i).valid) & simpEntryOldestSel.get(i).bits,
            Fill(params.numEnq, !compEntryOldestSel.get(i).valid && !simpEntryOldestSel.get(i).valid) & enqEntryOldestSel(i).bits
          )
        }
      }
    }

    finalDeqSelValidVec.zip(finalDeqSelOHVec).zip(deqSelValidVec).zip(deqSelOHVec).zipWithIndex.foreach {
      case ((((selValid, selOH), deqValid), deqOH), i) =>
      selValid := deqValid
      selOH := deqOH
    }
  }

  val toBusyTableDeqResp = Wire(Vec(params.numDeq, ValidIO(new IssueQueueDeqRespBundle)))

  toBusyTableDeqResp.zipWithIndex.foreach { case (deqResp, i) =>
    deqResp.valid := deqBeforeDly(i).valid
    deqResp.bits.resp   := RespType.success
    deqResp.bits.robIdx := DontCare
    deqResp.bits.sqIdx.foreach(_ := DontCare)
    deqResp.bits.lqIdx.foreach(_ := DontCare)
    deqResp.bits.fuType := deqBeforeDly(i).bits.common.fuType
    deqResp.bits.uopIdx.foreach(_ := DontCare)
  }

  //fuBusyTable
  fuBusyTableWrite.zip(fuBusyTableRead).zipWithIndex.foreach { case ((busyTableWrite: Option[FuBusyTableWrite], busyTableRead: Option[FuBusyTableRead]), i) =>
    if(busyTableWrite.nonEmpty) {
      val btwr = busyTableWrite.get
      val btrd = busyTableRead.get
      btwr.io.in.deqResp := toBusyTableDeqResp(i)
      btwr.io.in.og0Resp := io.og0Resp(i)
      btwr.io.in.og1Resp := io.og1Resp(i)
      btrd.io.in.fuBusyTable := btwr.io.out.fuBusyTable
      btrd.io.in.fuTypeRegVec := fuTypeVec
      fuBusyTableMask(i) := btrd.io.out.fuBusyTableMask
    }
    else {
      fuBusyTableMask(i) := 0.U(params.numEntries.W)
    }
  }

  //wbfuBusyTable write
  intWbBusyTableWrite.zip(intWbBusyTableOut).zip(intDeqRespSetOut).zipWithIndex.foreach { case (((busyTableWrite: Option[FuBusyTableWrite], busyTable: Option[UInt]), deqResp), i) =>
    if(busyTableWrite.nonEmpty) {
      val btwr = busyTableWrite.get
      val bt = busyTable.get
      val dq = deqResp.get
      btwr.io.in.deqResp := toBusyTableDeqResp(i)
      btwr.io.in.deqResp.valid := toBusyTableDeqResp(i).valid && deqBeforeDly(i).bits.common.rfWen.getOrElse(false.B)
      btwr.io.in.og0Resp := io.og0Resp(i)
      btwr.io.in.og1Resp := io.og1Resp(i)
      bt := btwr.io.out.fuBusyTable
      dq := btwr.io.out.deqRespSet
    }
  }

  vfWbBusyTableWrite.zip(vfWbBusyTableOut).zip(vfDeqRespSetOut).zipWithIndex.foreach { case (((busyTableWrite: Option[FuBusyTableWrite], busyTable: Option[UInt]), deqResp), i) =>
    if (busyTableWrite.nonEmpty) {
      val btwr = busyTableWrite.get
      val bt = busyTable.get
      val dq = deqResp.get
      btwr.io.in.deqResp := toBusyTableDeqResp(i)
      btwr.io.in.deqResp.valid := toBusyTableDeqResp(i).valid && deqBeforeDly(i).bits.common.vecWen.getOrElse(false.B)
      btwr.io.in.og0Resp := io.og0Resp(i)
      btwr.io.in.og1Resp := io.og1Resp(i)
      bt := btwr.io.out.fuBusyTable
      dq := btwr.io.out.deqRespSet
    }
  }

  v0WbBusyTableWrite.zip(v0WbBusyTableOut).zip(v0DeqRespSetOut).zipWithIndex.foreach { case (((busyTableWrite: Option[FuBusyTableWrite], busyTable: Option[UInt]), deqResp), i) =>
    if (busyTableWrite.nonEmpty) {
      val btwr = busyTableWrite.get
      val bt = busyTable.get
      val dq = deqResp.get
      btwr.io.in.deqResp := toBusyTableDeqResp(i)
      btwr.io.in.deqResp.valid := toBusyTableDeqResp(i).valid && deqBeforeDly(i).bits.common.v0Wen.getOrElse(false.B)
      btwr.io.in.og0Resp := io.og0Resp(i)
      btwr.io.in.og1Resp := io.og1Resp(i)
      bt := btwr.io.out.fuBusyTable
      dq := btwr.io.out.deqRespSet
    }
  }

  vlWbBusyTableWrite.zip(vlWbBusyTableOut).zip(vlDeqRespSetOut).zipWithIndex.foreach { case (((busyTableWrite: Option[FuBusyTableWrite], busyTable: Option[UInt]), deqResp), i) =>
    if (busyTableWrite.nonEmpty) {
      val btwr = busyTableWrite.get
      val bt = busyTable.get
      val dq = deqResp.get
      btwr.io.in.deqResp := toBusyTableDeqResp(i)
      btwr.io.in.deqResp.valid := toBusyTableDeqResp(i).valid && deqBeforeDly(i).bits.common.vlWen.getOrElse(false.B)
      btwr.io.in.og0Resp := io.og0Resp(i)
      btwr.io.in.og1Resp := io.og1Resp(i)
      bt := btwr.io.out.fuBusyTable
      dq := btwr.io.out.deqRespSet
    }
  }

  //wbfuBusyTable read
  intWbBusyTableRead.zip(intWbBusyTableIn).zipWithIndex.foreach { case ((busyTableRead: Option[FuBusyTableRead], busyTable: Option[UInt]), i) =>
    if(busyTableRead.nonEmpty) {
      val btrd = busyTableRead.get
      val bt = busyTable.get
      btrd.io.in.fuBusyTable := bt
      btrd.io.in.fuTypeRegVec := fuTypeVec
      intWbBusyTableMask(i) := btrd.io.out.fuBusyTableMask
    }
    else {
      intWbBusyTableMask(i) := 0.U(params.numEntries.W)
    }
  }

  vfWbBusyTableRead.zip(vfWbBusyTableIn).zipWithIndex.foreach { case ((busyTableRead: Option[FuBusyTableRead], busyTable: Option[UInt]), i) =>
    if (busyTableRead.nonEmpty) {
      val btrd = busyTableRead.get
      val bt = busyTable.get
      btrd.io.in.fuBusyTable := bt
      btrd.io.in.fuTypeRegVec := fuTypeVec
      vfWbBusyTableMask(i) := btrd.io.out.fuBusyTableMask
    }
    else {
      vfWbBusyTableMask(i) := 0.U(params.numEntries.W)
    }
  }

  v0WbBusyTableRead.zip(v0WbBusyTableIn).zipWithIndex.foreach { case ((busyTableRead: Option[FuBusyTableRead], busyTable: Option[UInt]), i) =>
    if (busyTableRead.nonEmpty) {
      val btrd = busyTableRead.get
      val bt = busyTable.get
      btrd.io.in.fuBusyTable := bt
      btrd.io.in.fuTypeRegVec := fuTypeVec
      v0WbBusyTableMask(i) := btrd.io.out.fuBusyTableMask
    }
    else {
      v0WbBusyTableMask(i) := 0.U(params.numEntries.W)
    }
  }

  vlWbBusyTableRead.zip(vlWbBusyTableIn).zipWithIndex.foreach { case ((busyTableRead: Option[FuBusyTableRead], busyTable: Option[UInt]), i) =>
    if (busyTableRead.nonEmpty) {
      val btrd = busyTableRead.get
      val bt = busyTable.get
      btrd.io.in.fuBusyTable := bt
      btrd.io.in.fuTypeRegVec := fuTypeVec
      vlWbBusyTableMask(i) := btrd.io.out.fuBusyTableMask
    }
    else {
      vlWbBusyTableMask(i) := 0.U(params.numEntries.W)
    }
  }

  wakeUpQueues.zipWithIndex.foreach { case (wakeUpQueueOption, i) =>
    wakeUpQueueOption.foreach {
      wakeUpQueue =>
        val flush = Wire(new WakeupQueueFlush)
        flush.redirect := io.flush
        flush.ldCancel := io.ldCancel
        flush.og0Fail := io.og0Resp(i).valid && RespType.isBlocked(io.og0Resp(i).bits.resp)
        flush.og1Fail := io.og1Resp(i).valid && RespType.isBlocked(io.og1Resp(i).bits.resp)
        wakeUpQueue.io.flush := flush
        wakeUpQueue.io.enq.valid := deqBeforeDly(i).valid
        wakeUpQueue.io.enq.bits.uop :<= deqBeforeDly(i).bits.common
        wakeUpQueue.io.enq.bits.uop.pdestCopy.foreach(_ := 0.U)
        wakeUpQueue.io.enq.bits.lat := getDeqLat(i, deqBeforeDly(i).bits.common.fuType)
    }
  }

  deqBeforeDly.zipWithIndex.foreach { case (deq, i) =>
    deq.valid                := finalDeqSelValidVec(i) && !cancelDeqVec(i)
    deq.bits.addrOH          := finalDeqSelOHVec(i)
    deq.bits.common.isFirstIssue := deqFirstIssueVec(i)
    deq.bits.common.iqIdx    := OHToUInt(finalDeqSelOHVec(i))
    deq.bits.common.fuType   := deqEntryVec(i).bits.status.fuType
    deq.bits.common.fuOpType := deqEntryVec(i).bits.payload.fuOpType
    deq.bits.common.rfWen.foreach(_ := deqEntryVec(i).bits.payload.rfWen)
    deq.bits.common.fpWen.foreach(_ := deqEntryVec(i).bits.payload.fpWen)
    deq.bits.common.vecWen.foreach(_ := deqEntryVec(i).bits.payload.vecWen)
    deq.bits.common.v0Wen.foreach(_ := deqEntryVec(i).bits.payload.v0Wen)
    deq.bits.common.vfWenH.foreach(_ := false.B)
    deq.bits.common.vfWenL.foreach(_ := false.B)
    deq.bits.common.v0WenH.foreach(_ := false.B)
    deq.bits.common.v0WenL.foreach(_ := false.B)
    deq.bits.common.vlWen.foreach(_ := deqEntryVec(i).bits.payload.vlWen)
    deq.bits.common.flushPipe.foreach(_ := deqEntryVec(i).bits.payload.flushPipe)
    deq.bits.common.pdest := deqEntryVec(i).bits.payload.pdest
    deq.bits.common.robIdx := deqEntryVec(i).bits.status.robIdx

    require(deq.bits.common.dataSources.size <= finalDataSources(i).size)
    deq.bits.common.dataSources.zip(finalDataSources(i)).foreach { case (sink, source) => sink := source}
    deq.bits.common.l1ExuOH.foreach(_.zip(finalWakeUpL1ExuOH.get(i)).foreach { case (sink, source) => sink := source})
    deq.bits.common.srcTimer.foreach(_ := DontCare)
    deq.bits.common.loadDependency.foreach(_.zip(finalLoadDependency(i)).foreach { case (sink, source) => sink := source})
    deq.bits.common.src := DontCare
    deq.bits.common.preDecode.foreach(_ := deqEntryVec(i).bits.payload.preDecodeInfo)

    deq.bits.rf.zip(deqEntryVec(i).bits.status.srcStatus.map(_.psrc)).zip(deqEntryVec(i).bits.status.srcStatus.map(_.srcType)).foreach { case ((rf, psrc), srcType) =>
      // psrc in status array can be pregIdx of IntRegFile or VfRegFile
      rf.foreach(_.addr := psrc)
      rf.foreach(_.srcType := srcType)
    }
    deq.bits.srcType.zip(deqEntryVec(i).bits.status.srcStatus.map(_.srcType)).foreach { case (sink, source) =>
      sink := source
    }
    deq.bits.immType := deqEntryVec(i).bits.payload.selImm
    deq.bits.common.imm := deqEntryVec(i).bits.imm.getOrElse(0.U)
    deq.bits.rcIdx.foreach(_ := deqEntryVec(i).bits.status.srcStatus.map(_.regCacheIdx.get))

    deq.bits.common.perfDebugInfo := deqEntryVec(i).bits.payload.debugInfo
    deq.bits.common.perfDebugInfo.selectTime := GTimer()
    deq.bits.common.perfDebugInfo.issueTime := GTimer() + 1.U
    
    deq.bits.common.rfForAssert.foreach(_ := DontCare)
  }

  val deqDelay = Reg(params.genIssueValidBundle)
  deqDelay.zip(deqBeforeDly).foreach { case (deqDly, deq) =>
    deqDly.valid := deq.valid
    when(validVec.asUInt.orR) {
      deqDly.bits := deq.bits
    }
    // deqBeforeDly.ready is always true
    deq.ready := true.B
  }
  io.deqDelay.zip(deqDelay).foreach { case (sink, source) =>
    sink.valid := source.valid
    sink.bits := source.bits
  }
  if(backendParams.debugEn) {
    dontTouch(deqDelay)
    dontTouch(io.deqDelay)
    dontTouch(deqBeforeDly)
  }
  io.wakeupToIQ.zipWithIndex.foreach { case (wakeup, i) =>
    if (wakeUpQueues(i).nonEmpty) {
      wakeup.valid := wakeUpQueues(i).get.io.deq.valid
      wakeup.bits.fromExuInput(wakeUpQueues(i).get.io.deq.bits)
      wakeup.bits.loadDependency := wakeUpQueues(i).get.io.deq.bits.loadDependency.getOrElse(0.U.asTypeOf(wakeup.bits.loadDependency))
      wakeup.bits.is0Lat := getDeqLat(i, wakeUpQueues(i).get.io.deq.bits.fuType) === 0.U
      wakeup.bits.rcDest.foreach(_ := io.replaceRCIdx.get(i))
    } else {
      wakeup.valid := false.B
      wakeup.bits := 0.U.asTypeOf(wakeup.bits)
    }
    if (wakeUpQueues(i).nonEmpty) {
      wakeup.bits.rfWen  := (if (wakeUpQueues(i).get.io.deq.bits.rfWen .nonEmpty) wakeUpQueues(i).get.io.deq.valid && wakeUpQueues(i).get.io.deq.bits.rfWen .get else false.B)
      wakeup.bits.fpWen  := (if (wakeUpQueues(i).get.io.deq.bits.fpWen .nonEmpty) wakeUpQueues(i).get.io.deq.valid && wakeUpQueues(i).get.io.deq.bits.fpWen .get else false.B)
      wakeup.bits.vecWen := (if (wakeUpQueues(i).get.io.deq.bits.vecWen.nonEmpty) wakeUpQueues(i).get.io.deq.valid && wakeUpQueues(i).get.io.deq.bits.vecWen.get else false.B)
      wakeup.bits.v0Wen  := (if (wakeUpQueues(i).get.io.deq.bits.v0Wen .nonEmpty) wakeUpQueues(i).get.io.deq.valid && wakeUpQueues(i).get.io.deq.bits.v0Wen.get else false.B)
      wakeup.bits.vlWen  := (if (wakeUpQueues(i).get.io.deq.bits.vlWen .nonEmpty) wakeUpQueues(i).get.io.deq.valid && wakeUpQueues(i).get.io.deq.bits.vlWen.get else false.B)
    }

    if(wakeUpQueues(i).nonEmpty && wakeup.bits.pdestCopy.nonEmpty){
      wakeup.bits.pdestCopy.get := wakeUpQueues(i).get.io.deq.bits.pdestCopy.get
    }
    if (wakeUpQueues(i).nonEmpty && wakeup.bits.rfWenCopy.nonEmpty) {
      wakeup.bits.rfWenCopy.get := wakeUpQueues(i).get.io.deq.bits.rfWenCopy.get
    }
    if (wakeUpQueues(i).nonEmpty && wakeup.bits.fpWenCopy.nonEmpty) {
      wakeup.bits.fpWenCopy.get := wakeUpQueues(i).get.io.deq.bits.fpWenCopy.get
    }
    if (wakeUpQueues(i).nonEmpty && wakeup.bits.vecWenCopy.nonEmpty) {
      wakeup.bits.vecWenCopy.get := wakeUpQueues(i).get.io.deq.bits.vecWenCopy.get
    }
    if (wakeUpQueues(i).nonEmpty && wakeup.bits.v0WenCopy.nonEmpty) {
      wakeup.bits.v0WenCopy.get := wakeUpQueues(i).get.io.deq.bits.v0WenCopy.get
    }
    if (wakeUpQueues(i).nonEmpty && wakeup.bits.vlWenCopy.nonEmpty) {
      wakeup.bits.vlWenCopy.get := wakeUpQueues(i).get.io.deq.bits.vlWenCopy.get
    }
    if (wakeUpQueues(i).nonEmpty && wakeup.bits.loadDependencyCopy.nonEmpty) {
      wakeup.bits.loadDependencyCopy.get := wakeUpQueues(i).get.io.deq.bits.loadDependencyCopy.get
    }
  }

  // Todo: better counter implementation
  private val enqHasValid = validVec.take(params.numEnq).reduce(_ | _)
  private val enqHasIssued = validVec.zip(issuedVec).take(params.numEnq).map(x => x._1 & x._2).reduce(_ | _)
  private val enqEntryValidCnt = PopCount(validVec.take(params.numEnq))
  private val othersValidCnt = PopCount(validVec.drop(params.numEnq))
  private val enqEntryValidCntDeq0 = PopCount(
    validVec.take(params.numEnq).zip(deqCanAcceptVec(0).take(params.numEnq)).map { case (a, b) => a && b }
  )
  private val othersValidCntDeq0 = PopCount(
    validVec.drop(params.numEnq).zip(deqCanAcceptVec(0).drop(params.numEnq)).map { case (a, b) => a && b }
  )
  private val enqEntryValidCntDeq1 = PopCount(
    validVec.take(params.numEnq).zip(deqCanAcceptVec.last.take(params.numEnq)).map { case (a, b) => a && b }
  )
  private val othersValidCntDeq1 = PopCount(
    validVec.drop(params.numEnq).zip(deqCanAcceptVec.last.drop(params.numEnq)).map { case (a, b) => a && b }
  )
  protected val deqCanAcceptVecEnq: Seq[IndexedSeq[Bool]] = deqFuCfgs.map { fuCfgs: Seq[FuConfig] =>
    io.enq.map(_.bits.fuType).map(fuType =>
      FuType.FuTypeOrR(fuType, fuCfgs.map(_.fuType)))
  }
  protected val enqValidCntDeq0 = PopCount(io.enq.map(_.fire).zip(deqCanAcceptVecEnq(0)).map { case (a, b) => a && b })
  protected val enqValidCntDeq1 = PopCount(io.enq.map(_.fire).zip(deqCanAcceptVecEnq.last).map { case (a, b) => a && b })
  io.validCntDeqVec.head := RegNext(enqEntryValidCntDeq0 +& othersValidCntDeq0 - io.deqDelay.head.fire) // validCntDeqVec(0)
  io.validCntDeqVec.last := RegNext(enqEntryValidCntDeq1 +& othersValidCntDeq1 - io.deqDelay.last.fire) // validCntDeqVec(1)
  io.status.leftVec(0) := validVec.drop(params.numEnq).reduce(_ & _)
  for (i <- 0 until params.numEnq) {
    io.status.leftVec(i + 1) := othersValidCnt === (params.numEntries - params.numEnq - (i + 1)).U
  }
  private val othersLeftOneCaseVec = Wire(Vec(params.numEntries - params.numEnq, UInt((params.numEntries - params.numEnq).W)))
  othersLeftOneCaseVec.zipWithIndex.foreach { case (leftone, i) =>
    leftone := ~(1.U((params.numEntries - params.numEnq).W) << i)
  }
  private val othersLeftOne = othersLeftOneCaseVec.map(_ === VecInit(validVec.drop(params.numEnq)).asUInt).reduce(_ | _)
  private val othersCanotIn = Wire(Bool())
  othersCanotIn := othersLeftOne || validVec.drop(params.numEnq).reduce(_ & _)
  // if has simp Entry, othersCanotIn will be simpCanotIn
  if (params.numSimp > 0) {
    val simpLeftOneCaseVec = Wire(Vec(params.numSimp, UInt((params.numSimp).W)))
    simpLeftOneCaseVec.zipWithIndex.foreach { case (leftone, i) =>
      leftone := ~(1.U((params.numSimp).W) << i)
    }
    val simpLeftOne = simpLeftOneCaseVec.map(_ === VecInit(validVec.drop(params.numEnq).take(params.numSimp)).asUInt).reduce(_ | _)
    val simpCanotIn = simpLeftOne || validVec.drop(params.numEnq).take(params.numSimp).reduce(_ & _)
    othersCanotIn := simpCanotIn
  }
  io.enq.foreach(_.ready := (!othersCanotIn || !enqHasValid) && !enqHasIssued)
  io.status.empty := !Cat(validVec).orR
  io.status.full := othersCanotIn
  io.status.validCnt := PopCount(validVec)

  protected def getDeqLat(deqPortIdx: Int, fuType: UInt) : UInt = {
    val res = WireDefault(0.U(4.W))
    wakeupFuLatencyMaps(deqPortIdx) map { case (k, v) => 
      when(k.id.U === fuType){
        res := v.U
      }
    }
    res
  }

  // issue perf counter
  // enq count
  XSPerfAccumulate("enq_valid_cnt", PopCount(io.enq.map(_.fire)))
  XSPerfAccumulate("enq_fire_cnt", PopCount(io.enq.map(_.fire)))
  XSPerfAccumulate("enq_alu_fire_cnt", PopCount(io.enq.map { case enq => enq.fire && FuType.isAlu(enq.bits.fuType) }))
  XSPerfAccumulate("enq_brh_fire_cnt", PopCount(io.enq.map { case enq => enq.fire && FuType.isBrh(enq.bits.fuType) }))
  XSPerfAccumulate("deqDelay0_fire_cnt", PopCount(io.deqDelay.head.fire))
  XSPerfAccumulate("deqDelay1_fire_cnt", PopCount(io.deqDelay.last.fire))
  // valid count
  XSPerfHistogram("enq_entry_valid_cnt", enqEntryValidCnt, true.B, 0, params.numEnq + 1)
  XSPerfHistogram("other_entry_valid_cnt", othersValidCnt, true.B, 0, params.numEntries - params.numEnq + 1)
  XSPerfHistogram("valid_cnt", PopCount(validVec), true.B, 0, params.numEntries + 1)
  // only split when more than 1 func type
  if (params.getFuCfgs.size > 0) {
    for (t <- FuType.functionNameMap.keys) {
      val fuName = FuType.functionNameMap(t)
      if (params.getFuCfgs.map(_.fuType == t).reduce(_ | _)) {
        XSPerfHistogram(s"valid_cnt_hist_futype_${fuName}", PopCount(validVec.zip(fuTypeVec).map { case (v, fu) => v && fu === t.id.U }), true.B, 0, params.numEntries, 1)
      }
    }
  }
  // ready instr count
  private val readyEntriesCnt = PopCount(validVec.zip(canIssueVec).map(x => x._1 && x._2))
  XSPerfHistogram("ready_cnt", readyEntriesCnt, true.B, 0, params.numEntries + 1)
  // only split when more than 1 func type
  if (params.getFuCfgs.size > 0) {
    for (t <- FuType.functionNameMap.keys) {
      val fuName = FuType.functionNameMap(t)
      if (params.getFuCfgs.map(_.fuType == t).reduce(_ | _)) {
        XSPerfHistogram(s"ready_cnt_hist_futype_${fuName}", PopCount(validVec.zip(canIssueVec).zip(fuTypeVec).map { case ((v, c), fu) => v && c && fu === t.id.U }), true.B, 0, params.numEntries, 1)
      }
    }
  }

  // deq instr count
  XSPerfAccumulate("issue_instr_pre_count", PopCount(deqBeforeDly.map(_.valid)))
  XSPerfHistogram("issue_instr_pre_count_hist", PopCount(deqBeforeDly.map(_.valid)), true.B, 0, params.numDeq + 1, 1)
  XSPerfAccumulate("issue_instr_count", PopCount(io.deqDelay.map(_.valid)))
  XSPerfHistogram("issue_instr_count_hist", PopCount(io.deqDelay.map(_.valid)), true.B, 0, params.numDeq + 1, 1)

  // deq instr data source count
  XSPerfAccumulate("issue_datasource_reg", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.reg && !SrcType.isNotReg(deq.bits.srcType(j)) })
  }.reduce(_ +& _))
  XSPerfAccumulate("issue_datasource_bypass", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.bypass && !SrcType.isNotReg(deq.bits.srcType(j)) })
  }.reduce(_ +& _))
  XSPerfAccumulate("issue_datasource_forward", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.forward && !SrcType.isNotReg(deq.bits.srcType(j)) })
  }.reduce(_ +& _))
  XSPerfAccumulate("issue_datasource_noreg", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && SrcType.isNotReg(deq.bits.srcType(j)) })
  }.reduce(_ +& _))

  XSPerfHistogram("issue_datasource_reg_hist", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.reg && !SrcType.isNotReg(deq.bits.srcType(j)) })
  }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
  XSPerfHistogram("issue_datasource_bypass_hist", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.bypass && !SrcType.isNotReg(deq.bits.srcType(j)) })
  }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
  XSPerfHistogram("issue_datasource_forward_hist", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.forward && !SrcType.isNotReg(deq.bits.srcType(j)) })
  }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
  XSPerfHistogram("issue_datasource_noreg_hist", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && SrcType.isNotReg(deq.bits.srcType(j)) })
  }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)

  // deq instr data source count for each futype
  for (t <- FuType.functionNameMap.keys) {
    val fuName = FuType.functionNameMap(t)
    if (params.getFuCfgs.map(_.fuType == t).reduce(_ | _)) {
      XSPerfAccumulate(s"issue_datasource_reg_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.reg && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.id.U })
      }.reduce(_ +& _))
      XSPerfAccumulate(s"issue_datasource_bypass_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.bypass && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.id.U })
      }.reduce(_ +& _))
      XSPerfAccumulate(s"issue_datasource_forward_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.forward && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.id.U })
      }.reduce(_ +& _))
      XSPerfAccumulate(s"issue_datasource_noreg_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.id.U })
      }.reduce(_ +& _))

      XSPerfHistogram(s"issue_datasource_reg_hist_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.reg && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.id.U })
      }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
      XSPerfHistogram(s"issue_datasource_bypass_hist_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.bypass && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.id.U })
      }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
      XSPerfHistogram(s"issue_datasource_forward_hist_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.forward && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.id.U })
      }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
      XSPerfHistogram(s"issue_datasource_noreg_hist_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.id.U })
      }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
    }
  }
}

class IssueQueueLoadBundle(implicit p: Parameters) extends XSBundle {
  val fastMatch = UInt(backendParams.LduCnt.W)
  val fastImm = UInt(12.W)
}

class IssueQueueIntIO()(implicit p: Parameters, params: IssueBlockParams) extends IssueQueueIO

class IssueQueueIntImp(override val wrapper: IssueQueue)(implicit p: Parameters, iqParams: IssueBlockParams)
  extends IssueQueueImp(wrapper)
{
  io.suggestName("none")
  override lazy val io = IO(new IssueQueueIntIO).suggestName("io")

  deqBeforeDly.zipWithIndex.foreach{ case (deq, i) => {
    deq.bits.common.pc.foreach(_ := DontCare)
    deq.bits.common.preDecode.foreach(_ := deqEntryVec(i).bits.payload.preDecodeInfo)
    deq.bits.common.ftqIdx.foreach(_ := deqEntryVec(i).bits.payload.ftqPtr)
    deq.bits.common.ftqOffset.foreach(_ := deqEntryVec(i).bits.payload.ftqOffset)
    deq.bits.common.predictInfo.foreach(x => {
      x.target := DontCare
      x.taken := deqEntryVec(i).bits.payload.pred_taken
    })
    // for std
    deq.bits.common.sqIdx.foreach(_ := deqEntryVec(i).bits.payload.sqIdx)
    // for i2f
    deq.bits.common.fpu.foreach(_ := deqEntryVec(i).bits.payload.fpu)

    deq.bits.common.vfWenH.foreach(_ := deqEntryVec(i).bits.payload.vecWen & FuType.FuTypeOrR(deqEntryVec(i).bits.payload.fuType, FuType.i2v))
    deq.bits.common.vfWenL.foreach(_ := deqEntryVec(i).bits.payload.vecWen)
    deq.bits.common.v0WenH.foreach(_ := deqEntryVec(i).bits.payload.v0Wen & FuType.FuTypeOrR(deqEntryVec(i).bits.payload.fuType, FuType.i2v))
    deq.bits.common.v0WenL.foreach(_ := deqEntryVec(i).bits.payload.v0Wen)
  }}
}

class IssueQueueVfImp(override val wrapper: IssueQueue)(implicit p: Parameters, iqParams: IssueBlockParams)
  extends IssueQueueImp(wrapper)
{
  deqBeforeDly.zipWithIndex.foreach{ case (deq, i) => {
    deq.bits.common.fpu.foreach(_ := deqEntryVec(i).bits.payload.fpu)
    deq.bits.common.vpu.foreach(_.connectSimple(deqEntryVec(i).bits.payload.vpu))
    deq.bits.common.vpu.foreach(_.vuopIdx := deqEntryVec(i).bits.payload.uopIdx)
    deq.bits.common.vpu.foreach(_.lastUop := deqEntryVec(i).bits.payload.lastUop)
    deq.bits.common.vpu.foreach(_.is_vfredosum := DontCare)
    deq.bits.common.vpu.foreach(_.is_fold := DontCare)
    deq.bits.common.vpu.foreach(_.is_reduction := DontCare)
    deq.bits.common.vpu.foreach(_.isVFCmp := false.B)
  }}
  if(iqParams.sharedVf) {
    when(deqBeforeDly(0).valid && !deqBeforeDly(0).bits.common.vpu.get.fpu.isFpToVecInst) {
      deqDelay.zip(deqBeforeDly).foreach { case (deqDly, deq) =>
        deqDly.valid := deq.valid
        when(validVec.asUInt.orR) {
          deqDly.bits := deq.bits
        }
        // deqBeforeDly.ready is always true
        deq.ready := true.B
      }
      deqDelay(1).bits := deqBeforeDly(0).bits

      deqDelay(0).valid := true.B
      deqDelay(0).bits.common.vfWenH.foreach(_ := false.B)
      deqDelay(0).bits.common.vfWenL.foreach(_ := true.B)
      deqDelay(0).bits.common.v0WenH.foreach(_ := false.B)
      deqDelay(0).bits.common.v0WenL.foreach(_ := true.B)
      wakeUpQueues(0).foreach(wq => wq.io.enq.valid := true.B)

      deqDelay(1).valid := true.B
      deqDelay(1).bits.common.vfWenH.foreach(_ := true.B)
      deqDelay(1).bits.common.vfWenL.foreach(_ := false.B)
      deqDelay(1).bits.common.v0WenH.foreach(_ := true.B)
      deqDelay(1).bits.common.v0WenL.foreach(_ := false.B)
      wakeUpQueues(1).foreach(wq => wq.io.enq.valid := false.B)
    }.otherwise {
      deqDelay.zip(deqBeforeDly).foreach { case (deqDly, deq) =>
        deqDly.valid := deq.valid
        when(validVec.asUInt.orR) {
          deqDly.bits := deq.bits
          deqDly.bits.common.vfWenL.foreach(_ := deq.bits.common.vecWen.get)
          deqDly.bits.common.v0WenL.foreach(_ := deq.bits.common.v0Wen.get)
          deqDly.bits.common.vfWenH.foreach(_ := false.B)
          deqDly.bits.common.v0WenH.foreach(_ := false.B)
        }
        // deqBeforeDly.ready is always true
        deq.ready := true.B
        wakeUpQueues(0).foreach(wq => wq.io.enq.valid := deqBeforeDly(0).valid)
        wakeUpQueues(1).foreach(wq => wq.io.enq.valid := deqBeforeDly(1).valid)
      }
    }
  } else {
    deqDelay.zip(deqBeforeDly).foreach { case (deqDly, deq) =>
      deqDly.valid := deq.valid
      when(validVec.asUInt.orR) {
        deqDly.bits := deq.bits
        deqDly.bits.common.vfWenL.foreach(_ := deq.bits.common.vecWen.get)
        deqDly.bits.common.vfWenH.foreach(_ := deq.bits.common.vecWen.get && !deq.bits.common.vpu.get.fpu.isFpToVecInst)
        deqDly.bits.common.v0WenL.foreach(_ := deq.bits.common.v0Wen.get)
        deqDly.bits.common.v0WenH.foreach(_ := deq.bits.common.v0Wen.get && !deq.bits.common.vpu.get.fpu.isFpToVecInst)
      }
      // deqBeforeDly.ready is always true
      deq.ready := true.B
    }
  }

  if(iqParams.sharedVf && (iqParams.backendParam.svaAssertEn || iqParams.backendParam.svaCoverEn)) {
    import xiangshan.backend.issue.assertion._
    val assertEn = iqParams.backendParam.svaAssertEn
    val coverEn = iqParams.backendParam.svaCoverEn
    AssertVfSplit(params, assertEn, coverEn, clock = clock, disable = reset.asDisable, deqBeforeDly(0), deqBeforeDly(1), deqDelay(0), deqDelay(1))
  }
}

class IssueQueueMemBundle(implicit p: Parameters, params: IssueBlockParams) extends Bundle {
  val feedbackIO = Flipped(Vec(params.numDeq, new MemRSFeedbackIO(params.isVecMemIQ)))

  // TODO: is still needed?
  val checkWait = new Bundle {
    val stIssuePtr = Input(new SqPtr)
    val memWaitUpdateReq = Flipped(new MemWaitUpdateReq)
  }
  val loadFastMatch = Output(Vec(params.LdExuCnt, new IssueQueueLoadBundle))

  // load wakeup
  val loadWakeUp = Input(Vec(params.LdExuCnt, ValidIO(new DynInst())))

  // vector
  val sqDeqPtr = Option.when(params.isVecMemIQ)(Input(new SqPtr))
  val lqDeqPtr = Option.when(params.isVecMemIQ)(Input(new LqPtr))
}

class IssueQueueMemIO(implicit p: Parameters, params: IssueBlockParams) extends IssueQueueIO {
  val memIO = Some(new IssueQueueMemBundle)
}

class IssueQueueMemAddrImp(override val wrapper: IssueQueue)(implicit p: Parameters, params: IssueBlockParams)
  extends IssueQueueImp(wrapper) with HasCircularQueuePtrHelper {

  require(params.StdCnt == 0 && (params.LduCnt + params.StaCnt + params.HyuCnt) > 0, "IssueQueueMemAddrImp can only be instance of MemAddr IQ, " +
    s"StdCnt: ${params.StdCnt}, LduCnt: ${params.LduCnt}, StaCnt: ${params.StaCnt}, HyuCnt: ${params.HyuCnt}")
  println(s"[IssueQueueMemAddrImp] StdCnt: ${params.StdCnt}, LduCnt: ${params.LduCnt}, StaCnt: ${params.StaCnt}, HyuCnt: ${params.HyuCnt}")

  io.suggestName("none")
  override lazy val io = IO(new IssueQueueMemIO).suggestName("io")
  private val memIO = io.memIO.get

  memIO.loadFastMatch := 0.U.asTypeOf(memIO.loadFastMatch) // TODO: is still needed?

  entries.io.fromMem.get.slowResp.zipWithIndex.foreach { case (slowResp, i) =>
    slowResp.valid       := memIO.feedbackIO(i).feedbackSlow.valid
    slowResp.bits.robIdx := memIO.feedbackIO(i).feedbackSlow.bits.robIdx
    slowResp.bits.sqIdx.foreach( _ := memIO.feedbackIO(i).feedbackSlow.bits.sqIdx)
    slowResp.bits.lqIdx.foreach( _ := memIO.feedbackIO(i).feedbackSlow.bits.lqIdx)
    slowResp.bits.resp   := Mux(memIO.feedbackIO(i).feedbackSlow.bits.hit, RespType.success, RespType.block)
    slowResp.bits.fuType := DontCare
  }

  entries.io.fromMem.get.fastResp.zipWithIndex.foreach { case (fastResp, i) =>
    fastResp.valid       := memIO.feedbackIO(i).feedbackFast.valid
    fastResp.bits.robIdx := memIO.feedbackIO(i).feedbackFast.bits.robIdx
    fastResp.bits.sqIdx.foreach( _ := memIO.feedbackIO(i).feedbackFast.bits.sqIdx)
    fastResp.bits.lqIdx.foreach( _ := memIO.feedbackIO(i).feedbackFast.bits.lqIdx)
    fastResp.bits.resp   := Mux(memIO.feedbackIO(i).feedbackFast.bits.hit, RespType.success, RespType.block)
    fastResp.bits.fuType := DontCare
  }

  // load wakeup
  val loadWakeUpIter = memIO.loadWakeUp.iterator
  io.wakeupToIQ.zip(params.exuBlockParams).zipWithIndex.foreach { case ((wakeup, param), i) =>
    if (param.hasLoadExu) {
      require(wakeUpQueues(i).isEmpty)
      val uop = loadWakeUpIter.next()

      wakeup.valid := GatedValidRegNext(uop.fire)
      wakeup.bits.rfWen  := (if (params.writeIntRf) GatedValidRegNext(uop.bits.rfWen  && uop.fire) else false.B)
      wakeup.bits.fpWen  := (if (params.writeFpRf)  GatedValidRegNext(uop.bits.fpWen  && uop.fire) else false.B)
      wakeup.bits.vecWen := (if (params.writeVecRf) GatedValidRegNext(uop.bits.vecWen && uop.fire) else false.B)
      wakeup.bits.v0Wen  := (if (params.writeV0Rf)  GatedValidRegNext(uop.bits.v0Wen  && uop.fire) else false.B)
      wakeup.bits.vlWen  := (if (params.writeVlRf)  GatedValidRegNext(uop.bits.vlWen  && uop.fire) else false.B)
      wakeup.bits.pdest  := RegEnable(uop.bits.pdest, uop.fire)
      wakeup.bits.rcDest.foreach(_ := io.replaceRCIdx.get(i))
      wakeup.bits.loadDependency.foreach(_ := 0.U) // this is correct for load only

      wakeup.bits.rfWenCopy .foreach(_.foreach(_ := (if (params.writeIntRf) GatedValidRegNext(uop.bits.rfWen  && uop.fire) else false.B)))
      wakeup.bits.fpWenCopy .foreach(_.foreach(_ := (if (params.writeFpRf)  GatedValidRegNext(uop.bits.fpWen  && uop.fire) else false.B)))
      wakeup.bits.vecWenCopy.foreach(_.foreach(_ := (if (params.writeVecRf) GatedValidRegNext(uop.bits.vecWen && uop.fire) else false.B)))
      wakeup.bits.v0WenCopy .foreach(_.foreach(_ := (if (params.writeV0Rf)  GatedValidRegNext(uop.bits.v0Wen  && uop.fire) else false.B)))
      wakeup.bits.vlWenCopy .foreach(_.foreach(_ := (if (params.writeVlRf)  GatedValidRegNext(uop.bits.vlWen  && uop.fire) else false.B)))
      wakeup.bits.pdestCopy .foreach(_.foreach(_ := RegEnable(uop.bits.pdest, uop.fire)))
      wakeup.bits.loadDependencyCopy.foreach(x => x := 0.U.asTypeOf(x)) // this is correct for load only

      wakeup.bits.is0Lat := 0.U
    }
  }
  require(!loadWakeUpIter.hasNext)

  deqBeforeDly.zipWithIndex.foreach { case (deq, i) =>
    deq.bits.common.mdpTag.foreach(_ := deqEntryVec(i).bits.payload.mdpTag)
    deq.bits.common.sqIdx.get := deqEntryVec(i).bits.payload.sqIdx
    deq.bits.common.lqIdx.get := deqEntryVec(i).bits.payload.lqIdx
    deq.bits.common.ftqIdx.foreach(_ := deqEntryVec(i).bits.payload.ftqPtr)
    deq.bits.common.ftqOffset.foreach(_ := deqEntryVec(i).bits.payload.ftqOffset)
  }
}

class IssueQueueVecMemImp(override val wrapper: IssueQueue)(implicit p: Parameters, params: IssueBlockParams)
  extends IssueQueueImp(wrapper) with HasCircularQueuePtrHelper {

//  require((params.VlduCnt + params.VstuCnt) > 0, "IssueQueueVecMemImp can only be instance of VecMem IQ")
//  println(s"[IssueQueueVecMemImp] VlduCnt: ${params.VlduCnt}, VstuCnt: ${params.VstuCnt}")
//
//  io.suggestName("none")
  override lazy val io = IO(new IssueQueueMemIO).suggestName("io")
//  private val memIO = io.memIO.get
//
//  require(params.numExu == 1, "VecMem IssueQueue has not supported more than 1 deq ports")
//
//  for (i <- entries.io.enq.indices) {
//    entries.io.enq(i).bits.status match { case enqData =>
//      enqData.vecMem.get.sqIdx := s0_enqBits(i).sqIdx
//      enqData.vecMem.get.lqIdx := s0_enqBits(i).lqIdx
//      // MemAddrIQ also handle vector insts
//      enqData.vecMem.get.numLsElem := s0_enqBits(i).numLsElem
//
//      val isFirstLoad           = s0_enqBits(i).lqIdx <= memIO.lqDeqPtr.get
//      val isVleff               = s0_enqBits(i).vpu.isVleff
//      enqData.blocked          := !isFirstLoad && isVleff
//    }
//  }
//
//  entries.io.fromMem.get.slowResp.zipWithIndex.foreach { case (slowResp, i) =>
//    slowResp.valid                 := memIO.feedbackIO(i).feedbackSlow.valid
//    slowResp.bits.robIdx           := memIO.feedbackIO(i).feedbackSlow.bits.robIdx
//    slowResp.bits.sqIdx.get        := memIO.feedbackIO(i).feedbackSlow.bits.sqIdx
//    slowResp.bits.lqIdx.get        := memIO.feedbackIO(i).feedbackSlow.bits.lqIdx
//    slowResp.bits.resp             := Mux(memIO.feedbackIO(i).feedbackSlow.bits.hit, RespType.success, RespType.block)
//    slowResp.bits.fuType           := DontCare
//    slowResp.bits.uopIdx.get       := DontCare
//  }
//
//  entries.io.fromMem.get.fastResp.zipWithIndex.foreach { case (fastResp, i) =>
//    fastResp.valid                 := memIO.feedbackIO(i).feedbackFast.valid
//    fastResp.bits.robIdx           := memIO.feedbackIO(i).feedbackFast.bits.robIdx
//    fastResp.bits.sqIdx.get        := memIO.feedbackIO(i).feedbackFast.bits.sqIdx
//    fastResp.bits.lqIdx.get        := memIO.feedbackIO(i).feedbackFast.bits.lqIdx
//    fastResp.bits.resp             := Mux(memIO.feedbackIO(i).feedbackFast.bits.hit, RespType.success, RespType.block)
//    fastResp.bits.fuType           := DontCare
//    fastResp.bits.uopIdx.get       := DontCare
//  }
//
//  entries.io.vecMemIn.get.sqDeqPtr := memIO.sqDeqPtr.get
//  entries.io.vecMemIn.get.lqDeqPtr := memIO.lqDeqPtr.get

  deqBeforeDly.zipWithIndex.foreach { case (deq, i) =>
//    deq.bits.common.sqIdx.foreach(_ := deqEntryVec(i).bits.status.vecMem.get.sqIdx)
//    deq.bits.common.lqIdx.foreach(_ := deqEntryVec(i).bits.status.vecMem.get.lqIdx)
//    deq.bits.common.numLsElem.foreach(_ := deqEntryVec(i).bits.status.vecMem.get.numLsElem)
//    if (params.isVecLduIQ) {
//      deq.bits.common.ftqIdx.get := deqEntryVec(i).bits.payload.ftqPtr
//      deq.bits.common.ftqOffset.get := deqEntryVec(i).bits.payload.ftqOffset
//    }
//    deq.bits.common.fpu.foreach(_ := deqEntryVec(i).bits.payload.fpu)
//    deq.bits.common.vpu.foreach(_.connectSimple(deqEntryVec(i).bits.payload.vpu))
//    deq.bits.common.vpu.foreach(_.vuopIdx := deqEntryVec(i).bits.payload.uopIdx)
//    deq.bits.common.vpu.foreach(_.lastUop := deqEntryVec(i).bits.payload.lastUop)
//    deq.bits.common.vpu.foreach(_.isVFCmp := false.B)
//    deq.bits.common.vfWenH.foreach(_ := deqEntryVec(i).bits.payload.vecWen)
//    deq.bits.common.vfWenL.foreach(_ := deqEntryVec(i).bits.payload.vecWen)
//    deq.bits.common.v0WenH.foreach(_ := deqEntryVec(i).bits.payload.v0Wen)
//    deq.bits.common.v0WenL.foreach(_ := deqEntryVec(i).bits.payload.v0Wen)
//    deq.bits.common.vpu.foreach(_.is_vfredosum := DontCare)
//    deq.bits.common.vpu.foreach(_.is_fold := DontCare)
//    deq.bits.common.vpu.foreach(_.is_reduction := DontCare)
    deq.bits := DontCare
  }

  io.vecLoadIssueResp.foreach(dontTouch(_))

  entries.io <> DontCare
  io <> DontCare
}
