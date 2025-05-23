package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import ujson.IndexedValue.True
import utils.MathUtils
import xs.utils.{HasCircularQueuePtrHelper}
import xs.utils.perf.{XSError}
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.datapath.DataSource
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.vector.Bundles.NumLsElem
import xiangshan.backend.rob.RobPtr
import xiangshan.mem.{LqPtr, MemWaitUpdateReq, SqPtr}
import utils.OptionWrapper
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.FtqPtr

object EntryBundles extends HasCircularQueuePtrHelper {

  class Status(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    // basic status
    val robIdx                = new RobPtr
    val fuType                = FuType()
    // src status
    val srcStatus             = Vec(params.numRegSrc, new SrcStatus)
    // issue status
    val blocked               = Bool()
    val issued                = Bool()
    val firstIssue            = Bool()
    val issueTimer            = UInt(2.W)
    val deqPortIdx            = UInt(1.W)
    // vector mem status
    val vecMem                = Option.when(params.isVecMemIQ)(new StatusVecMemPart)

    def srcReady: Bool        = {
      VecInit(srcStatus.map(_.srcState).map(SrcState.isReady)).asUInt.andR
    }

    def canIssue: Bool        = {
      srcReady && !issued && !blocked
    }

    def mergedLoadDependency: Vec[UInt] = {
      srcStatus.map(_.srcLoadDependency).reduce({
        case (l: Vec[UInt], r: Vec[UInt]) => VecInit(l.zip(r).map(x => x._1 | x._2))
      }: (Vec[UInt], Vec[UInt]) => Vec[UInt])
    }
  }

  class SrcStatus(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    val psrc                  = UInt(params.rdPregIdxWidth.W)
    val srcType               = SrcType()
    val srcState              = SrcState()
    val dataSources           = DataSource()
    val srcLoadDependency     = Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W))
    val srcWakeUpL1ExuOH      = Option.when(params.hasIQWakeUp)(ExuVec())
    // reg cache
    val useRegCache           = Option.when(params.needReadRegCache)(Bool())
    val regCacheIdx           = Option.when(params.needReadRegCache)(UInt(RegCacheIdxWidth.W))
  }

  class StatusVecMemPart(implicit p:Parameters, params: IssueBlockParams) extends Bundle {
    val sqIdx                 = new SqPtr
    val lqIdx                 = new LqPtr
    val numLsElem             = NumLsElem()
  }

  class EntryDeqRespBundle(implicit p: Parameters, val params: IssueBlockParams) extends XSBundle {
    val robIdx                = new RobPtr
    val resp                  = RespType()
    val fuType                = FuType()
    val uopIdx                = Option.when(params.isVecMemIQ)(Output(UopIdx()))
    val sqIdx                 = Option.when(params.needFeedBackSqIdx)(new SqPtr())
    val lqIdx                 = Option.when(params.needFeedBackLqIdx)(new LqPtr())
  }

  object RespType {
    def apply() = UInt(2.W)

    def isBlocked(resp: UInt) = {
      resp === block
    }

    def succeed(resp: UInt) = {
      resp === success
    }

    val block = "b00".U
    val uncertain = "b01".U
    val success = "b11".U
  }

  class EntryPayloadBundle(implicit p: Parameters) extends XSBundle {
    // passed from StaticInst
    val instr           = UInt(32.W)
    val isFetchMalAddr  = Bool()
    val trigger         = TriggerAction()
    val preDecodeInfo   = new PreDecodeInfo
    val pred_taken      = Bool()
    val crossPageIPFFix = Bool()
    val ftqPtr          = new FtqPtr
    val ftqOffset       = UInt(log2Up(PredictWidth).W)
    // passed from DecodedInst
    val srcType         = Vec(backendParams.numSrc, SrcType())
    val ldest           = UInt(LogicRegsWidth.W)
    val fuType          = FuType()
    val fuOpType        = FuOpType()
    val rfWen           = Bool()
    val fpWen           = Bool()
    val vecWen          = Bool()
    val v0Wen           = Bool()
    val vlWen           = Bool()
    val isXSTrap        = Bool()
    val waitForward     = Bool() // no speculate execution
    val blockBackward   = Bool()
    val flushPipe       = Bool() // This inst will flush all the pipe when commit, like exception but can commit
    val canRobCompress  = Bool()
    val selImm          = SelImm()
    val imm             = UInt(32.W)
    val fpu             = new FPUCtrlSignals
    val vpu             = new SimpleVPUCtrlSignals
    val vlsInstr        = Bool()
    val wfflags         = Bool()
    val isMove          = Bool()
    val uopIdx          = UopIdx()
    val isVset          = Bool()
    val firstUop        = Bool()
    val lastUop         = Bool()
    val numUops         = UInt(log2Up(MaxUopSize).W) // rob need this
    val numWB           = UInt(log2Up(MaxUopSize).W) // rob need this
    val commitType      = CommitType()
    // rename
    val srcState        = Vec(backendParams.numSrc, SrcState())
    val srcLoadDependency  = Vec(backendParams.numSrc, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))
    val psrc            = Vec(backendParams.numSrc, UInt(PhyRegIdxWidth.W))
    val pdest           = UInt(PhyRegIdxWidth.W)
    // reg cache
    val useRegCache     = Vec(backendParams.numIntRegSrc, Bool())
    val regCacheIdx     = Vec(backendParams.numIntRegSrc, UInt(RegCacheIdxWidth.W))
    val robIdx          = new RobPtr
    val instrSize       = UInt(log2Ceil(RenameWidth + 1).W)
    val dirtyFs         = Bool()
    val dirtyVs         = Bool()
    val debugInfo       = new PerfDebugInfo
    // Todo
    val lqIdx = new LqPtr
    val sqIdx = new SqPtr
    // debug module
    val singleStep      = Bool()
    // schedule
    val replayInst      = Bool()
    val debug_fuType    = OptionWrapper(backendParams.debugEn, FuType())
    val numLsElem       = NumLsElem()
    val mdpTag = UInt(MemPredPCWidth.W)
  }

  class EntryBundle(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    val status                = new Status()
    val imm                   = Option.when(params.needImm)(UInt((params.deqImmTypesMaxLen).W))
    val payload               = new EntryPayloadBundle()
  }

  class CommonInBundle(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    val flush                 = Flipped(ValidIO(new Redirect))
    val enq                   = Flipped(ValidIO(new EntryBundle))
    // wakeup
    val wakeUpFromWB: MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = Flipped(params.genWBWakeUpSinkValidBundle)
    val wakeUpFromIQ: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(params.genIQWakeUpSinkValidBundle)
    // vl
    val vlFromIntIsZero       = Input(Bool())
    val vlFromIntIsVlmax      = Input(Bool())
    val vlFromVfIsZero        = Input(Bool())
    val vlFromVfIsVlmax       = Input(Bool())
    // cancel
    val og0Cancel             = Input(ExuVec())
    val og1Cancel             = Input(ExuVec())
    val ldCancel              = Vec(backendParams.LdExuCnt, Flipped(new LoadCancelIO))
    // deq sel
    val deqSel                = Input(Bool())
    val deqPortIdxWrite       = Input(UInt(1.W))
    val issueResp             = Flipped(ValidIO(new EntryDeqRespBundle))
    // trans sel
    val transSel              = Input(Bool())
    // vector mem only
    val fromLsq = Option.when(params.isVecMemIQ)(new Bundle {
      val sqDeqPtr            = Input(new SqPtr)
      val lqDeqPtr            = Input(new LqPtr)
    })
  }

  class CommonOutBundle(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    // status
    val valid                 = Output(Bool())
    val issued                = Output(Bool())
    val canIssue              = Output(Bool())
    val fuType                = Output(FuType())
    val robIdx                = Output(new RobPtr)
    val uopIdx                = Option.when(params.isVecMemIQ)(Output(UopIdx()))
    // src
    val dataSource            = Vec(params.numRegSrc, Output(DataSource()))
    val srcWakeUpL1ExuOH      = Option.when(params.hasIQWakeUp)(Vec(params.numRegSrc, Output(ExuVec())))
    // deq
    val isFirstIssue          = Output(Bool())
    val entry                 = ValidIO(new EntryBundle)
    val cancelBypass          = Output(Bool())
    val deqPortIdxRead        = Output(UInt(1.W))
    val issueTimerRead        = Output(UInt(2.W))
    // trans
    val enqReady              = Output(Bool())
    val transEntry            = ValidIO(new EntryBundle)
    // debug
    val entryInValid          = Output(Bool())
    val entryOutDeqValid      = Output(Bool())
    val entryOutTransValid    = Output(Bool())
    val perfLdCancel          = Option.when(params.hasIQWakeUp)(Output(Vec(params.numRegSrc, Bool())))
    val perfOg0Cancel         = Option.when(params.hasIQWakeUp)(Output(Vec(params.numRegSrc, Bool())))
    val perfWakeupByWB        = Output(Vec(params.numRegSrc, Bool()))
    val perfWakeupByIQ        = Option.when(params.hasIQWakeUp)(Output(Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool()))))
  }

  class CommonWireBundle(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    val validRegNext          = Bool()
    val flushed               = Bool()
    val clear                 = Bool()
    val canIssue              = Bool()
    val enqReady              = Bool()
    val deqSuccess            = Bool()
    val srcWakeup             = Vec(params.numRegSrc, Bool())
    val srcWakeupByWB         = Vec(params.numRegSrc, Bool())
    val vlWakeupByIntWb       = Bool()
    val vlWakeupByVfWb        = Bool()
    val srcCancelVec          = Vec(params.numRegSrc, Bool())
    val srcLoadCancelVec      = Vec(params.numRegSrc, Bool())
    val srcLoadDependencyNext = Vec(params.numRegSrc, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))
  }

  def CommonWireConnect(
    common      : CommonWireBundle,
    hasIQWakeup : Option[CommonIQWakeupBundle],
    validReg    : Bool,
    status      : Status,
    commonIn    : CommonInBundle,
    isEnq       : Boolean)(implicit p: Parameters, params: IssueBlockParams) = {
    
    val hasIQWakeupGet = hasIQWakeup.getOrElse(0.U.asTypeOf(new CommonIQWakeupBundle))
    common.flushed            := status.robIdx.needFlush(commonIn.flush)
    common.deqSuccess         := (if (params.isVecMemIQ) status.issued else true.B) &&
                                  commonIn.issueResp.valid &&
                                  RespType.succeed(commonIn.issueResp.bits.resp) && !common.srcLoadCancelVec.asUInt.orR
    common.srcWakeup          := common.srcWakeupByWB.zip(hasIQWakeupGet.srcWakeupByIQ).map { case (x, y) => x || y.asUInt.orR }
    common.srcWakeupByWB      := commonIn.wakeUpFromWB.map{ bundle => 
                                    val psrcSrcTypeVec = status.srcStatus.map(_.psrc) zip status.srcStatus.map(_.srcType)
                                    if (params.numRegSrc == 5) {
                                      bundle.bits.wakeUp(psrcSrcTypeVec.take(3), bundle.valid) :+ 
                                      bundle.bits.wakeUpV0(psrcSrcTypeVec(3), bundle.valid) :+ 
                                      bundle.bits.wakeUpVl(psrcSrcTypeVec(4), bundle.valid)
                                    }
                                    else {
                                      bundle.bits.wakeUp(psrcSrcTypeVec, bundle.valid)
                                    }
                                  }.transpose.map(x => VecInit(x.toSeq).asUInt.orR).toSeq
    common.canIssue           := validReg && status.canIssue
    common.enqReady           := !validReg || commonIn.transSel
    common.clear              := common.flushed || common.deqSuccess || commonIn.transSel
    common.srcCancelVec.zip(common.srcLoadCancelVec).zip(hasIQWakeupGet.srcWakeupByIQWithoutCancel).zipWithIndex.foreach {
      case (((srcCancel, srcLoadCancel), wakeUpByIQVec), srcIdx) => {
        val ldTransCancel = if(params.hasIQWakeUp) Mux1H(wakeUpByIQVec, hasIQWakeupGet.wakeupLoadDependencyByIQVec.map(dep => LoadShouldCancel(Some(dep), commonIn.ldCancel)))
                            else false.B
        srcLoadCancel := LoadShouldCancel(Some(status.srcStatus(srcIdx).srcLoadDependency), commonIn.ldCancel)
        srcCancel := srcLoadCancel || ldTransCancel
      }
    }
    common.srcLoadDependencyNext.zip(status.srcStatus.map(_.srcLoadDependency)).foreach {
      case (ldsNext, lds) =>
        ldsNext.zip(lds).foreach{ case (ldNext, ld) => ldNext := ld << 1 }
    }
    if(isEnq) {
      common.validRegNext := Mux(commonIn.enq.valid && common.enqReady, true.B, Mux(common.clear, false.B, validReg))
    } else {
      common.validRegNext := Mux(commonIn.enq.valid, true.B, Mux(common.clear, false.B, validReg))
    }
    if (params.numRegSrc == 5) {
      // only when numRegSrc == 5 need vl
      val wakeUpFromVl = VecInit(commonIn.wakeUpFromWB.map{bundle => 
        val psrcSrcTypeVec = status.srcStatus.map(_.psrc) zip status.srcStatus.map(_.srcType)
        bundle.bits.wakeUpVl(psrcSrcTypeVec(4), bundle.valid)
      })
      var numVecWb = params.needWakeupFromVfWBPort.size
      var numV0Wb = params.needWakeupFromV0WBPort.size
      // int wb is first bit of vlwb, which is after vfwb and v0wb
      common.vlWakeupByIntWb  := wakeUpFromVl(numVecWb + numV0Wb)
      // vf wb is second bit of wb
      common.vlWakeupByVfWb   := wakeUpFromVl(numVecWb + numV0Wb + 1)
    } else {
      common.vlWakeupByIntWb  := false.B
      common.vlWakeupByVfWb   := false.B
    }
  }

  class CommonIQWakeupBundle(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    val srcWakeupByIQ                             = Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool()))
    val srcWakeupByIQWithoutCancel                = Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool()))
    val srcWakeupByIQButCancel                    = Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool()))
    val srcWakeupL1ExuOH                          = Vec(params.numRegSrc, ExuVec())
    val wakeupLoadDependencyByIQVec               = Vec(params.numWakeupFromIQ, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))
    val shiftedWakeupLoadDependencyByIQVec        = Vec(params.numWakeupFromIQ, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))
    val canIssueBypass                            = Bool()
  }

  def CommonIQWakeupConnect(common: CommonWireBundle, hasIQWakeupGet: CommonIQWakeupBundle, validReg: Bool, status: Status, commonIn: CommonInBundle, isEnq: Boolean)(implicit p: Parameters, params: IssueBlockParams) = {
    val wakeupVec: Seq[Seq[Bool]] = commonIn.wakeUpFromIQ.map{(bundle: ValidIO[IssueQueueIQWakeUpBundle]) =>
      val psrcSrcTypeVec = status.srcStatus.map(_.psrc) zip status.srcStatus.map(_.srcType)
      if (params.numRegSrc == 5) {
        bundle.bits.wakeUpFromIQ(psrcSrcTypeVec.take(3)) :+ 
        bundle.bits.wakeUpV0FromIQ(psrcSrcTypeVec(3)) :+ 
        bundle.bits.wakeUpVlFromIQ(psrcSrcTypeVec(4))
      }
      else
        bundle.bits.wakeUpFromIQ(psrcSrcTypeVec)
    }.toSeq.transpose
    val cancelSel = params.wakeUpSourceExuIdx.zip(commonIn.wakeUpFromIQ).map { case (x, y) => commonIn.og0Cancel(x) && y.bits.is0Lat }

    hasIQWakeupGet.srcWakeupByIQ                    := wakeupVec.map(x => VecInit(x.zip(cancelSel).map { case (wakeup, cancel) => wakeup && !cancel }))
    hasIQWakeupGet.srcWakeupByIQButCancel           := wakeupVec.map(x => VecInit(x.zip(cancelSel).map { case (wakeup, cancel) => wakeup && cancel }))
    hasIQWakeupGet.srcWakeupByIQWithoutCancel       := wakeupVec.map(x => VecInit(x))
    hasIQWakeupGet.wakeupLoadDependencyByIQVec      := commonIn.wakeUpFromIQ.map(_.bits.loadDependency).toSeq
    hasIQWakeupGet.srcWakeupL1ExuOH.zip(status.srcStatus.map(_.srcWakeUpL1ExuOH.get)).foreach {
      case (exuOH, regExuOH) =>
        exuOH                                       := 0.U.asTypeOf(exuOH)
        params.wakeUpSourceExuIdx.foreach(x => exuOH(x) := regExuOH(x))
    }
    hasIQWakeupGet.canIssueBypass                   := validReg && !status.issued && !status.blocked &&
      VecInit(status.srcStatus.map(_.srcState).zip(hasIQWakeupGet.srcWakeupByIQWithoutCancel).zipWithIndex.map { case ((state, wakeupVec), srcIdx) =>
        wakeupVec.asUInt.orR | state
      }).asUInt.andR
  }


  def ShiftLoadDependency(hasIQWakeupGet: CommonIQWakeupBundle)(implicit p: Parameters, params: IssueBlockParams) = {
    hasIQWakeupGet.shiftedWakeupLoadDependencyByIQVec
      .zip(hasIQWakeupGet.wakeupLoadDependencyByIQVec)
      .zip(params.wakeUpInExuSources.map(_.name)).foreach {
      case ((deps, originalDeps), name) => deps.zip(originalDeps).zipWithIndex.foreach {
        case ((dep, originalDep), deqPortIdx) =>
          if (params.backendParam.getLdExuIdx(params.backendParam.allExuParams.find(_.name == name).get) == deqPortIdx)
            dep := 1.U
          else
            dep := originalDep << 1
      }
    }
  }

  def wakeUpByVf(OH: Vec[Bool])(implicit p: Parameters): Bool = {
    val allExuParams = p(XSCoreParamsKey).backendParams.allExuParams
    OH.zip(allExuParams).map{case (oh,e) =>
      if (e.isVfExeUnit) oh else false.B
    }.reduce(_ || _)
  }

  def EntryRegCommonConnect(common: CommonWireBundle, hasIQWakeup: Option[CommonIQWakeupBundle], validReg: Bool, entryUpdate: EntryBundle, entryReg: EntryBundle, status: Status, commonIn: CommonInBundle, isEnq: Boolean)(implicit p: Parameters, params: IssueBlockParams) = {
    val hasIQWakeupGet                                 = hasIQWakeup.getOrElse(0.U.asTypeOf(new CommonIQWakeupBundle))
    val cancelByLd                                     = common.srcCancelVec.asUInt.orR
    val cancelWhenWakeup                               = VecInit(hasIQWakeupGet.srcWakeupByIQButCancel.map(_.asUInt.orR)).asUInt.orR
    val respIssueFail                                  = commonIn.issueResp.valid && RespType.isBlocked(commonIn.issueResp.bits.resp)
    entryUpdate.status.robIdx                         := status.robIdx
    entryUpdate.status.fuType                         := status.fuType
    entryUpdate.status.srcStatus.zip(status.srcStatus).zipWithIndex.foreach { case ((srcStatusNext, srcStatus), srcIdx) =>
      val cancel = common.srcCancelVec(srcIdx)
      val wakeupByIQ = hasIQWakeupGet.srcWakeupByIQ(srcIdx).asUInt.orR
      val wakeupByIQOH = hasIQWakeupGet.srcWakeupByIQ(srcIdx)
      val wakeup = common.srcWakeup(srcIdx)

      val ignoreOldVd = Wire(Bool())
      val vlWakeUpByIntWb = common.vlWakeupByIntWb
      val vlWakeUpByVfWb = common.vlWakeupByVfWb
      val isDependOldvd = entryReg.payload.vpu.isDependOldvd
      val isWritePartVd = entryReg.payload.vpu.isWritePartVd
      val vta = entryReg.payload.vpu.vta
      val vma = entryReg.payload.vpu.vma
      val vm = entryReg.payload.vpu.vm
      val vlFromIntIsZero = commonIn.vlFromIntIsZero
      val vlFromIntIsVlmax = commonIn.vlFromIntIsVlmax
      val vlFromVfIsZero = commonIn.vlFromVfIsZero
      val vlFromVfIsVlmax = commonIn.vlFromVfIsVlmax
      val vlIsVlmax = (vlFromIntIsVlmax && vlWakeUpByIntWb) || (vlFromVfIsVlmax && vlWakeUpByVfWb)
      val vlIsNonZero = (!vlFromIntIsZero && vlWakeUpByIntWb) || (!vlFromVfIsZero && vlWakeUpByVfWb)
      val ignoreTail = vlIsVlmax && (vm =/= 0.U || vma) && !isWritePartVd
      val ignoreWhole = (vm =/= 0.U || vma) && vta
      val srcIsVec = SrcType.isVp(srcStatus.srcType)
      if (params.numVfSrc > 0 && srcIdx == 2) {
        /**
          * the src store the old vd, update it when vl is write back
          * 1. when the instruction depend on old vd, we cannot set the srctype to imm, we will update the method of uop split to avoid this situation soon
          * 2. when vl = 0, we cannot set the srctype to imm because the vd keep the old value
          * 3. when vl = vlmax, we can set srctype to imm when vta is not set
          */
        ignoreOldVd := !VlduType.isFof(entryReg.payload.fuOpType) && srcIsVec && vlIsNonZero && !isDependOldvd && (ignoreTail || ignoreWhole)
      } else {
        ignoreOldVd := false.B
      }

      srcStatusNext.psrc                              := srcStatus.psrc
      srcStatusNext.srcType                           := Mux(ignoreOldVd, SrcType.no, srcStatus.srcType)
      srcStatusNext.srcState                          := Mux(cancel, false.B, wakeup | srcStatus.srcState | ignoreOldVd)
      srcStatusNext.dataSources.value                 := (if(params.inVfSchd && params.readVfRf && params.hasIQWakeUp) {
                                                            MuxCase(srcStatus.dataSources.value, Seq(
                                                              wakeupByIQ                        -> DataSource.bypass,
                                                              srcStatus.dataSources.readBypass  -> DataSource.bypass2,
                                                              srcStatus.dataSources.readBypass2 -> DataSource.reg,
                                                            ))
                                                          }
                                                          else { MuxCase(srcStatus.dataSources.value, Seq(
                                                              wakeupByIQ                         -> DataSource.bypass,
                                                              srcStatus.dataSources.readBypass   -> DataSource.reg,
                                                            ))
                                                          })
      if(params.hasIQWakeUp) {
        ExuOHGen(srcStatusNext.srcWakeUpL1ExuOH.get, wakeupByIQOH, hasIQWakeupGet.srcWakeupL1ExuOH(srcIdx))
        srcStatusNext.srcLoadDependency               := Mux(wakeupByIQ,
                                                            Mux1H(wakeupByIQOH, hasIQWakeupGet.shiftedWakeupLoadDependencyByIQVec),
                                                            common.srcLoadDependencyNext(srcIdx))
      } else {
        srcStatusNext.srcLoadDependency               := common.srcLoadDependencyNext(srcIdx)
      }

      if (params.needReadRegCache) {
        val wakeupSrcExuWriteRC = wakeupByIQOH.zip(commonIn.wakeUpFromIQ).filter(_._2.bits.params.needWriteRegCache)
        val wakeupRC    = wakeupSrcExuWriteRC.map(_._1).fold(false.B)(_ || _) && SrcType.isXp(srcStatus.srcType)
        val wakeupRCIdx = Mux1H(wakeupSrcExuWriteRC.map(_._1), wakeupSrcExuWriteRC.map(_._2.bits.rcDest.get))
        val replaceRC   = wakeupSrcExuWriteRC.map(x => x._2.bits.rfWen && x._2.bits.rcDest.get === srcStatus.regCacheIdx.get).fold(false.B)(_ || _)

        srcStatusNext.useRegCache.get                 := MuxCase(srcStatus.useRegCache.get, Seq(
                                                            cancel    -> false.B,
                                                            wakeupRC  -> true.B,
                                                            replaceRC -> false.B,
                                                          ))
        srcStatusNext.regCacheIdx.get                 := Mux(wakeupRC, wakeupRCIdx, srcStatus.regCacheIdx.get)
      }
    }
    entryUpdate.status.blocked                        := false.B
    entryUpdate.status.issued                         := MuxCase(status.issued, Seq(
      (cancelByLd || cancelWhenWakeup || respIssueFail) -> false.B,
      commonIn.deqSel                                   -> true.B,
      !status.srcReady                                  -> false.B,
    ))
    entryUpdate.status.firstIssue                     := commonIn.deqSel || status.firstIssue
    entryUpdate.status.issueTimer                     := Mux(commonIn.deqSel, 0.U, Mux(status.issued, Mux(status.issueTimer === "b11".U, status.issueTimer, status.issueTimer + 1.U), "b11".U))
    entryUpdate.status.deqPortIdx                     := Mux(commonIn.deqSel, commonIn.deqPortIdxWrite, Mux(status.issued, status.deqPortIdx, 0.U))
    entryUpdate.imm.foreach(_                         := entryReg.imm.get)
    entryUpdate.payload                               := entryReg.payload
    if (params.isVecMemIQ) {
      entryUpdate.status.vecMem.get := entryReg.status.vecMem.get
    }
  }

  def CommonOutConnect(commonOut: CommonOutBundle, common: CommonWireBundle, hasIQWakeup: Option[CommonIQWakeupBundle], validReg: Bool, entryUpdate: EntryBundle, entryReg: EntryBundle, status: Status, commonIn: CommonInBundle, isEnq: Boolean, isComp: Boolean)(implicit p: Parameters, params: IssueBlockParams) = {
    val hasIQWakeupGet                                 = hasIQWakeup.getOrElse(0.U.asTypeOf(new CommonIQWakeupBundle))
    commonOut.valid                                   := validReg
    commonOut.issued                                  := entryReg.status.issued
    commonOut.canIssue                                := (if (isComp) (common.canIssue || hasIQWakeupGet.canIssueBypass) && !common.flushed
                                                          else common.canIssue && !common.flushed)
    commonOut.fuType                                  := status.fuType
    commonOut.robIdx                                  := status.robIdx
    commonOut.dataSource.zipWithIndex.foreach{ case (dataSourceOut, srcIdx) =>
      val wakeupByIQWithoutCancel = hasIQWakeupGet.srcWakeupByIQWithoutCancel(srcIdx).asUInt.orR
      val wakeupByIQWithoutCancelOH = hasIQWakeupGet.srcWakeupByIQWithoutCancel(srcIdx)
      val isWakeupByMemIQ = wakeupByIQWithoutCancelOH.zip(commonIn.wakeUpFromIQ).filter(_._2.bits.params.isMemExeUnit).map(_._1).fold(false.B)(_ || _)
      val useRegCache = status.srcStatus(srcIdx).useRegCache.getOrElse(false.B) && status.srcStatus(srcIdx).dataSources.readReg
      dataSourceOut.value                             := (if (isComp)
                                                            if (params.inVfSchd && params.readVfRf && params.hasWakeupFromMem) {
                                                              MuxCase(status.srcStatus(srcIdx).dataSources.value, Seq(
                                                                (wakeupByIQWithoutCancel && !isWakeupByMemIQ)  -> DataSource.forward,
                                                                (wakeupByIQWithoutCancel && isWakeupByMemIQ)   -> DataSource.bypass,
                                                              ))
                                                            } else {
                                                              MuxCase(status.srcStatus(srcIdx).dataSources.value, Seq(
                                                                wakeupByIQWithoutCancel                        -> DataSource.forward,
                                                                useRegCache                                    -> DataSource.regcache,
                                                              ))
                                                            }
                                                          else {
                                                              MuxCase(status.srcStatus(srcIdx).dataSources.value, Seq(
                                                                useRegCache                                    -> DataSource.regcache,
                                                              ))
                                                          })
    }
    commonOut.isFirstIssue                            := !status.firstIssue
    commonOut.entry.valid                             := validReg
    commonOut.entry.bits                              := entryReg
    if(isEnq) {
      commonOut.entry.bits.status                     := status
    }
    commonOut.issueTimerRead                          := status.issueTimer
    commonOut.deqPortIdxRead                          := status.deqPortIdx

    if(params.hasIQWakeUp) {
      commonOut.srcWakeUpL1ExuOH.get.zipWithIndex.foreach{ case (exuOHOut, srcIdx) =>
        val wakeupByIQWithoutCancelOH = hasIQWakeupGet.srcWakeupByIQWithoutCancel(srcIdx)
        if (isComp)
          ExuOHGen(exuOHOut, wakeupByIQWithoutCancelOH, hasIQWakeupGet.srcWakeupL1ExuOH(srcIdx))
        else
          ExuOHGen(exuOHOut, 0.U.asTypeOf(wakeupByIQWithoutCancelOH), hasIQWakeupGet.srcWakeupL1ExuOH(srcIdx))
      }
    }

    val srcLoadDependencyForCancel                     = Wire(chiselTypeOf(common.srcLoadDependencyNext))
    val srcLoadDependencyOut                           = Wire(chiselTypeOf(common.srcLoadDependencyNext))
    if(params.hasIQWakeUp) {
      val wakeupSrcLoadDependency                      = hasIQWakeupGet.srcWakeupByIQWithoutCancel.map(x => Mux1H(x, hasIQWakeupGet.wakeupLoadDependencyByIQVec))
      val wakeupSrcLoadDependencyNext                  = hasIQWakeupGet.srcWakeupByIQWithoutCancel.map(x => Mux1H(x, hasIQWakeupGet.shiftedWakeupLoadDependencyByIQVec))
      srcLoadDependencyForCancel.zipWithIndex.foreach { case (ldOut, srcIdx) =>
        ldOut                                         := (if (isComp) Mux(hasIQWakeupGet.srcWakeupByIQWithoutCancel(srcIdx).asUInt.orR,
                                                                      wakeupSrcLoadDependency(srcIdx),
                                                                      status.srcStatus(srcIdx).srcLoadDependency)
                                                          else status.srcStatus(srcIdx).srcLoadDependency)
      }
      srcLoadDependencyOut.zipWithIndex.foreach { case (ldOut, srcIdx) =>
        ldOut                                         := (if (isComp) Mux(hasIQWakeupGet.srcWakeupByIQWithoutCancel(srcIdx).asUInt.orR,
                                                                      wakeupSrcLoadDependencyNext(srcIdx),
                                                                      common.srcLoadDependencyNext(srcIdx))
                                                          else common.srcLoadDependencyNext(srcIdx))
      }
    } else {
      srcLoadDependencyForCancel                      := status.srcStatus.map(_.srcLoadDependency)
      srcLoadDependencyOut                            := common.srcLoadDependencyNext
    }
    commonOut.cancelBypass                            := srcLoadDependencyForCancel.map(x => LoadShouldCancel(Some(x), commonIn.ldCancel)).reduce(_ | _)
    commonOut.entry.bits.status.srcStatus.map(_.srcLoadDependency).zipWithIndex.foreach { case (ldOut, srcIdx) =>
      ldOut                                           := srcLoadDependencyOut(srcIdx)
    }

    commonOut.enqReady                                := common.enqReady
    commonOut.transEntry.valid                        := validReg && !common.flushed && !status.issued
    commonOut.transEntry.bits                         := entryUpdate
    // debug
    commonOut.entryInValid                            := commonIn.enq.valid
    commonOut.entryOutDeqValid                        := validReg && (common.flushed || common.deqSuccess)
    commonOut.entryOutTransValid                      := validReg && commonIn.transSel && !(common.flushed || common.deqSuccess)
    commonOut.perfWakeupByWB                          := common.srcWakeupByWB.zip(status.srcStatus).map{ case (w, s) => w && SrcState.isBusy(s.srcState) && validReg }
    if (params.hasIQWakeUp) {
      commonOut.perfLdCancel.get                      := common.srcCancelVec.map(_ && validReg)
      commonOut.perfOg0Cancel.get                     := hasIQWakeupGet.srcWakeupByIQButCancel.map(_.asUInt.orR && validReg)
      commonOut.perfWakeupByIQ.get                    := hasIQWakeupGet.srcWakeupByIQ.map(x => VecInit(x.map(_ && validReg)))
    }
    // vecMem
    if (params.isVecMemIQ) {
      commonOut.uopIdx.get                            := entryReg.payload.uopIdx
    }
  }

  def EntryVecMemConnect(commonIn: CommonInBundle, common: CommonWireBundle, validReg: Bool, entryReg: EntryBundle, entryRegNext: EntryBundle, entryUpdate: EntryBundle)(implicit p: Parameters, params: IssueBlockParams) = {
    val fromLsq                                        = commonIn.fromLsq.get
    val vecMemStatus                                   = entryReg.status.vecMem.get
    val vecMemStatusUpdate                             = entryUpdate.status.vecMem.get
    vecMemStatusUpdate                                := vecMemStatus

    val isFirstLoad = entryReg.status.vecMem.get.lqIdx === fromLsq.lqDeqPtr

    val isVleff                                        = entryReg.payload.vpu.isVleff
    // update blocked
    entryUpdate.status.blocked                        := !isFirstLoad && isVleff
  }

  def ExuOHGen(exuOH: Vec[Bool], wakeupByIQOH: Vec[Bool], regSrcExuOH: Vec[Bool])(implicit p: Parameters, params: IssueBlockParams) = {
    val origExuOH = Wire(chiselTypeOf(exuOH))
    when(wakeupByIQOH.asUInt.orR) {
      origExuOH := Mux1H(wakeupByIQOH, params.wakeUpSourceExuIdx.map(x => MathUtils.IntToOH(x).U(p(XSCoreParamsKey).backendParams.numExu.W)).toSeq).asBools
    }.otherwise {
      origExuOH := regSrcExuOH
    }
    exuOH := 0.U.asTypeOf(exuOH)
    params.wakeUpSourceExuIdx.foreach(x => exuOH(x) := origExuOH(x))
  }

  object IQFuType {
    def num = FuType.num

    def width = log2Up(num)

    def apply() = Vec(width, Bool())
  }

  class EnqDelayInBundle(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    // wakeup
    val wakeUpFromWB: MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = Flipped(params.genWBWakeUpSinkValidBundle)
    val wakeUpFromIQ: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(params.genIQWakeUpSinkValidBundle)
    // cancel
    val srcLoadDependency     = Input(Vec(params.numRegSrc, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W))))
    val og0Cancel             = Input(ExuVec())
    val ldCancel              = Vec(backendParams.LdExuCnt, Flipped(new LoadCancelIO))
  }

  class EnqDelayOutBundle(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    val srcWakeUpByWB: Vec[UInt]                            = Vec(params.numRegSrc, SrcState())
    val srcWakeUpByIQ: Vec[UInt]                            = Vec(params.numRegSrc, SrcState())
    val srcWakeUpByIQVec: Vec[Vec[Bool]]                    = Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool()))
    val srcCancelByLoad: Vec[Bool]                          = Vec(params.numRegSrc, Bool())
    val shiftedWakeupLoadDependencyByIQVec: Vec[Vec[UInt]]  = Vec(params.numWakeupFromIQ, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))
  }

  def EnqDelayWakeupConnect(enqDelayIn: EnqDelayInBundle, enqDelayOut: EnqDelayOutBundle, status: Status, delay: Int)(implicit p: Parameters, params: IssueBlockParams) = {
    enqDelayOut.srcWakeUpByWB.zipWithIndex.foreach { case (wakeup, i) =>
      wakeup := enqDelayIn.wakeUpFromWB.map{ x => 
        if (i == 3)
          x.bits.wakeUpV0((status.srcStatus(i).psrc, status.srcStatus(i).srcType), x.valid)
        else if (i == 4)
          x.bits.wakeUpVl((status.srcStatus(i).psrc, status.srcStatus(i).srcType), x.valid)
        else
          x.bits.wakeUp(Seq((status.srcStatus(i).psrc, status.srcStatus(i).srcType)), x.valid).head
      }.reduce(_ || _)
    }

    if (params.hasIQWakeUp) {
      val wakeupVec: IndexedSeq[IndexedSeq[Bool]] = enqDelayIn.wakeUpFromIQ.map{ x =>
        val psrcSrcTypeVec = status.srcStatus.map(_.psrc) zip status.srcStatus.map(_.srcType)
        if (params.numRegSrc == 5) {
          x.bits.wakeUpFromIQ(psrcSrcTypeVec.take(3)) :+ 
          x.bits.wakeUpV0FromIQ(psrcSrcTypeVec(3)) :+ 
          x.bits.wakeUpVlFromIQ(psrcSrcTypeVec(4))
        }
        else
          x.bits.wakeUpFromIQ(psrcSrcTypeVec)
      }.toIndexedSeq.transpose
      val cancelSel = params.wakeUpSourceExuIdx.zip(enqDelayIn.wakeUpFromIQ).map{ case (x, y) => enqDelayIn.og0Cancel(x) && y.bits.is0Lat}
      enqDelayOut.srcWakeUpByIQVec := wakeupVec.map(x => VecInit(x.zip(cancelSel).map { case (wakeup, cancel) => wakeup && !cancel }))
    } else {
      enqDelayOut.srcWakeUpByIQVec := 0.U.asTypeOf(enqDelayOut.srcWakeUpByIQVec)
    }

    if (params.hasIQWakeUp) {
      enqDelayOut.srcWakeUpByIQ.zipWithIndex.foreach { case (wakeup, i) =>
        val ldTransCancel = Mux1H(enqDelayOut.srcWakeUpByIQVec(i), enqDelayIn.wakeUpFromIQ.map(_.bits.loadDependency).map(dp => LoadShouldCancel(Some(dp), enqDelayIn.ldCancel)).toSeq)
        wakeup := enqDelayOut.srcWakeUpByIQVec(i).asUInt.orR && !ldTransCancel
      }
      enqDelayOut.srcCancelByLoad.zipWithIndex.foreach { case (ldCancel, i) =>
        ldCancel := LoadShouldCancel(Some(enqDelayIn.srcLoadDependency(i)), enqDelayIn.ldCancel)
      }
    } else {
      enqDelayOut.srcWakeUpByIQ := 0.U.asTypeOf(enqDelayOut.srcWakeUpByIQ)
      enqDelayOut.srcCancelByLoad := 0.U.asTypeOf(enqDelayOut.srcCancelByLoad)
    }

    enqDelayOut.shiftedWakeupLoadDependencyByIQVec.zip(enqDelayIn.wakeUpFromIQ.map(_.bits.loadDependency))
      .zip(params.wakeUpInExuSources.map(_.name)).foreach { case ((dps, ldps), name) =>
      dps.zip(ldps).zipWithIndex.foreach { case ((dp, ldp), deqPortIdx) =>
        if (params.backendParam.getLdExuIdx(params.backendParam.allExuParams.find(_.name == name).get) == deqPortIdx)
          dp := 1.U << (delay - 1)
        else
          dp := ldp << delay
      }
    }
  }
}
