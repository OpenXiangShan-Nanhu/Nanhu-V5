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

package xiangshan

import org.chipsalliance.cde.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import xiangshan.backend.datapath.RdConfig._
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.dispatch.DispatchParameters
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.fu.FuConfig._
import xiangshan.backend.issue.{IntScheduler, IssueBlockParams, MemScheduler, SchdBlockParams, SchedulerType, VfScheduler}
import xiangshan.backend.regfile._
import xiangshan.backend.BackendParams
import xiangshan.backend.trace._
import xiangshan.cache.DCacheParameters
import xiangshan.frontend.{BasePredictor, BranchPredictionResp, FTB, RAS, ITTage, Tage_SC, FauFTB}
import xiangshan.frontend.icache.ICacheParameters
import xiangshan.cache.mmu.{L2TLBParameters, TLBParameters}
import xiangshan.cache.wpu.WPUParameters
import xiangshan.backend.datapath.WakeUpConfig
import xiangshan.mem.prefetch.{PrefetcherParams, SMSParams}
import xs.utils.perf.DebugOptionsKey
import xs.utils.cache.EnableCHI
import xiangshan.PMParameKey

import scala.math.{max, min}

case object XLen extends Field[Int]

case object XSTileKey extends Field[Seq[XSCoreParameters]]

case object XSCoreParamsKey extends Field[XSCoreParameters]

case class XSCoreParameters
(
  HartId: Int = 0,
  XLEN: Int = 64,
  VLEN: Int = 128,
  ELEN: Int = 64,
  HSXLEN: Int = 64,
  HasCExtension: Boolean = true,
  HasHExtension: Boolean = true,
  AddrBits: Int = 64,
  VAddrBitsSv39: Int = 39,
  GPAddrBitsSv39x4: Int = 41,
  VAddrBitsSv48: Int = 48,
  GPAddrBitsSv48x4: Int = 50,
  FetchWidth: Int = 8,
  AsidLength: Int = 16,
  EnableSC: Boolean = false,
  EnbaleTlbDebug: Boolean = false,
  EnableClockGate: Boolean = true,
  EnableJal: Boolean = false,
  EnableSv48: Boolean = true,
  UbtbGHRLength: Int = 4,
  // HistoryLength: Int = 512,
  EnableGHistDiff: Boolean = true,
  EnableCommitGHistDiff: Boolean = true,
  UbtbSize: Int = 256,
  FtbSize: Int = 4096,//2048,
  RasSize: Int = 16,
  RasSpecSize: Int = 32,
  RasCtrSize: Int = 3,
  CacheLineSize: Int = 512,
  FtbWays: Int = 4,
  TageTableInfos: Seq[Tuple3[Int,Int,Int]] =
  //       Sets  Hist   Tag
    Seq(( 2048,    8,    8),
        ( 2048,   13,    8),
        ( 2048,   32,    8),
        ( 2048,  119,    8)),
  ITTageTableInfos: Seq[Tuple3[Int,Int,Int]] =
    Seq(( 256,    8,    9),
        ( 512,   32,    9)),
  SCNRows: Int = 512,
  SCNTables: Int = 4,
  SCCtrBits: Int = 6,
  SCHistLens: Seq[Int] = Seq(0, 4, 10, 16),
  numBr: Int = 2,
  jalOffsetWidth: Int = 21,
  branchPredictor: (BranchPredictionResp, Parameters) => Tuple2[Seq[BasePredictor], BranchPredictionResp] =
  (resp_in: BranchPredictionResp, p: Parameters) => {
    val ftb = Module(new FTB()(p))
    val uftb = Module(new FauFTB()(p))
    val tage = Module(new Tage_SC()(p))
    val ras = Module(new RAS()(p))
    val ittage = Module(new ITTage()(p))
    val preds = Seq(uftb, tage, ftb, ittage, ras)
    preds.map(_.io := DontCare)

    ftb.io.fauftb_entry_in  := uftb.io.fauftb_entry_out
    ftb.io.fauftb_entry_hit_in := uftb.io.fauftb_entry_hit_out

    uftb.io.in.bits.resp_in(0) := resp_in
    tage.io.in.bits.resp_in(0) := uftb.io.out
    ftb.io.in.bits.resp_in(0) := tage.io.out
    ittage.io.in.bits.resp_in(0) := ftb.io.out
    ras.io.in.bits.resp_in(0) := ittage.io.out

    (preds, ras.io.out)
  },
  ICacheForceMetaECCError: Boolean = false,
  ICacheForceDataECCError: Boolean = false,
  IBufSize: Int = 32,
  IBufNBank: Int = 4, // IBuffer bank amount, should divide IBufSize
  DecodeWidth: Int = 4,
  RenameWidth: Int = 4,
  CommitWidth: Int = 8,
  RobCommitWidth: Int = 8,
  RabCommitWidth: Int = 6,
  MaxUopSize: Int = 65,
  EnableRenameSnapshot: Boolean = true,
  RenameSnapshotNum: Int = 3,
  FtqSize: Int = 48,
  EnableLoadFastWakeUp: Boolean = true, // NOTE: not supported now, make it false
  IntLogicRegs: Int = 32,
  FpLogicRegs: Int = 32 + 1 + 1, // 1: I2F, 1: stride
  VecLogicRegs: Int = 32 + 15, // 15: tmp
  V0LogicRegs: Int = 1, // V0
  VlLogicRegs: Int = 1, // Vl
  V0_IDX: Int = 0,
  Vl_IDX: Int = 0,
  NRPhyRegs: Int = 128,
  VirtualLoadQueueSize: Int = 56,
  LoadQueueRAWSize: Int = 24,
  RollbackGroupSize: Int = 8,
  LoadQueueReplaySize: Int = 32,
  LoadUncacheBufferSize: Int = 8,
  LoadQueueNWriteBanks: Int = 4, // NOTE: make sure that LoadQueueRARSize/LoadQueueRAWSize is divided by LoadQueueNWriteBanks
  StoreQueueSize: Int = 32,
  StoreQueueNWriteBanks: Int = 8, // NOTE: make sure that StoreQueueSize is divided by StoreQueueNWriteBanks
  StoreQueueForwardWithMask: Boolean = true,
  VlsQueueSize: Int = 8,
  RobSize: Int = 96,
  RabSize: Int = 96,
  VTypeBufferSize: Int = 24, // used to reorder vtype
  WaitTableSize: Int = 1024,
  dpParams: DispatchParameters = DispatchParameters(
    IntDqSize = 8,
    FpDqSize = 12,
    LsDqSize = 12,
    IntDqDeqWidth = 8,
    FpDqDeqWidth = 6,
    VecDqDeqWidth = 8,
    LsDqDeqWidth = 6,
  ),
  intPreg: PregParams = IntPregParams(
    numEntries = 128,
    numRead = None,
    numWrite = None,
  ),
  vfPreg: VfPregParams = VfPregParams(
    numEntries = 160,
    numRead = None,
    numWrite = None,
  ),
  v0Preg: V0PregParams = V0PregParams(
    numEntries = 22,
    numRead = None,
    numWrite = None,
  ),
  vlPreg: VlPregParams = VlPregParams(
    numEntries = 32,
    numRead = None,
    numWrite = None,
  ),
  IntRegCacheSize: Int = 16,
  MemRegCacheSize: Int = 12,
  EnableMiniConfig: Boolean = false,
  prefetcher: Option[PrefetcherParams] = Some(SMSParams()),
  IfuRedirectNum: Int = 1,
  LoadPipelineWidth: Int = 2,
  StorePipelineWidth: Int = 1,
  VecLoadPipelineWidth: Int = 2,
  VecStorePipelineWidth: Int = 1,
  VecMemSrcInWidth: Int = 2,
  VecMemInstWbWidth: Int = 1,
  VecMemDispatchWidth: Int = 1,
  VecMemDispatchMaxNumber: Int = 16,
  VecMemUnitStrideMaxFlowNum: Int = 2,
  VecMemLSQEnqIteratorNumberSeq: Seq[Int] = Seq(16, 2, 2, 2, 2, 2),
  StoreBufferSize: Int = 8,
  StoreBufferThreshold: Int = 7,
  EnsbufferWidth: Int = 2,
  LoadDependencyWidth: Int = 2,
  // ============ VLSU ============
  VlMergeBufferSize: Int = 16,
  VsMergeBufferSize: Int = 16,
  UopWritebackWidth: Int = 2,
  VLUopWritebackWidth: Int = 1,
  VSUopWritebackWidth: Int = 1,
  VSegmentBufferSize: Int = 8,
  VFOFBufferSize: Int = 8,
  VLFOFWritebackWidth: Int = 1,
  // ==============================
  UncacheBufferSize: Int = 16,
  EnableLoadToLoadForward: Boolean = false,
  EnableFastForward: Boolean = true,
  EnableLdVioCheckAfterReset: Boolean = true,
  EnableSoftPrefetchAfterReset: Boolean = true,
  EnableCacheErrorAfterReset: Boolean = true,
  EnableAccurateLoadError: Boolean = false,
  EnableUncacheWriteOutstanding: Boolean = false,
  EnableStorePrefetchAtIssue: Boolean = false,
  EnableStorePrefetchAtCommit: Boolean = false,
  EnableAtCommitMissTrigger: Boolean = true,
  EnableStorePrefetchSMS: Boolean = false,
  EnableStorePrefetchSPB: Boolean = false,
  HasCMO: Boolean = true,
  MMUAsidLen: Int = 16, // max is 16, 0 is not supported now
  MMUVmidLen: Int = 14,
  ReSelectLen: Int = 7, // load replay queue replay select counter len
  iwpuParameters: WPUParameters = WPUParameters(
    enWPU = false,
    algoName = "mmru",
    isICache = true,
  ),
  dwpuParameters: WPUParameters = WPUParameters(
    enWPU = false,
    algoName = "mmru",
    enCfPred = false,
    isICache = false,
  ),
  itlbParameters: TLBParameters = TLBParameters(
    name = "itlb",
    fetchi = true,
    useDmode = false,
    NWays = 48,
  ),
  itlbPortNum: Int = ICacheParameters().PortNumber + 1,
  ipmpPortNum: Int = 2 * ICacheParameters().PortNumber + 1,
  dtlbParameters: TLBParameters = TLBParameters(
    name = "dtlb",
    NWays = 48,
    outReplace = false,
    partialStaticPMP = true,
    outsideRecvFlush = true,
    saveLevel = false,
    lgMaxSize = 4
  ),
  ldtlbParameters: TLBParameters = TLBParameters(
    name = "ldtlb",
    NWays = 48,
    outReplace = false,
    partialStaticPMP = true,
    outsideRecvFlush = true,
    saveLevel = false,
    lgMaxSize = 4
  ),
  sttlbParameters: TLBParameters = TLBParameters(
    name = "sttlb",
    NWays = 48,
    outReplace = false,
    partialStaticPMP = true,
    outsideRecvFlush = true,
    saveLevel = false,
    lgMaxSize = 4
  ),
  hytlbParameters: TLBParameters = TLBParameters(
    name = "hytlb",
    NWays = 48,
    outReplace = false,
    partialStaticPMP = true,
    outsideRecvFlush = true,
    saveLevel = false,
    lgMaxSize = 4
  ),
  pftlbParameters: TLBParameters = TLBParameters(
    name = "pftlb",
    NWays = 48,
    outReplace = false,
    partialStaticPMP = true,
    outsideRecvFlush = true,
    saveLevel = false,
    lgMaxSize = 4
  ),
  l2ToL1tlbParameters: TLBParameters = TLBParameters(
    name = "l2tlb",
    NWays = 48,
    outReplace = false,
    partialStaticPMP = true,
    outsideRecvFlush = true,
    saveLevel = false
  ),
  refillBothTlb: Boolean = false,
  btlbParameters: TLBParameters = TLBParameters(
    name = "btlb",
    NWays = 48,
  ),
  l2tlbParameters: L2TLBParameters = L2TLBParameters(),
  NumPerfCounters: Int = 16,
  icacheParameters: ICacheParameters = ICacheParameters(
    tagECC = Some("parity"),
    dataECC = Some("parity"),
    replacer = Some("setplru"),
  ),
  dcacheParametersOpt: Option[DCacheParameters] = Some(DCacheParameters(
    tagECC = Some("secded"),
    dataECC = Some("secded"),
    replacer = Some("setplru"),
    nMissEntries = 16,
    nProbeEntries = 8,
    nReleaseEntries = 18,
    nMaxPrefetchEntry = 6,
  )),
  L2NBanks: Int = 1,
  usePTWRepeater: Boolean = false,
  softTLB: Boolean = false, // dpi-c l1tlb debug only
  softPTW: Boolean = false, // dpi-c l2tlb debug only
  softPTWDelay: Int = 1,
  EnableAIA: Boolean = false,
  hasMbist:Boolean = false
){
  def vlWidth = log2Up(VLEN) + 1

  /**
   * the minimum element length of vector elements
   */
  lazy val minVecElen: Int = 8

  /**
   * the maximum number of elements in vector register
   */
  lazy val maxElemPerVreg: Int = VLEN / minVecElen

  lazy val allHistLens = SCHistLens ++ ITTageTableInfos.map(_._2) ++ TageTableInfos.map(_._2) :+ UbtbGHRLength
  lazy val HistoryLength = allHistLens.max + numBr * FtqSize + 9 // 256 for the predictor configs now

  lazy val RegCacheSize = IntRegCacheSize + MemRegCacheSize
  lazy val RegCacheIdxWidth = log2Up(RegCacheSize)

  lazy val intSchdParams = {
    implicit val schdType: SchedulerType = IntScheduler()
    SchdBlockParams(Seq(
      IssueBlockParams(Seq(
        ExeUnitParams("ALU0", Seq(AluCfg, MulCfg, BkuCfg),
                              Seq(IntWB(0, 0)),
                              Seq(Seq(IntRD(0, 0)), Seq(IntRD(1, 0))), true, 2),

        ExeUnitParams("BJU0", Seq(BrhCfg, JmpCfg),
                              Seq(IntWB(0, 1)),
                              Seq(Seq(IntRD(6, 1)), Seq(IntRD(7, 1))), true, 2),
      ), numEntries = if(EnableMiniConfig) 4 else (18), numEnq = if(EnableMiniConfig) 2 else (2), numComp = if(EnableMiniConfig) 2 else (16)),
      IssueBlockParams(Seq(
        ExeUnitParams("ALU1", Seq(AluCfg, MulCfg, BkuCfg),
                              Seq(IntWB(1, 0)),
                              Seq(Seq(IntRD(2, 0)), Seq(IntRD(3, 0))), true, 2),

        ExeUnitParams("BJU1", Seq(BrhCfg, JmpCfg),
                              Seq(IntWB(1, 1)),
                              Seq(Seq(IntRD(4, 1)), Seq(IntRD(5, 1))), true, 2),
      ), numEntries = if(EnableMiniConfig) 4 else (18), numEnq = if(EnableMiniConfig) 2 else (2), numComp = if(EnableMiniConfig) 2 else (16)),
      IssueBlockParams(Seq(
        ExeUnitParams("ALU2", Seq(AluCfg),
                              Seq(IntWB(2, 0)),
                              Seq(Seq(IntRD(4, 0)), Seq(IntRD(5, 0))), true, 2),

        ExeUnitParams("BJU2", Seq(BrhCfg, JmpCfg, I2fCfg, VSetRiWiCfg, VSetRiWvfCfg, I2vCfg),
                              Seq(IntWB(4, 0), VfWB(0, 0), V0WB(0, 0), VlWB(0, 0)),
                              Seq(Seq(IntRD(2, 1)), Seq(IntRD(3, 1)))),
      ), numEntries = if(EnableMiniConfig) 4 else (18), numEnq = if(EnableMiniConfig) 2 else (2), numComp = if(EnableMiniConfig) 2 else (16)),
      IssueBlockParams(Seq(
        ExeUnitParams("ALU3", Seq(AluCfg),
                              Seq(IntWB(3, 0)),
                              Seq(Seq(IntRD(6, 0)), Seq(IntRD(7, 0))), true, 2),

        ExeUnitParams("BJU3", Seq(CsrCfg, FenceCfg, DivCfg),
                              Seq(IntWB(4, 1)),
                              Seq(Seq(IntRD(0, 1)), Seq(IntRD(1, 1)))),
      ), numEntries = if(EnableMiniConfig) 4 else (18), numEnq = if(EnableMiniConfig) 2 else (2), numComp = if(EnableMiniConfig) 2 else (16)),
    ),
      numPregs = intPreg.numEntries,
      numDeqOutside = 0,
      schdType = schdType,
      rfDataWidth = intPreg.dataCfg.map(_.dataWidth).max,
      numUopIn = dpParams.IntDqDeqWidth,
    )
  }

  lazy val vfSchdParams = {
    implicit val schdType: SchedulerType = VfScheduler()
    SchdBlockParams(Seq(
      IssueBlockParams(Seq(
        ExeUnitParams("VFEX0",  Seq(VialuCfg, VimacCfg, VppuCfg, VipuCfg, VidivCfg, VSetRvfWvfCfg, F2vCfg), 
                                Seq(VfWB(1, 0), V0WB(1, 0), VlWB(1, 0), IntWB(1, 2)),
                                Seq(Seq(VfRD(3, 1)), Seq(VfRD(4, 1)), Seq(VfRD(6, 1)), Seq(V0RD(0, 0)), Seq(VlRD(0, 0)))),
      ), numEntries = if(EnableMiniConfig) 4 else (8), numEnq = if(EnableMiniConfig) 1 else (1), numComp = if(EnableMiniConfig) 3 else (7)),
      IssueBlockParams(Seq(
        ExeUnitParams("VFMA0",  Seq(Vfma64Cfg),
                                Seq(VfWB(2, 0), V0WB(2, 0)),
                                Seq(Seq(VfRD(3, 0)), Seq(VfRD(4, 0)), Seq(VfRD(5, 0)), Seq(V0RD(1, 0)), Seq(VlRD(1, 0)))),

        ExeUnitParams("VFMA1",  Seq(Vfma64Cfg),
                                Seq(VfWB(3, 0), V0WB(3, 0)),
                                Seq(Seq(VfRD(6, 0)), Seq(VfRD(7, 0)), Seq(VfRD(8, 0)), Seq(V0RD(2, 0)), Seq(VlRD(2, 0)))),
      ), numEntries = if(EnableMiniConfig) 4 else (16), numEnq = if(EnableMiniConfig) 1 else (2), numComp = if(EnableMiniConfig) 3 else (8), sharedVf = true),
      IssueBlockParams(Seq(
        ExeUnitParams("VFALU0", Seq(Vfalu64Cfg, Vfcvt64Cfg),
                                Seq(VfWB(4, 0), V0WB(4, 0), IntWB(7, 0)),
                                Seq(Seq(VfRD(9, 0)), Seq(VfRD(10, 0)), Seq(VfRD(5, 2)), Seq(V0RD(3, 0)), Seq(VlRD(3, 0)))),

        ExeUnitParams("VFALU1", Seq(Vfalu64Cfg, Vfcvt64Cfg),
                                Seq(VfWB(5, 0), V0WB(5, 0), IntWB(8, 0)),
                                Seq(Seq(VfRD(11, 0)), Seq(VfRD(12, 0)), Seq(VfRD(8, 2)), Seq(V0RD(4, 0)), Seq(VlRD(4, 0)))),
      ), numEntries = if(EnableMiniConfig) 4 else (16), numEnq = if(EnableMiniConfig) 1 else (2), numComp = if(EnableMiniConfig) 3 else (8), sharedVf = true),
      IssueBlockParams(Seq(
        ExeUnitParams("VFDIV", Seq(VfdivCfg),
                                Seq(VfWB(1, 1), V0WB(1, 1)),
                                Seq(Seq(VfRD(5, 1)), Seq(VfRD(8, 1)), Seq(VfRD(7, 1)), Seq(V0RD(0, 1)), Seq(VlRD(0, 1)))),

        // ExeUnitParams("VFDIV1", Seq(Vfdiv64Cfg),
        //                         Seq(VfWB(7, 0), V0WB(7, 0)),
        //                         Seq(Seq(VfRD(18, 0)), Seq(VfRD(19, 0)), Seq(VfRD(20, 0)), Seq(V0RD(6, 0)), Seq(VlRD(6, 0)))),
      ), numEntries = if(EnableMiniConfig) 4 else (10), numEnq = if(EnableMiniConfig) 1 else (1), numComp = if(EnableMiniConfig) 3 else (9)),
      
    ),
      numPregs = vfPreg.numEntries,
      numDeqOutside = 0,
      schdType = schdType,
      rfDataWidth = vfPreg.dataCfg.map(_.dataWidth).max,
      numUopIn = dpParams.VecDqDeqWidth,
    )
  }

  lazy val memSchdParams = {
    implicit val schdType: SchedulerType = MemScheduler()
    val rfDataWidth = 64

    SchdBlockParams(Seq(
      IssueBlockParams(Seq(
        ExeUnitParams("STA0", Seq(StaCfg, MouCfg), Seq(FakeIntWB()), Seq(Seq(IntRD(10, 0)))),
      ), numEntries = if(EnableMiniConfig) 4 else (20), numEnq = if(EnableMiniConfig) 1 else (2), numComp = if(EnableMiniConfig) 3 else (18)),
      IssueBlockParams(Seq(
        ExeUnitParams("LDU0", Seq(LduCfg),
                              Seq(IntWB(5, 0), VfWB(6, 0)),
                              Seq(Seq(IntRD(8, 0))), true, 2),
      ), numEntries = if(EnableMiniConfig) 4 else (16), numEnq = if(EnableMiniConfig) 1 else (1), numComp = if(EnableMiniConfig) 3 else (15)),
      IssueBlockParams(Seq(
        ExeUnitParams("LDU1", Seq(LduCfg), Seq(IntWB(6, 0), VfWB(7, 0)), Seq(Seq(IntRD(9, 0))), true, 2),
      ), numEntries = if(EnableMiniConfig) 4 else (16), numEnq = if(EnableMiniConfig) 1 else (1), numComp = if(EnableMiniConfig) 3 else (15)),
      IssueBlockParams(Seq(
        ExeUnitParams("VLSU0", Seq(VlduCfg, VstuCfg, VseglduSeg, VsegstuCfg), Seq(VfWB(8, 0), V0WB(6, 0), VlWB(2, 0)), Seq(Seq(VfRD(0, 0)), Seq(VfRD(1, 0)), Seq(VfRD(2, 0)), Seq(V0RD(5, 0)), Seq(VlRD(5, 0)))),
      ), numEntries = if(EnableMiniConfig) 4 else (16), numEnq = if(EnableMiniConfig) 1 else (2), numComp = if(EnableMiniConfig) 3 else (14)),
      IssueBlockParams(Seq(
        ExeUnitParams("STD0", Seq(StdCfg, MoudCfg), Seq(), Seq(Seq(IntRD(11, 0), VfRD(13, 0)))),
      ), numEntries = if(EnableMiniConfig) 4 else (20), numEnq = if(EnableMiniConfig) 1 else (2), numComp = if(EnableMiniConfig) 3 else (18)),
    ),
      numPregs = intPreg.numEntries max vfPreg.numEntries,
      numDeqOutside = 0,
      schdType = schdType,
      rfDataWidth = rfDataWidth,
      numUopIn = dpParams.LsDqDeqWidth,
    )
  }

  def PregIdxWidthMax = intPreg.addrWidth max vfPreg.addrWidth

  def iqWakeUpParams = {
    Seq(
      WakeUpConfig(
        Seq("ALU0", "ALU1", "ALU2", "ALU3", "LDU0", "LDU1") ->
        Seq("ALU0", "BJU0", "ALU1", "BJU1", "ALU2", "BJU2", "ALU3", "BJU3", "LDU0", "LDU1", "STA0", "STD0")
      ),
      WakeUpConfig(
        Seq("VFMA0", "VFMA1", "VFALU0", "VFALU1") ->
        Seq("VFMA0", "VFMA1", "VFALU0", "VFALU1", "VFDIV")
      ),
    ).flatten
  }

  def fakeIntPreg = FakeIntPregParams(intPreg.numEntries, intPreg.numRead, intPreg.numWrite)

  lazy val backendParams: BackendParams = {
    val res = backend.BackendParams(
      Map(
        IntScheduler() -> intSchdParams,
        // FpScheduler() -> fpSchdParams,
        VfScheduler() -> vfSchdParams,
        MemScheduler() -> memSchdParams,
      ),
      Seq(
        intPreg,
        vfPreg,
        v0Preg,
        vlPreg,
        fakeIntPreg
      ),
      iqWakeUpParams,
    )
    require(LoadPipelineWidth == res.LdExuCnt, "LoadPipelineWidth must be equal exuParameters.LduCnt!")
    require(StorePipelineWidth == res.StaCnt, "StorePipelineWidth must be equal exuParameters.StuCnt!")
    res
  }

  // Parameters for trace extension.
  // Trace parameters is useful for XSTOP.
  lazy val traceParams: TraceParams = new TraceParams(
    HasEncoder     = true,
    TraceEnable    = true,
    TraceGroupNum  = 4,
    IaddrWidth     = GPAddrBitsSv48x4,
    PrivWidth      = 3,
    ItypeWidth     = 4,
    IlastsizeWidth = 1,
  )
}

trait HasXSParameter {

  implicit val p: Parameters

  def PAddrBits = 48// p(SoCParamsKey).PAddrBits // PAddrBits is Phyical Memory addr bits
  def PmemRanges = p(PMParameKey).PmemRanges
  final val PageOffsetWidth = 12
  def NodeIDWidth = 11 // p(SoCParamsKey).NodeIDWidthList(p(CHIIssue)) // NodeID width among NoC
  def L3notEmpty = true// p(SoCParamsKey).L3CacheParamsOpt.nonEmpty

  def coreParams = p(XSCoreParamsKey)
  def env = p(DebugOptionsKey)

  def XLEN = coreParams.XLEN
  def VLEN = coreParams.VLEN
  def ELEN = coreParams.ELEN
  def HSXLEN = coreParams.HSXLEN
  val minFLen = 32
  val fLen = 64
  def hartIdLen = 6// p(MaxHartIdBits)
  val xLen = XLEN

  def HasCExtension = coreParams.HasCExtension
  def HasHExtension = coreParams.HasHExtension
  def EnableSv48 = coreParams.EnableSv48
  def AddrBits = coreParams.AddrBits // AddrBits is used in some cases
  def GPAddrBitsSv39x4 = coreParams.GPAddrBitsSv39x4
  def GPAddrBitsSv48x4 = coreParams.GPAddrBitsSv48x4
  def GPAddrBits = {
    if (EnableSv48)
      coreParams.GPAddrBitsSv48x4
    else
      coreParams.GPAddrBitsSv39x4
  }
  def VAddrBits = {
    if (HasHExtension) {
      if (EnableSv48)
        coreParams.GPAddrBitsSv48x4
      else
        coreParams.GPAddrBitsSv39x4
    } else {
      if (EnableSv48)
        coreParams.VAddrBitsSv48
      else
        coreParams.VAddrBitsSv39
    }
  } // VAddrBits is Virtual Memory addr bits

  def VAddrMaxBits = {
    if(EnableSv48) {
      coreParams.VAddrBitsSv48 max coreParams.GPAddrBitsSv48x4
    } else {
      coreParams.VAddrBitsSv39 max coreParams.GPAddrBitsSv39x4
    }
  }

  def AsidLength = coreParams.AsidLength
  def ReSelectLen = coreParams.ReSelectLen
  def AddrBytes = AddrBits / 8 // unused
  def DataBits = XLEN
  def DataBytes = DataBits / 8
  def VDataBytes = VLEN / 8
  def FetchWidth = coreParams.FetchWidth
  def PredictWidth = FetchWidth * (if (HasCExtension) 2 else 1)
  def EnableSC = coreParams.EnableSC
  def EnbaleTlbDebug = coreParams.EnbaleTlbDebug
  def HistoryLength = coreParams.HistoryLength
  def EnableGHistDiff = coreParams.EnableGHistDiff
  def EnableCommitGHistDiff = coreParams.EnableCommitGHistDiff
  def EnableClockGate = coreParams.EnableClockGate
  def UbtbGHRLength = coreParams.UbtbGHRLength
  def UbtbSize = coreParams.UbtbSize
  def FtbSize = coreParams.FtbSize
  def FtbWays = coreParams.FtbWays
  def RasSize = coreParams.RasSize
  def RasSpecSize = coreParams.RasSpecSize
  def RasCtrSize = coreParams.RasCtrSize

  def getBPDComponents(resp_in: BranchPredictionResp, p: Parameters) = {
    coreParams.branchPredictor(resp_in, p)
  }
  def numBr = coreParams.numBr
  def jalOffsetWidth = coreParams.jalOffsetWidth
  def TageTableInfos = coreParams.TageTableInfos
  def TageBanks = coreParams.numBr
  def SCNRows = coreParams.SCNRows
  def SCCtrBits = coreParams.SCCtrBits
  def SCHistLens = coreParams.SCHistLens
  def SCNTables = coreParams.SCNTables

  def SCTableInfos = Seq.fill(SCNTables)((SCNRows, SCCtrBits)) zip SCHistLens map {
    case ((n, cb), h) => (n, cb, h)
  }
  def ITTageTableInfos = coreParams.ITTageTableInfos
  type FoldedHistoryInfo = Tuple2[Int, Int]
  def foldedGHistInfos =
    (TageTableInfos.map{ case (nRows, h, t) =>
      if (h > 0)
        Set((h, min(log2Ceil(nRows/numBr), h)), (h, min(h, t)), (h, min(h, t-1)))
      else
        Set[FoldedHistoryInfo]()
    }.reduce(_++_).toSet ++
    SCTableInfos.map{ case (nRows, _, h) =>
      if (h > 0)
        Set((h, min(log2Ceil(nRows/TageBanks), h)))
      else
        Set[FoldedHistoryInfo]()
    }.reduce(_++_).toSet ++
    ITTageTableInfos.map{ case (nRows, h, t) =>
      if (h > 0)
        Set((h, min(log2Ceil(nRows), h)), (h, min(h, t)), (h, min(h, t-1)))
      else
        Set[FoldedHistoryInfo]()
    }.reduce(_++_) ++
      Set[FoldedHistoryInfo]((UbtbGHRLength, log2Ceil(UbtbSize)))
    ).toList



  def CacheLineSize = coreParams.CacheLineSize
  def CacheLineHalfWord = CacheLineSize / 16
  def ExtHistoryLength = HistoryLength + 64
  def ICacheForceMetaECCError = coreParams.ICacheForceMetaECCError
  def ICacheForceDataECCError = coreParams.ICacheForceDataECCError
  def IBufSize = coreParams.IBufSize
  def IBufNBank = coreParams.IBufNBank
  def backendParams: BackendParams = coreParams.backendParams
  def DecodeWidth = coreParams.DecodeWidth
  def RenameWidth = coreParams.RenameWidth
  def CommitWidth = coreParams.CommitWidth
  def RobCommitWidth = coreParams.RobCommitWidth
  def RabCommitWidth = coreParams.RabCommitWidth
  def MaxUopSize = coreParams.MaxUopSize
  def EnableRenameSnapshot = coreParams.EnableRenameSnapshot
  def RenameSnapshotNum = coreParams.RenameSnapshotNum
  def FtqSize = coreParams.FtqSize
  def EnableLoadFastWakeUp = coreParams.EnableLoadFastWakeUp
  def IntLogicRegs = coreParams.IntLogicRegs
  def FpLogicRegs = coreParams.FpLogicRegs
  def VecLogicRegs = coreParams.VecLogicRegs
  def V0LogicRegs = coreParams.V0LogicRegs
  def VlLogicRegs = coreParams.VlLogicRegs
  def MaxLogicRegs = Set(IntLogicRegs, FpLogicRegs, VecLogicRegs, V0LogicRegs, VlLogicRegs).max
  def LogicRegsWidth = log2Ceil(MaxLogicRegs)
  def V0_IDX = coreParams.V0_IDX
  def Vl_IDX = coreParams.Vl_IDX
  def IntPhyRegs = coreParams.intPreg.numEntries
  def VfPhyRegs = coreParams.vfPreg.numEntries
  def V0PhyRegs = coreParams.v0Preg.numEntries
  def VlPhyRegs = coreParams.vlPreg.numEntries
  def MaxPhyRegs = Seq(IntPhyRegs, VfPhyRegs, V0PhyRegs, VlPhyRegs).max
  def IntPhyRegIdxWidth = log2Up(IntPhyRegs)
  def VfPhyRegIdxWidth = log2Up(VfPhyRegs)
  def V0PhyRegIdxWidth = log2Up(V0PhyRegs)
  def VlPhyRegIdxWidth = log2Up(VlPhyRegs)
  def PhyRegIdxWidth = Seq(IntPhyRegIdxWidth, VfPhyRegIdxWidth, V0PhyRegIdxWidth, VlPhyRegIdxWidth).max
  def RobSize = coreParams.RobSize
  def RabSize = coreParams.RabSize
  def VTypeBufferSize = coreParams.VTypeBufferSize
  def IntRegCacheSize = coreParams.IntRegCacheSize
  def MemRegCacheSize = coreParams.MemRegCacheSize
  def RegCacheSize = coreParams.RegCacheSize
  def RegCacheIdxWidth = coreParams.RegCacheIdxWidth
  /**
   * the minimum element length of vector elements
   */
  def minVecElen: Int = coreParams.minVecElen

  /**
   * the maximum number of elements in vector register
   */
  def maxElemPerVreg: Int = coreParams.maxElemPerVreg

  def IntRefCounterWidth = log2Ceil(RobSize)
  def LSQEnqWidth = coreParams.dpParams.LsDqDeqWidth
  def LSQLdEnqWidth = LSQEnqWidth min backendParams.numLoadDp
  def LSQStEnqWidth = LSQEnqWidth min backendParams.numStoreDp
  def VirtualLoadQueueSize = coreParams.VirtualLoadQueueSize
  def LoadQueueRAWSize = coreParams.LoadQueueRAWSize
  def RollbackGroupSize = coreParams.RollbackGroupSize
  def LoadQueueReplaySize = coreParams.LoadQueueReplaySize
  def LoadUncacheBufferSize = coreParams.LoadUncacheBufferSize
  def LoadQueueNWriteBanks = coreParams.LoadQueueNWriteBanks
  def StoreQueueSize = coreParams.StoreQueueSize
  def VirtualLoadQueueMaxStoreQueueSize = VirtualLoadQueueSize max StoreQueueSize
  def StoreQueueNWriteBanks = coreParams.StoreQueueNWriteBanks
  def StoreQueueForwardWithMask = coreParams.StoreQueueForwardWithMask
  def VlsQueueSize = coreParams.VlsQueueSize
  def dpParams = coreParams.dpParams

  def MemIQSizeMax = backendParams.memSchdParams.get.issueBlockParams.map(_.numEntries).max
  def IQSizeMax = backendParams.allSchdParams.map(_.issueBlockParams.map(_.numEntries).max).max

  def NumRedirect = backendParams.numRedirect
  def BackendRedirectNum = NumRedirect + 2 //2: ldReplay + Exception
  def FtqRedirectAheadNum = NumRedirect
  def IfuRedirectNum = coreParams.IfuRedirectNum
  def LoadPipelineWidth = coreParams.LoadPipelineWidth
  def StorePipelineWidth = coreParams.StorePipelineWidth
  def VecLoadPipelineWidth = coreParams.VecLoadPipelineWidth
  def VecStorePipelineWidth = coreParams.VecStorePipelineWidth
  def VecMemSrcInWidth = coreParams.VecMemSrcInWidth
  def VecMemInstWbWidth = coreParams.VecMemInstWbWidth
  def VecMemDispatchWidth = coreParams.VecMemDispatchWidth
  def VecMemDispatchMaxNumber = coreParams.VecMemDispatchMaxNumber
  def VecMemUnitStrideMaxFlowNum = coreParams.VecMemUnitStrideMaxFlowNum
  def VecMemLSQEnqIteratorNumberSeq = coreParams.VecMemLSQEnqIteratorNumberSeq
  def StoreBufferSize = coreParams.StoreBufferSize
  def StoreBufferThreshold = coreParams.StoreBufferThreshold
  def EnsbufferWidth = coreParams.EnsbufferWidth
  def LoadDependencyWidth = coreParams.LoadDependencyWidth
  def VlMergeBufferSize = coreParams.VlMergeBufferSize
  def VsMergeBufferSize = coreParams.VsMergeBufferSize
  def UopWritebackWidth = coreParams.UopWritebackWidth
  def VLUopWritebackWidth = coreParams.VLUopWritebackWidth
  def VSUopWritebackWidth = coreParams.VSUopWritebackWidth
  def VSegmentBufferSize = coreParams.VSegmentBufferSize
  def VFOFBufferSize = coreParams.VFOFBufferSize
  def UncacheBufferSize = coreParams.UncacheBufferSize
  def EnableLoadToLoadForward = coreParams.EnableLoadToLoadForward
  def EnableFastForward = coreParams.EnableFastForward
  def EnableLdVioCheckAfterReset = coreParams.EnableLdVioCheckAfterReset
  def EnableSoftPrefetchAfterReset = coreParams.EnableSoftPrefetchAfterReset
  def EnableCacheErrorAfterReset = coreParams.EnableCacheErrorAfterReset
  def EnableAccurateLoadError = coreParams.EnableAccurateLoadError
  def EnableUncacheWriteOutstanding = coreParams.EnableUncacheWriteOutstanding
  def EnableStorePrefetchAtIssue = coreParams.EnableStorePrefetchAtIssue
  def EnableStorePrefetchAtCommit = coreParams.EnableStorePrefetchAtCommit
  def EnableAtCommitMissTrigger = coreParams.EnableAtCommitMissTrigger
  def EnableStorePrefetchSMS = coreParams.EnableStorePrefetchSMS
  def EnableStorePrefetchSPB = coreParams.EnableStorePrefetchSPB
  def HasCMO = coreParams.HasCMO && p(EnableCHI)

  def Enable3Load3Store = (LoadPipelineWidth == 3 && StorePipelineWidth == 3)
  def asidLen = coreParams.MMUAsidLen
  def vmidLen = coreParams.MMUVmidLen
  def BTLBWidth = coreParams.LoadPipelineWidth + coreParams.StorePipelineWidth
  def refillBothTlb = coreParams.refillBothTlb
  def iwpuParam = coreParams.iwpuParameters
  def dwpuParam = coreParams.dwpuParameters
  def dtlbParams = coreParams.dtlbParameters
  def itlbParams = coreParams.itlbParameters
  def ldtlbParams = coreParams.ldtlbParameters
  def sttlbParams = coreParams.sttlbParameters
  def hytlbParams = coreParams.hytlbParameters
  def pftlbParams = coreParams.pftlbParameters
  def l2ToL1Params = coreParams.l2ToL1tlbParameters
  def btlbParams = coreParams.btlbParameters
  def l2tlbParams = coreParams.l2tlbParameters
  def NumPerfCounters = coreParams.NumPerfCounters

  def instBytes = if (HasCExtension) 2 else 4
  def instOffsetBits = log2Ceil(instBytes)

  def icacheParameters = coreParams.icacheParameters
  def dcacheParameters = coreParams.dcacheParametersOpt.getOrElse(DCacheParameters())

  // dcache block cacheline when lr for LRSCCycles - LRSCBackOff cycles
  // for constrained LR/SC loop
  def LRSCCycles = 64
  // for lr storm
  def LRSCBackOff = 8

  // cache hierarchy configurations
  def l1BusDataWidth = 256

  // load violation predict
  def ResetTimeMax2Pow = 20 //1078576
  def ResetTimeMin2Pow = 10 //1024
  // wait table parameters
  def WaitTableSize = coreParams.WaitTableSize
  def MemPredPCWidth = 10
  def LWTUse2BitCounter = true
  // store set parameters
  def SSITSize = WaitTableSize
  def LFSTSize = 32
  def SSIDWidth = log2Up(LFSTSize)
  def LFSTWidth = 4
  def StoreSetEnable = true // LWT will be disabled if SS is enabled
  def LFSTEnable = true

  def PCntIncrStep: Int = 6
  def numPCntHc: Int = 12
  def numPCntPtw: Int = 19

  def numCSRPCntFrontend = 8
  def numCSRPCntCtrl     = 8
  def numCSRPCntLsu      = 8
  def numCSRPCntHc       = 5
  def printEventCoding   = true

  def EnableAIA = coreParams.EnableAIA

  def hasMbist = coreParams.hasMbist

  // Vector load exception
  def maxMergeNumPerCycle = 4

  // Parameters for Sdtrig extension
  protected def TriggerNum = 4
  protected def TriggerChainMaxLength = 2

  // Parameters for Trace extension
  def TraceGroupNum          = coreParams.traceParams.TraceGroupNum
  def HasEncoder             = coreParams.traceParams.HasEncoder
  def TraceEnable            = coreParams.traceParams.TraceEnable
  def CauseWidth             = XLEN
  def TvalWidth              = coreParams.traceParams.IaddrWidth
  def PrivWidth              = coreParams.traceParams.PrivWidth
  def IaddrWidth             = coreParams.traceParams.IaddrWidth
  def ItypeWidth             = coreParams.traceParams.ItypeWidth
  def IretireWidthInPipe     = log2Up(RenameWidth * 2 + 1)
  def IretireWidthCompressed = log2Up(RenameWidth * CommitWidth * 2 + 1)
  def IlastsizeWidth         = coreParams.traceParams.IlastsizeWidth
}
