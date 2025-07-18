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
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomacy.{BundleBridgeSource, LazyModule, LazyModuleImp}
import freechips.rocketchip.interrupts.{IntSinkNode, IntSinkPortSimple}
import freechips.rocketchip.tile.HasFPUParameters
import freechips.rocketchip.tilelink._
import device.MsiInfoBundle
import utils._
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, MemExuInput, MemExuOutput}
import xiangshan.backend.ctrlblock.{DebugLSIO, LsTopdownInfo}
import xiangshan.backend.exu.MemExeUnit
import xiangshan.backend.fu._
import xiangshan.backend.fu.FuType._
import xiangshan.backend.rob.{RobDebugRollingIO, RobPtr}
import xiangshan.backend.fu.util.{CSRConst, HasCSRConst, SdtrigExt}
import xiangshan.cache._
import xiangshan.cache.mmu._
import xiangshan.mem._
import xiangshan.mem.mdp._
import xiangshan.frontend.HasInstrMMIOConst
import xiangshan.mem.prefetch.{BasePrefecher, L1Prefetcher, SMSParams, SMSPrefetcher}
import xiangshan.backend.datapath.NewPipelineConnect
import xs.utils._
import xs.utils.cache.common._
import xs.utils.mbist.{MbistInterface, MbistPipeline}
import xs.utils.sram.{SramBroadcastBundle, SramHelper}
import xs.utils.perf.{HasPerfEvents, PerfEvent, XSDebug, XSError, XSPerfAccumulate}
import xs.utils.perf.{DebugOptionsKey, HPerfMonitor, XSPerfHistogram}
import xs.utils.cache.common.{PrefetchRecv, CMOResp, CMOReq}
import xs.utils.cache.EnableCHI

trait HasMemBlockParameters extends HasXSParameter {
  // number of memory units
  val LduCnt  = backendParams.LduCnt
  val StaCnt  = backendParams.StaCnt
  val StdCnt  = backendParams.StdCnt
  val HyuCnt  = backendParams.HyuCnt
  val VlduCnt = backendParams.VlduCnt
  val VstuCnt = backendParams.VstuCnt

  val LdExuCnt  = LduCnt + HyuCnt
  val StAddrCnt = StaCnt + HyuCnt
  val StDataCnt = StdCnt
  val MemExuCnt = LduCnt + HyuCnt + StaCnt + StdCnt
  val MemAddrExtCnt = LdExuCnt + StaCnt
  val MemVExuCnt = VlduCnt + VstuCnt
}

abstract class MemBlockBundle(implicit val p: Parameters) extends Bundle with HasMemBlockParameters

class Std(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) {
  io.in.ready := io.out.ready
  io.out.valid := io.in.valid
  io.out.bits := 0.U.asTypeOf(io.out.bits)
  io.out.bits.res.data := io.in.bits.data.src(0)
  io.out.bits.ctrl.robIdx := io.in.bits.ctrl.robIdx
}

class ooo_to_mem(implicit p: Parameters) extends MemBlockBundle {
  val backendToTopBypass = Flipped(new BackendToTopBundle)

  val loadFastMatch = Vec(LdExuCnt, Input(UInt(LdExuCnt.W)))
  val loadFastFuOpType = Vec(LdExuCnt, Input(FuOpType()))
  val loadFastImm = Vec(LdExuCnt, Input(UInt(12.W)))
  val sfence = Input(new SfenceBundle)
  val tlbCsr = Input(new TlbCsrBundle)
  val lsqio = new Bundle {
    val lcommit = Input(UInt(log2Up(CommitWidth + 1).W))
    val scommit = Input(UInt(log2Up(CommitWidth + 1).W))
    val pendingld = Input(Bool())
    val pendingst = Input(Bool())
    val pendingVst = Input(Bool())
    val commit = Input(Bool())
    val pendingPtr = Input(new RobPtr)
    val pendingPtrNext = Input(new RobPtr)
  }

  val isStoreException = Input(Bool())
  val isVlsException = Input(Bool())
  val csrCtrl = Flipped(new CustomCSRCtrlIO)
  val enqLsq = new LsqEnqIO
  val flushSb = Input(Bool())

  val loadPc = Vec(LduCnt, Input(UInt(VAddrBits.W))) // for hw prefetch
  val storePc = Vec(StaCnt, Input(UInt(VAddrBits.W))) // for hw prefetch
  val hybridPc = Vec(HyuCnt, Input(UInt(VAddrBits.W))) // for hw prefetch

  val issueLda = MixedVec(Seq.fill(LduCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueSta = MixedVec(Seq.fill(StaCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueStd = MixedVec(Seq.fill(StdCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueHya = MixedVec(Seq.fill(HyuCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueVldu = MixedVec(Seq.fill(VlduCnt)(Flipped(DecoupledIO(new MemExuInput(isVector=true)))))

  def issueUops = issueLda ++ issueSta ++ issueStd ++ issueHya ++ issueVldu
}

class mem_to_ooo(implicit p: Parameters) extends MemBlockBundle {
  val topToBackendBypass = new TopToBackendBundle

  val otherFastWakeup = Vec(LdExuCnt, ValidIO(new DynInst))
  val lqCancelCnt = Output(UInt(log2Up(VirtualLoadQueueSize + 1).W))
  val sqCancelCnt = Output(UInt(log2Up(StoreQueueSize + 1).W))
  val sqDeq = Output(UInt(log2Ceil(EnsbufferWidth + 1).W))
  val lqDeq = Output(UInt(log2Up(CommitWidth + 1).W))
  // used by VLSU issue queue, the vector store would wait all store before it, and the vector load would wait all load
  val sqDeqPtr = Output(new SqPtr)
  val lqDeqPtr = Output(new LqPtr)
  val stIn = Vec(StAddrCnt, ValidIO(new MemExuInput))
  val stIssuePtr = Output(new SqPtr())

  val memoryViolation = ValidIO(new Redirect)
  val sbIsEmpty = Output(Bool())
  val cmoFinish = Output(Bool())

  val lsTopdownInfo = Vec(LdExuCnt, Output(new LsTopdownInfo))

  val lsqio = new Bundle {
    val vaddr = Output(UInt(XLEN.W))
    val vstart = Output(UInt((log2Up(VLEN) + 1).W))
    val vl = Output(UInt((log2Up(VLEN) + 1).W))
    val gpaddr = Output(UInt(XLEN.W))
    val isForVSnonLeafPTE = Output(Bool())
    val mmio = Output(Vec(LoadPipelineWidth, Bool()))
    val uop = Output(Vec(LoadPipelineWidth, new DynInst))
    val lqCanAccept = Output(Bool())
    val sqCanAccept = Output(Bool())
  }
  val writebackLda = Vec(LduCnt, DecoupledIO(new MemExuOutput))
  val writebackSta = Vec(StaCnt, DecoupledIO(new MemExuOutput))
  val writebackStd = Vec(StdCnt, DecoupledIO(new MemExuOutput))
  val writebackHyuLda = Vec(HyuCnt, DecoupledIO(new MemExuOutput))
  val writebackHyuSta = Vec(HyuCnt, DecoupledIO(new MemExuOutput))
  val writebackVldu = Vec(VlduCnt, DecoupledIO(new MemExuOutput(isVector = true)))
  def writeBack: Seq[DecoupledIO[MemExuOutput]] = {
    writebackSta ++
      writebackHyuLda ++ writebackHyuSta ++
      writebackLda ++
      writebackVldu ++
      writebackStd
  }

  val ldaIqFeedback = Vec(LduCnt, new MemRSFeedbackIO)
  val staIqFeedback = Vec(StaCnt, new MemRSFeedbackIO)
  val hyuIqFeedback = Vec(HyuCnt, new MemRSFeedbackIO)
  val vstuIqFeedback= Vec(VstuCnt, new MemRSFeedbackIO(isVector = true))
  val vlduIqFeedback= Vec(VlduCnt, new MemRSFeedbackIO(isVector = true))
  val ldCancel = Vec(backendParams.LdExuCnt, new LoadCancelIO)
  val wakeup = Vec(backendParams.LdExuCnt, Valid(new DynInst))

  val s3_delayed_load_error = Vec(LdExuCnt, Output(Bool()))

  val sqHasCmo = Output(Bool())
}

class MemCoreTopDownIO extends Bundle {
  val robHeadMissInDCache = Output(Bool())
  val robHeadTlbReplay = Output(Bool())
  val robHeadTlbMiss = Output(Bool())
  val robHeadLoadVio = Output(Bool())
  val robHeadLoadMSHR = Output(Bool())
}

class fetch_to_mem(implicit p: Parameters) extends XSBundle{
  val itlb = Flipped(new TlbPtwIO())
}

// triple buffer applied in i-mmio path (two at MemBlock, one at L2Top)
class InstrUncacheBuffer()(implicit p: Parameters) extends LazyModule with HasInstrMMIOConst {
  val node = new TLBufferNode(BufferParams.default, BufferParams.default, BufferParams.default, BufferParams.default, BufferParams.default)
  lazy val module = new InstrUncacheBufferImpl

  class InstrUncacheBufferImpl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.a <> BufferParams.default(BufferParams.default(in.a))
      in.d <> BufferParams.default(BufferParams.default(out.d))

      // only a.valid, a.ready, a.address can change
      // hoping that the rest would be optimized to keep MemBlock port unchanged after adding buffer
      out.a.bits.data := 0.U
      out.a.bits.mask := Fill(mmioBusBytes, 1.U(1.W))
      out.a.bits.opcode := 4.U // Get
      out.a.bits.size := log2Ceil(mmioBusBytes).U
      out.a.bits.source := 0.U
    }
  }
}

// triple buffer applied in L1I$-L2 path (two at MemBlock, one at L2Top)
class ICacheBuffer()(implicit p: Parameters) extends LazyModule {
  val node = new TLBufferNode(BufferParams.default, BufferParams.default, BufferParams.default, BufferParams.default, BufferParams.default)
  lazy val module = new ICacheBufferImpl

  class ICacheBufferImpl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.a <> BufferParams.default(BufferParams.default(in.a))
      in.d <> BufferParams.default(BufferParams.default(out.d))
    }
  }
}

// Frontend bus goes through MemBlock
class FrontendBridge()(implicit p: Parameters) extends LazyModule {
  val icache_node = LazyModule(new ICacheBuffer()).suggestName("icache").node// to keep IO port name
  val instr_uncache_node = LazyModule(new InstrUncacheBuffer()).suggestName("instr_uncache").node
  lazy val module = new LazyModuleImp(this) {
  }
}

class MemBlockInlined()(implicit p: Parameters) extends LazyModule
  with HasXSParameter {
  override def shouldBeInlined: Boolean = true

  val dcache = LazyModule(new DCacheWrapper())
  val uncache = LazyModule(new Uncache())
  val ptw = LazyModule(new L2TLBWrapper())
  val ptw_to_l2_buffer = if (!coreParams.softPTW) LazyModule(new TLBuffer) else null
  val l1d_to_l2_buffer = if (coreParams.dcacheParametersOpt.nonEmpty) LazyModule(new TLBuffer) else null
  val dcache_port = TLNameNode("dcache_client") // to keep dcache-L2 port name
  val l2_pf_sender_opt = coreParams.prefetcher.map(_ =>
    BundleBridgeSource(() => new PrefetchRecv)
  )
  val l3_pf_sender_opt = if (L3notEmpty && !p(EnableCHI)) coreParams.prefetcher.map(_ =>
    BundleBridgeSource(() => new PrefetchRecv)
  ) else None
  val cmo_sender  = if (HasCMO && !p(EnableCHI)) Some(BundleBridgeSource(() => DecoupledIO(new CMOReq))) else None
  val cmo_reciver = if (HasCMO && !p(EnableCHI)) Some(BundleBridgeSink(Some(() => DecoupledIO(new CMOResp)))) else None
  val frontendBridge = LazyModule(new FrontendBridge)
  // interrupt sinks
  val clint_int_sink = IntSinkNode(IntSinkPortSimple(1, 2))
  val debug_int_sink = IntSinkNode(IntSinkPortSimple(1, 1))
  val plic_int_sink = IntSinkNode(IntSinkPortSimple(2, 1))
  val nmi_int_sink = IntSinkNode(IntSinkPortSimple(1, (new NonmaskableInterruptIO).elements.size))

  if (!coreParams.softPTW) {
    ptw_to_l2_buffer.node := ptw.node
  }

  lazy val module = new MemBlockInlinedImp(this)
}

class MemBlockPMPWrapper(pmpCheckSize : Int, lgMaxSize : Int = 4)(implicit p: Parameters) extends XSModule with HasPMParameters {
  val io = IO(new Bundle {
    val distribute_csr = Input(new DistributedCSRIO())
    val pmpInfo = new Bundle {
      val pmp = Output(Vec(NumPMP, new PMPEntry()))
      val pma = Output(Vec(NumPMA, new PMPEntry()))
    }
    val privDmode = Input(UInt(2.W))
    val dtlbPmps = Vec(pmpCheckSize, Flipped(ValidIO(new PMPReqBundle(lgMaxSize))))
    val pmpCheckerResp = Vec(pmpCheckSize, new PMPRespBundle)
  })

  val pmp = Module(new PMP())
  val pmp_checkers = Seq.fill(pmpCheckSize)(Module(new PMPChecker(lgMaxSize, leaveHitMux = true)))

  for ((p, d) <- pmp_checkers zip io.dtlbPmps) {
    p.io.apply(io.privDmode, io.pmpInfo.pmp, io.pmpInfo.pma, d)
    require(p.io.req.bits.size.getWidth == d.bits.size.getWidth)
  }

  for ((p,res) <- pmp_checkers zip io.pmpCheckerResp){
    res := p.io.resp
  }

  io.pmpInfo.pmp := pmp.io.pmp
  io.pmpInfo.pma := pmp.io.pma
  pmp.io.distribute_csr := io.distribute_csr
}




class MemBlockInlinedImp(outer: MemBlockInlined) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasFPUParameters
  with HasPerfEvents
  with HasL1PrefetchSourceParameter
  with HasCircularQueuePtrHelper
  with HasMemBlockParameters
  with HasTlbConst
  with HasCSRConst
  with SdtrigExt
{
  val io = IO(new Bundle {
    val hartId = Input(UInt(hartIdLen.W))
    val redirect = Flipped(ValidIO(new Redirect))

    val ooo_to_mem = new ooo_to_mem
    val mem_to_ooo = new mem_to_ooo
    val fetch_to_mem = new fetch_to_mem

    val ifetchPrefetch = Vec(LduCnt, ValidIO(new SoftIfetchPrefetchBundle))

    // misc
    val error = ValidIO(new L1CacheErrorInfo)
    val memInfo = new Bundle {
      val sqFull = Output(Bool())
      val lqFull = Output(Bool())
      val dcacheMSHRFull = Output(Bool())
    }
    val debug_ls = new DebugLSIO
    val l2_hint = Input(Valid(new L2ToL1Hint()))
    val l2PfqBusy = Input(Bool())
    val l2_tlb_req = Flipped(new TlbRequestIO(nRespDups = 2))
    val l2_pmp_resp = new PMPRespBundle

    val debugTopDown = new Bundle {
      val robHeadVaddr = Flipped(Valid(UInt(VAddrBits.W)))
      val toCore = new MemCoreTopDownIO
    }
    val debugRolling = Flipped(new RobDebugRollingIO)

    // All the signals from/to frontend/backend to/from bus will go through MemBlock
    val fromTopToBackend = Input(new Bundle {
      val msiInfo   = ValidIO(new MsiInfoBundle)
      val clintTime = ValidIO(UInt(64.W))
    })
    val inner_hartId = Output(UInt(hartIdLen.W))
    val inner_reset_vector = Output(UInt(PAddrBits.W))
    val outer_reset_vector = Input(UInt(PAddrBits.W))
    val outer_cpu_halt = Output(Bool())
    val inner_beu_errors_icache = Input(new L1BusErrorUnitInfo)
    val outer_beu_errors_icache = Output(new L1BusErrorUnitInfo)
    val inner_l2_pf_enable = Input(Bool())
    val outer_l2_pf_enable = Output(Bool())
    val inner_hc_perfEvents = Output(Vec(numPCntHc * coreParams.L2NBanks + 1, new PerfEvent))
    val outer_hc_perfEvents = Input(Vec(numPCntHc * coreParams.L2NBanks + 1, new PerfEvent))
    val memPredUpdate = Input(new MemPredUpdateReq)

    val power = new Bundle{
      val flushSb = Input(Bool())
      val sbIsEmpty = Output(Bool())
    }

    // reset signals of frontend & backend are generated in memblock
    val reset_backend = Output(Reset())
    // Reset singal from frontend.
    val resetInFrontendBypass = new Bundle{
      val fromFrontend = Input(Bool())
      val toL2Top      = Output(Bool())
    }
    val dftBypass = new Bundle() {
      val fromL2Top = new Bundle() {
        val func      = Option.when(hasMbist)(Input(new SramBroadcastBundle))
        val reset = Option.when(hasMbist)(Input(new DFTResetSignals()))
      }
      val toFrontend = new Bundle() {
        val func      = Option.when(hasMbist)(Output(new SramBroadcastBundle))
        val reset = Option.when(hasMbist)(Output(new DFTResetSignals()))
      }
      val toBackend = new Bundle() {
        val func      = Option.when(hasMbist)(Output(new SramBroadcastBundle))
        val reset = Option.when(hasMbist)(Output(new DFTResetSignals()))
      }
    }
  })

  dontTouch(io.inner_hartId)
  dontTouch(io.inner_reset_vector)
  dontTouch(io.outer_reset_vector)
  dontTouch(io.outer_cpu_halt)
  dontTouch(io.inner_beu_errors_icache)
  dontTouch(io.outer_beu_errors_icache)
  dontTouch(io.inner_l2_pf_enable)
  dontTouch(io.outer_l2_pf_enable)
  dontTouch(io.inner_hc_perfEvents)
  dontTouch(io.outer_hc_perfEvents)

  require(StdCnt == 1)
  require(StaCnt == 1)

  val redirect = RegNextWithEnable(io.redirect)

  val redirectDupName = List("DTLB", "LoadUnit_0", "LoadUnit_1", "StoreUnit_0")
  val redirectReg = PipeDup("redirect", redirectDupName, io.redirect)

  private val dcache = outer.dcache.module
  val uncache = outer.uncache.module

  //val delayedDcacheRefill = RegNext(dcache.io.lsu.lsq)

  val csrCtrl = DelayN(io.ooo_to_mem.csrCtrl, 2)
  dcache.io.csr.distribute_csr <> csrCtrl.distribute_csr
  dcache.io.l2_pf_store_only := RegNext(io.ooo_to_mem.csrCtrl.l2_pf_store_only, false.B)
  io.error <> DelayNWithValid(dcache.io.error, 2)
  when(!csrCtrl.cache_error_enable){
    io.error.bits.report_to_beu := false.B
    io.error.valid := false.B
  }

  val mdp = Module(new NewMDP)
  mdp.io.reUpdate.valid := io.memPredUpdate.valid
  mdp.io.reUpdate.bits.stIdx := io.memPredUpdate.stIdx
  mdp.io.reUpdate.bits.ld_stIdx := io.memPredUpdate.ld_stIdx
  mdp.io.reUpdate.bits.ldFoldPc := io.memPredUpdate.ldFoldPc
  mdp.io.reUpdate.bits.stFoldPc := io.memPredUpdate.stFoldPc


  val loadUnits = Seq.fill(LduCnt)(Module(new LoadUnit))
  val storeUnits = Seq.fill(StaCnt)(Module(new StoreUnit))
  val stdExeUnits = Seq.fill(StdCnt)(Module(new MemExeUnit(backendParams.memSchdParams.get.issueBlockParams.find(_.StdCnt != 0).get.exuBlockParams.head)))
  val stData = stdExeUnits.map(_.io.out)
  val exeUnits = loadUnits ++ storeUnits
  // val vlWrapper = Module(new VectorLoadWrapper)
  // val vsUopQueue = Module(new VsUopQueue)
  // val vsFlowQueue = Module(new VsFlowQueue)

  // The number of vector load/store units is decoupled with the number of load/store units
  val vlSplit = Seq.fill(VlduCnt)(Module(new VLSplitImp))
  val vsSplit = Seq.fill(VstuCnt)(Module(new VSSplitImp))
  require(StaCnt == 1)

  val vlMergeBuffer = Module(new VLMergeBufferImp)
  val vsMergeBuffer = Seq.fill(VstuCnt)(Module(new VSMergeBufferImp))
  val vSegmentUnit  = Module(new VSegmentUnit)
  val vfofBuffer    = Module(new VfofBuffer)

  vfofBuffer.io := DontCare
  vlMergeBuffer.io := DontCare
  vsMergeBuffer.foreach(_.io := DontCare)
  vSegmentUnit.io := DontCare


  // misalign Buffer
  val loadMisalignBuffer = Module(new LoadMisalignBuffer)
  val storeMisalignBuffer = Module(new StoreMisalignBuffer)

  val l1_pf_req = Wire(Decoupled(new L1PrefetchReq()))
  dcache.io.sms_agt_evict_req.ready := false.B
  val prefetcherOpt: Option[BasePrefecher] = coreParams.prefetcher.map {
    case _: SMSParams =>
      val sms = Module(new SMSPrefetcher())
      sms.io_agt_en := GatedRegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_enable_agt, 2, Some(false.B))
      sms.io_pht_en := GatedRegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_enable_pht, 2, Some(false.B))
      sms.io_act_threshold := GatedRegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_active_threshold, 2, Some(12.U))
      sms.io_act_stride := GatedRegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_active_stride, 2, Some(30.U))
      sms.io_stride_en := false.B
      sms.io_dcache_evict <> dcache.io.sms_agt_evict_req
      val mbistPlSms = MbistPipeline.PlaceMbistPipeline(1, "MbistPipeSms", hasMbist)
      sms
  }
  prefetcherOpt.foreach{ pf => pf.io.l1_req.ready := false.B }
  val hartId = p(XSCoreParamsKey).HartId
  val l1PrefetcherOpt: Option[BasePrefecher] = coreParams.prefetcher.map {
    case _ =>
      val l1Prefetcher = Module(new L1Prefetcher())
      val enableL1StreamPrefetcher = Constantin.createRecord(s"enableL1StreamPrefetcher$hartId", initValue = true)
      l1Prefetcher.io.enable := enableL1StreamPrefetcher &&
        GatedRegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_enable, 2, Some(false.B))
      l1Prefetcher.pf_ctrl <> dcache.io.pf_ctrl
      l1Prefetcher.l2PfqBusy := io.l2PfqBusy

      // stride will train on miss or prefetch hit
      for (i <- 0 until LduCnt) {
        val source = loadUnits(i).io.prefetch_train_l1
        l1Prefetcher.stride_train(i).valid := source.valid && source.bits.isFirstIssue && (
          source.bits.miss || isFromStride(source.bits.meta_prefetch)
        )
        l1Prefetcher.stride_train(i).bits := source.bits
        l1Prefetcher.stride_train(i).bits.uop.pc := RegEnable(RegEnable(io.ooo_to_mem.loadPc(i), loadUnits(i).io.s1_prefetch_spec), loadUnits(i).io.s2_prefetch_spec)
      }
      l1Prefetcher
  }
  // load prefetch to l1 Dcache
  l1PrefetcherOpt match {
    case Some(pf) => l1_pf_req <> Pipeline(in = pf.io.l1_req, depth = 1, pipe = false, name = Some("pf_queue_to_ldu_reg"))
    case None =>
      l1_pf_req.valid := false.B
      l1_pf_req.bits := DontCare
  }
  val pf_train_on_hit = RegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_train_on_hit, 2, Some(true.B))

  loadUnits.zipWithIndex.map(x => x._1.suggestName("LoadUnit_"+x._2))
  storeUnits.zipWithIndex.map(x => x._1.suggestName("StoreUnit_"+x._2))
  val atomicsUnit = Module(new AtomicsUnit)

  val ldaWritebackOverride  = Mux(
    loadMisalignBuffer.io.writeBack.valid,
    loadMisalignBuffer.io.writeBack.bits,
    Mux(
      atomicsUnit.io.out.valid,
      atomicsUnit.io.out.bits,
      loadUnits.head.io.ldout.bits
    ))
  val ldaOut = Wire(Decoupled(new MemExuOutput))
  // misalignBuffer will overwrite the source from ldu if it is about to writeback
  ldaOut.valid := atomicsUnit.io.out.valid || loadUnits.head.io.ldout.valid || loadMisalignBuffer.io.writeBack.valid
  ldaOut.bits  := ldaWritebackOverride
  ldaOut.bits.isFromLoadUnit := !(atomicsUnit.io.out.valid || loadMisalignBuffer.io.writeBack.valid)
  atomicsUnit.io.out.ready := ldaOut.ready
  loadUnits.head.io.ldout.ready := ldaOut.ready
  loadMisalignBuffer.io.writeBack.ready := ldaOut.ready

  val ldaExeWbReqs = ldaOut +: loadUnits.tail.map(_.io.ldout)
  io.mem_to_ooo.writebackLda <> ldaExeWbReqs
  io.mem_to_ooo.writebackSta <> storeUnits.map(_.io.stout)
  io.mem_to_ooo.writebackStd.zip(stdExeUnits).foreach {x =>
    x._1.bits  := x._2.io.out.bits
    x._1.valid := x._2.io.out.fire
  }
  io.mem_to_ooo.otherFastWakeup := DontCare
  io.mem_to_ooo.otherFastWakeup.drop(HyuCnt).take(LduCnt).zip(loadUnits.map(_.io.fast_uop)).foreach{case(a,b)=> a := b}
  val stOut = io.mem_to_ooo.writebackSta ++ io.mem_to_ooo.writebackHyuSta

  // prefetch to l1 req
  // Stream's confidence is always 1
  // (LduCnt + HyuCnt) l1_pf_reqs ?
  loadUnits.foreach(load_unit => {
    load_unit.io.prefetch_req.valid <> l1_pf_req.valid
    load_unit.io.prefetch_req.bits <> l1_pf_req.bits
  })


  // NOTE: loadUnits(0) has higher bank conflict and miss queue arb priority than loadUnits(1) and loadUnits(2)
  // when loadUnits(1)/loadUnits(2) stage 0 is busy, hw prefetch will never use that pipeline
  val LowConfPorts = if (LduCnt == 2) Seq(1) else if (LduCnt == 3) Seq(1, 2) else Seq(0)
  LowConfPorts.map{case i => loadUnits(i).io.prefetch_req.bits.confidence := 0.U}

  val canAcceptHighConfPrefetch = loadUnits.map(_.io.canAcceptHighConfPrefetch)
  val canAcceptLowConfPrefetch = loadUnits.map(_.io.canAcceptLowConfPrefetch)
  l1_pf_req.ready := (0 until LduCnt + HyuCnt).map{
    case i => {
      if (LowConfPorts.contains(i)) {
        loadUnits(i).io.canAcceptLowConfPrefetch
      } else {
        Mux(l1_pf_req.bits.confidence === 1.U, canAcceptHighConfPrefetch(i), canAcceptLowConfPrefetch(i))
      }
    }
  }.reduce(_ || _)

  // l1 pf fuzzer interface
  val DebugEnableL1PFFuzzer = false
  if (DebugEnableL1PFFuzzer) {
    // l1 pf req fuzzer
    val fuzzer = Module(new L1PrefetchFuzzer())
    fuzzer.io.vaddr := DontCare
    fuzzer.io.paddr := DontCare

    // override load_unit prefetch_req
    loadUnits.foreach(load_unit => {
      load_unit.io.prefetch_req.valid <> fuzzer.io.req.valid
      load_unit.io.prefetch_req.bits <> fuzzer.io.req.bits
    })

    fuzzer.io.req.ready := l1_pf_req.ready
  }

  // TODO: fast load wakeup
  val lsq     = Module(new LsqWrapper)
  val sbuffer = Module(new Sbuffer)
  lsq.io := DontCare
  mdp.io.ldUpdate := lsq.io.mdpTrainUpdate
  io.mem_to_ooo.stIssuePtr := lsq.io.issuePtrExt
  io.mem_to_ooo.sqHasCmo := lsq.io.sqHasCmo

  dcache.io.hartId := io.hartId
  lsq.io.hartId := io.hartId
  sbuffer.io.hartId := io.hartId
  atomicsUnit.io.hartId := io.hartId

  dcache.io.lqEmpty := lsq.io.lqEmpty

  // load/store prefetch to l2 cache
  prefetcherOpt.foreach(sms_pf => {
    l1PrefetcherOpt.foreach(l1_pf => {
      val sms_pf_to_l2 = DelayNWithValid(sms_pf.io.l2_req, 2)
      val l1_pf_to_l2 = DelayNWithValid(l1_pf.io.l2_req, 2)

      outer.l2_pf_sender_opt.get.out.head._1.addr_valid := sms_pf_to_l2.valid || l1_pf_to_l2.valid
      outer.l2_pf_sender_opt.get.out.head._1.addr := Mux(l1_pf_to_l2.valid, l1_pf_to_l2.bits.addr, sms_pf_to_l2.bits.addr)
      outer.l2_pf_sender_opt.get.out.head._1.pf_source := Mux(l1_pf_to_l2.valid, l1_pf_to_l2.bits.source, sms_pf_to_l2.bits.source)
      outer.l2_pf_sender_opt.get.out.head._1.l2_pf_en := RegNextN(io.ooo_to_mem.csrCtrl.l2_pf_enable, 2, Some(true.B))

      sms_pf.io.enable := RegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_enable, 2, Some(false.B))

      val l2_trace = Wire(new LoadPfDbBundle)
      l2_trace.paddr := outer.l2_pf_sender_opt.get.out.head._1.addr
      val table = ChiselDB.createTable(s"L2PrefetchTrace$hartId", new LoadPfDbBundle, basicDB = false)
      table.log(l2_trace, l1_pf_to_l2.valid, "StreamPrefetchTrace", clock, reset)
      table.log(l2_trace, !l1_pf_to_l2.valid && sms_pf_to_l2.valid, "L2PrefetchTrace", clock, reset)

      val l1_pf_to_l3 = ValidIODelay(l1_pf.io.l3_req, 4)
      outer.l3_pf_sender_opt.foreach(_.out.head._1.addr_valid := l1_pf_to_l3.valid)
      outer.l3_pf_sender_opt.foreach(_.out.head._1.addr := l1_pf_to_l3.bits)
      outer.l3_pf_sender_opt.foreach(_.out.head._1.l2_pf_en := RegNextN(io.ooo_to_mem.csrCtrl.l2_pf_enable, 4, Some(true.B)))

      val l3_trace = Wire(new LoadPfDbBundle)
      l3_trace.paddr := outer.l3_pf_sender_opt.map(_.out.head._1.addr).getOrElse(0.U)
      val l3_table = ChiselDB.createTable(s"L3PrefetchTrace$hartId", new LoadPfDbBundle, basicDB = false)
      l3_table.log(l3_trace, l1_pf_to_l3.valid, "StreamPrefetchTrace", clock, reset)

      XSPerfAccumulate("prefetch_fire_l2", outer.l2_pf_sender_opt.get.out.head._1.addr_valid)
      XSPerfAccumulate("prefetch_fire_l3", outer.l3_pf_sender_opt.map(_.out.head._1.addr_valid).getOrElse(false.B))
      XSPerfAccumulate("l1pf_fire_l2", l1_pf_to_l2.valid)
      XSPerfAccumulate("sms_fire_l2", !l1_pf_to_l2.valid && sms_pf_to_l2.valid)
      XSPerfAccumulate("sms_block_by_l1pf", l1_pf_to_l2.valid && sms_pf_to_l2.valid)
    })
  })

  // ptw
  val sfence = RegNext(RegNext(io.ooo_to_mem.sfence))
  val tlbcsr = RegNext(RegNext(io.ooo_to_mem.tlbCsr))
  private val ptw = outer.ptw.module
  private val ptw_to_l2_buffer = outer.ptw_to_l2_buffer.module
  private val l1d_to_l2_buffer = outer.l1d_to_l2_buffer.module
  ptw.io.hartId := io.hartId
  ptw.io.sfence <> sfence
  ptw.io.csr.tlb <> tlbcsr
  ptw.io.csr.distribute_csr <> csrCtrl.distribute_csr

  val perfEventsPTW = Wire(Vec(19, new PerfEvent))
  if (!coreParams.softPTW) {
    perfEventsPTW := ptw.getPerf
  } else {
    perfEventsPTW := DontCare
  }

  // dtlb
  val DTLB_PORT_NUM = LduCnt + StaCnt + 1 + 2 //bop sms stride
  val DTLB_PORT_START_LD = 0
  val DTLB_PORT_START_ST = LduCnt
  val DTLB_PORT_START_BOP = LduCnt + StaCnt
  val DTLB_PORT_START_SMS = LduCnt + StaCnt + 1
  val DTLB_PORT_START_STRIDE = LduCnt + StaCnt + 2

  val dtlb_all = Module(new TLBNonBlock(DTLB_PORT_NUM, 2, dtlbParams))
  val dtlbIO = dtlb_all.io

  val DTlbSize = DTLB_PORT_NUM
  val ptwio = Wire(new VectorTlbPtwIO(DTlbSize))
  val dtlb_reqs = dtlbIO.requestor
  val dtlb_pmps = dtlbIO.pmp
  dtlbIO.hartId := io.hartId
  dtlbIO.sfence := sfence
  dtlbIO.csr := tlbcsr
  dtlbIO.flushPipe.map(a => a := false.B) // non-block doesn't need
  dtlbIO.redirect := redirectReg("DTLB")


  val ptw_resp_next = RegEnable(ptwio.resp.bits, ptwio.resp.valid)
  //for fanout
  val pte_resp_next_data_dup = ptw_resp_next.data

  val ptw_resp_v = RegNext(ptwio.resp.valid && !(sfence.valid && tlbcsr.satp.changed && tlbcsr.vsatp.changed && tlbcsr.hgatp.changed), init = false.B)
  ptwio.resp.ready := true.B

  val tlbreplay = WireInit(VecInit(Seq.fill(LdExuCnt)(false.B)))
  val tlbreplay_reg = GatedValidRegNext(tlbreplay)
  val dtlb_ld0_tlbreplay_reg = GatedValidRegNext(dtlbIO.tlbreplay)

  dontTouch(tlbreplay)
  for (i <- 0 until LdExuCnt) {
    tlbreplay(i) := dtlbIO.ptw.req(i).valid && ptw_resp_next.vector(0) && ptw_resp_v &&
      ptw_resp_next.data.hit(dtlbIO.ptw.req(i).bits.vpn, tlbcsr.satp.asid, tlbcsr.vsatp.asid, tlbcsr.hgatp.vmid, allType = true, ignoreAsid = true)
  }


  dtlbIO.ptw.req.map(r => r)
    .zipWithIndex
    .foreach{
      case (tlb, i) =>
        val vector_hit = Cat(ptw_resp_next.vector).orR
        tlb.ready := ptwio.req(i).ready
        ptwio.req(i).bits := tlb.bits
        ptwio.req(i).valid := tlb.valid && !(ptw_resp_v && vector_hit && pte_resp_next_data_dup.hit(tlb.bits.vpn, tlbcsr.satp.asid, tlbcsr.vsatp.asid, tlbcsr.hgatp.vmid, allType = true, ignoreAsid = true))
    }


  dtlbIO.ptw.resp.bits := pte_resp_next_data_dup
  dtlbIO.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector).orR
  dtlbIO.ptw.resp.bits.getGpa := false.B //todo tmp

  val dtlbRepeater  = PTWNewFilter(ldtlbParams.fenceDelay, ptwio, ptw.io.tlb(1), sfence, tlbcsr, l2tlbParams.dfilterSize)
  val itlbRepeater3 = PTWRepeaterNB(passReady = false, itlbParams.fenceDelay, io.fetch_to_mem.itlb, ptw.io.tlb(0), sfence, tlbcsr)

  lsq.io.debugTopDown.robHeadMissInDTlb := dtlbRepeater.io.rob_head_miss_in_tlb

  // pmp
  val pmpWrapper = Module(new MemBlockPMPWrapper(DTlbSize))
  pmpWrapper.io.privDmode := tlbcsr.priv.dmode
  pmpWrapper.io.distribute_csr <> csrCtrl.distribute_csr
  pmpWrapper.io.dtlbPmps := dtlb_pmps
  ptw.io.pmpInfo.pmp := pmpWrapper.io.pmpInfo.pmp
  ptw.io.pmpInfo.pma := pmpWrapper.io.pmpInfo.pma
  val pmpCheckResp = pmpWrapper.io.pmpCheckerResp

  for (i <- 0 until LduCnt) {
    io.debug_ls.debugLsInfo(i) := loadUnits(i).io.debug_ls
  }

  for (i <- 0 until StaCnt) {
    io.debug_ls.debugLsInfo.drop(LduCnt + HyuCnt)(i) := storeUnits(i).io.debug_ls
  }


  io.mem_to_ooo.lsTopdownInfo := loadUnits.map(_.io.lsTopdownInfo)

  // trigger
  val tdata = RegInit(VecInit(Seq.fill(TriggerNum)(0.U.asTypeOf(new MatchTriggerIO))))
  val tEnable = RegInit(VecInit(Seq.fill(TriggerNum)(false.B)))
  tEnable := csrCtrl.mem_trigger.tEnableVec
  when(csrCtrl.mem_trigger.tUpdate.valid) {
    tdata(csrCtrl.mem_trigger.tUpdate.bits.addr) := csrCtrl.mem_trigger.tUpdate.bits.tdata
  }
  val triggerCanRaiseBpExp = csrCtrl.mem_trigger.triggerCanRaiseBpExp
  val debugMode = csrCtrl.mem_trigger.debugMode

  val backendTriggerTimingVec = VecInit(tdata.map(_.timing))
  val backendTriggerChainVec = VecInit(tdata.map(_.chain))

  XSDebug(tEnable.asUInt.orR, "Debug Mode: At least one store trigger is enabled\n")
  for (j <- 0 until TriggerNum)
    PrintTriggerInfo(tEnable(j), tdata(j))

  // The segment instruction is executed atomically.
  // After the segment instruction directive starts executing, no other instructions should be executed.
  val vSegmentFlag = RegInit(false.B)
  val vSegmentFlag_dup = RegInit(false.B) //for fanout

  when(vSegmentUnit.io.in.fire){
    vSegmentFlag := true.B
    vSegmentFlag_dup := true.B
  }.elsewhen(vSegmentUnit.io.uopwriteback.valid){
    vSegmentFlag := false.B
    vSegmentFlag_dup := false.B
  }

  for (i <- 0 until LduCnt) {
    loadUnits(i).io.redirect <> redirectReg(s"LoadUnit_${i}")
    mdp.io.ldReq(i) := loadUnits(i).io.s0_reqMDP
    loadUnits(i).io.s1_respMDP := mdp.io.ldResp(i)
    // get input form dispatch
    loadUnits(i).io.ldin <> io.ooo_to_mem.issueLda(i)
    io.mem_to_ooo.ldaIqFeedback(i).feedbackSlow := loadUnits(i).io.feedback_slow
    io.mem_to_ooo.ldaIqFeedback(i).feedbackFast := loadUnits(i).io.feedback_fast
    io.mem_to_ooo.ldCancel.drop(HyuCnt)(i) := loadUnits(i).io.ldCancel
    io.mem_to_ooo.wakeup.drop(HyuCnt)(i) := loadUnits(i).io.wakeup

    // vector
    if (i < VlduCnt) {
      loadUnits(i).io.vecldout.ready := false.B
    } else {
      loadUnits(i).io.vecldin.valid := false.B
      loadUnits(i).io.vecldin.bits := DontCare
      loadUnits(i).io.vecldout.ready := false.B
    }

    // fast replay
    loadUnits(i).io.fast_rep_in <> loadUnits(i).io.fast_rep_out

    // SoftPrefetch to frontend (prefetch.i)
    io.ifetchPrefetch(i).valid <> RegNext(loadUnits(i).io.ifetchPrefetch.valid,false.B)
    io.ifetchPrefetch(i).bits <> RegEnable(loadUnits(i).io.ifetchPrefetch.bits,loadUnits(i).io.ifetchPrefetch.valid)

    // dcache access
    loadUnits(i).io.dcache <> dcache.io.lsu.load(i)
    if(i == 0){
      vSegmentUnit.io.rdcache := DontCare
      dcache.io.lsu.load(i).req.valid := (loadUnits(i).io.dcache.req.valid && !vSegmentUnit.io.in.fire) || vSegmentUnit.io.rdcache.req.valid
      dcache.io.lsu.load(i).req.bits  := Mux1H(Seq(
        vSegmentUnit.io.rdcache.req.valid -> vSegmentUnit.io.rdcache.req.bits,
        loadUnits(i).io.dcache.req.valid -> loadUnits(i).io.dcache.req.bits
      ))
      vSegmentUnit.io.rdcache.req.ready := dcache.io.lsu.load(i).req.ready
    }

    // Dcache requests must also be preempted by the segment.
    when(vSegmentFlag){
      loadUnits(i).io.dcache.req.ready             := false.B // Dcache is preempted.

      dcache.io.lsu.load(0).pf_source              := vSegmentUnit.io.rdcache.pf_source
      dcache.io.lsu.load(0).s1_paddr_dup_lsu       := vSegmentUnit.io.rdcache.s1_paddr_dup_lsu
      dcache.io.lsu.load(0).s1_paddr_dup_dcache    := vSegmentUnit.io.rdcache.s1_paddr_dup_dcache
      dcache.io.lsu.load(0).s1_kill                := vSegmentUnit.io.rdcache.s1_kill
      dcache.io.lsu.load(0).s2_kill                := vSegmentUnit.io.rdcache.s2_kill
      dcache.io.lsu.load(0).s0_pc                  := vSegmentUnit.io.rdcache.s0_pc
      dcache.io.lsu.load(0).s1_pc                  := vSegmentUnit.io.rdcache.s1_pc
      dcache.io.lsu.load(0).s2_pc                  := vSegmentUnit.io.rdcache.s2_pc
    }
    //for fanout
    when(!vSegmentFlag_dup){
      loadUnits(i).io.dcache.req.ready             := dcache.io.lsu.load(i).req.ready

      dcache.io.lsu.load(0).pf_source              := loadUnits(0).io.dcache.pf_source
      dcache.io.lsu.load(0).s1_paddr_dup_lsu       := loadUnits(0).io.dcache.s1_paddr_dup_lsu
      dcache.io.lsu.load(0).s1_paddr_dup_dcache    := loadUnits(0).io.dcache.s1_paddr_dup_dcache
      dcache.io.lsu.load(0).s1_kill                := loadUnits(0).io.dcache.s1_kill
      dcache.io.lsu.load(0).s2_kill                := loadUnits(0).io.dcache.s2_kill
      dcache.io.lsu.load(0).s0_pc                  := loadUnits(0).io.dcache.s0_pc
      dcache.io.lsu.load(0).s1_pc                  := loadUnits(0).io.dcache.s1_pc
      dcache.io.lsu.load(0).s2_pc                  := loadUnits(0).io.dcache.s2_pc
    }

    // forward
    loadUnits(i).io.lsq.forward <> lsq.io.forward(i)
    loadUnits(i).io.sbuffer <> sbuffer.io.forward(i)
    loadUnits(i).io.tl_d_channel := dcache.io.lsu.forward_D(i)
    loadUnits(i).io.forward_mshr <> dcache.io.lsu.forward_mshr(i)
    // ld-ld violation check
    loadUnits(i).io.lsq.ldld_nuke_query <> lsq.io.ldu.ldld_nuke_query(i)
    loadUnits(i).io.lsq.stld_nuke_query <> lsq.io.ldu.stld_nuke_query(i)
    loadUnits(i).io.lsq.mmio_paddr <> lsq.io.ldu.mmio_paddr(i)
    loadUnits(i).io.csrCtrl       <> csrCtrl
    // dtlb
    loadUnits(i).io.tlb <> dtlb_reqs.take(LduCnt)(i)
    if(i == 0 ){ // port 0 assign to vsegmentUnit
      val vsegmentDtlbReqValid = vSegmentUnit.io.dtlb.req.valid // segment tlb resquest need to delay 1 cycle
      dtlb_reqs.take(LduCnt)(i).req.valid := loadUnits(i).io.tlb.req.valid || RegNext(vsegmentDtlbReqValid)
      vSegmentUnit.io.dtlb.req.ready      := dtlb_reqs.take(LduCnt)(i).req.ready
      dtlb_reqs.take(LduCnt)(i).req.bits  := ParallelPriorityMux(Seq(
        RegNext(vsegmentDtlbReqValid)     -> RegEnable(vSegmentUnit.io.dtlb.req.bits, vsegmentDtlbReqValid),
        loadUnits(i).io.tlb.req.valid     -> loadUnits(i).io.tlb.req.bits
      ))
    }
    // pmp
    loadUnits(i).io.pmp <> pmpCheckResp(i)
    // st-ld violation query
    val stld_nuke_query = storeUnits.map(_.io.stld_nuke_query)
    for (s <- 0 until StorePipelineWidth) {
      loadUnits(i).io.stld_nuke_query(s) := stld_nuke_query(s)
    }
    // load prefetch train
    prefetcherOpt.foreach(pf => {
      // sms will train on all miss load sources
      val source = loadUnits(i).io.prefetch_train
      pf.io.ld_in(i).valid := Mux(pf_train_on_hit,
        source.valid,
        source.valid && source.bits.isFirstIssue && source.bits.miss
      )
      pf.io.ld_in(i).bits := source.bits
      pf.io.ld_in(i).bits.uop.pc := RegEnable(RegEnable(io.ooo_to_mem.loadPc(i), loadUnits(i).io.s1_prefetch_spec), loadUnits(i).io.s2_prefetch_spec)
    })
    l1PrefetcherOpt.foreach(pf => {
      // stream will train on all load sources
      val source = loadUnits(i).io.prefetch_train_l1
      pf.io.ld_in(i).valid := source.valid && source.bits.isFirstIssue
      pf.io.ld_in(i).bits := source.bits
    })

    loadUnits(i).io.l2l_fwd_in := DontCare
    loadUnits(i).io.replay <> lsq.io.replay(i)

    val l2_hint = RegNext(io.l2_hint)

    // L2 Hint for DCache
    dcache.io.l2_hint <> l2_hint

    loadUnits(i).io.tlb_hint.id := dtlbRepeater.io.hint.get.req(i).id
    loadUnits(i).io.tlb_hint.full := dtlbRepeater.io.hint.get.req(i).full ||
      tlbreplay_reg(i) || dtlb_ld0_tlbreplay_reg(i)

    // passdown to lsq (load s2)
    lsq.io.ldu.ldin(i) <> loadUnits(i).io.lsq.ldin
    (0 until LoadPipelineWidth).map ( i => {
      loadUnits(i).io.lsq.rob.pendingld  := io.ooo_to_mem.lsqio.pendingld
      loadUnits(i).io.lsq.rob.pendingPtr := io.ooo_to_mem.lsqio.pendingPtr
    })
    lsq.io.ldout <> loadUnits(0).io.lsq.uncache
    lsq.io.ld_raw_data <> loadUnits(0).io.lsq.ld_raw_data
    loadUnits(1).io.lsq.uncache := DontCare
    loadUnits(1).io.lsq.ld_raw_data := DontCare
    lsq.io.l2_hint.valid := l2_hint.valid
    lsq.io.l2_hint.bits.sourceId := l2_hint.bits.sourceId
    lsq.io.l2_hint.bits.isKeyword := l2_hint.bits.isKeyword

    lsq.io.tlb_hint <> dtlbRepeater.io.hint.get

    // connect misalignBuffer
    loadMisalignBuffer.io.req(i) <> loadUnits(i).io.misalign_buf

    if (i == 0) {
      loadUnits(i).io.misalign_ldin  <> loadMisalignBuffer.io.splitLoadReq
      loadUnits(i).io.misalign_ldout <> loadMisalignBuffer.io.splitLoadResp
    } else {
      loadUnits(i).io.misalign_ldin.valid := false.B
      loadUnits(i).io.misalign_ldin.bits := DontCare
    }

    // alter writeback exception info
    io.mem_to_ooo.s3_delayed_load_error(i) := loadUnits(i).io.s3_dly_ld_err

    // update mem dependency predictor
    // io.memPredUpdate(i) := DontCare

    // --------------------------------
    // Load Triggers
    // --------------------------------
    loadUnits(i).io.fromCsrTrigger.tdataVec := tdata
    loadUnits(i).io.fromCsrTrigger.tEnableVec := tEnable
    loadUnits(i).io.fromCsrTrigger.triggerCanRaiseBpExp := triggerCanRaiseBpExp
    loadUnits(i).io.fromCsrTrigger.debugMode := debugMode
  }

  // misalignBuffer
  loadMisalignBuffer.io.redirect                <> redirect
  loadMisalignBuffer.io.rob.lcommit             := io.ooo_to_mem.lsqio.lcommit
  loadMisalignBuffer.io.rob.scommit             := io.ooo_to_mem.lsqio.scommit
  loadMisalignBuffer.io.rob.pendingld           := io.ooo_to_mem.lsqio.pendingld
  loadMisalignBuffer.io.rob.pendingst           := io.ooo_to_mem.lsqio.pendingst
  loadMisalignBuffer.io.rob.pendingVst          := io.ooo_to_mem.lsqio.pendingVst
  loadMisalignBuffer.io.rob.commit              := io.ooo_to_mem.lsqio.commit
  loadMisalignBuffer.io.rob.pendingPtr          := io.ooo_to_mem.lsqio.pendingPtr
  loadMisalignBuffer.io.rob.pendingPtrNext      := io.ooo_to_mem.lsqio.pendingPtrNext

  lsq.io.flushFrmMaBuf                          := loadMisalignBuffer.io.flushLdExpBuff

  storeMisalignBuffer.io.redirect               <> redirect
  storeMisalignBuffer.io.rob.lcommit            := io.ooo_to_mem.lsqio.lcommit
  storeMisalignBuffer.io.rob.scommit            := io.ooo_to_mem.lsqio.scommit
  storeMisalignBuffer.io.rob.pendingld          := io.ooo_to_mem.lsqio.pendingld
  storeMisalignBuffer.io.rob.pendingst          := io.ooo_to_mem.lsqio.pendingst
  storeMisalignBuffer.io.rob.pendingVst         := io.ooo_to_mem.lsqio.pendingVst
  storeMisalignBuffer.io.rob.commit             := io.ooo_to_mem.lsqio.commit
  storeMisalignBuffer.io.rob.pendingPtr         := io.ooo_to_mem.lsqio.pendingPtr
  storeMisalignBuffer.io.rob.pendingPtrNext     := io.ooo_to_mem.lsqio.pendingPtrNext

  lsq.io.maControl                              <> storeMisalignBuffer.io.sqControl

  // // lsq to l2 CMO
  // outer.cmo_sender match {
  //   case Some(x) =>
  //     x.out.head._1 <> lsq.io.cmoOpReq
  //   case None =>
  //     lsq.io.cmoOpReq.ready  := false.B
  // }
  // outer.cmo_reciver match {
  //   case Some(x) =>
  //     x.in.head._1  <> lsq.io.cmoOpResp
  //   case None =>
  //     lsq.io.cmoOpResp.valid := false.B
  //     lsq.io.cmoOpResp.bits  := 0.U.asTypeOf(new CMOResp)
  // }

  // cmoreq from sq send to MissQueue
  lsq.io.cmoOpReq <> dcache.io.cmoOpReq
  // lsq.io.cmoOpResp <> dcache.io.cmoOpResp

  // Prefetcher
//  val StreamDTLBPortIndex = TlbStartVec(dtlb_ld_idx) + LduCnt + HyuCnt
//  val PrefetcherDTLBPortIndex = TlbStartVec(dtlb_pf_idx)
//  val L2toL1DLBPortIndex = TlbStartVec(dtlb_pf_idx) + 1
  prefetcherOpt match {
  case Some(pf) => 
    dtlb_reqs(DTLB_PORT_START_SMS) <> pf.io.tlb_req
    pf.io.pmp_resp := pmpCheckResp(DTLB_PORT_START_SMS)
  case None =>
    dtlb_reqs(DTLB_PORT_START_SMS) := DontCare
    dtlb_reqs(DTLB_PORT_START_SMS).req.valid := false.B
    dtlb_reqs(DTLB_PORT_START_SMS).resp.ready := true.B
  }
  l1PrefetcherOpt match {
    case Some(pf) => 
      dtlb_reqs(DTLB_PORT_START_STRIDE) <> pf.io.tlb_req
      pf.io.pmp_resp := pmpCheckResp(DTLB_PORT_START_STRIDE)
    case None =>
        dtlb_reqs(DTLB_PORT_START_STRIDE) := DontCare
        dtlb_reqs(DTLB_PORT_START_STRIDE).req.valid := false.B
        dtlb_reqs(DTLB_PORT_START_STRIDE).resp.ready := true.B
  }
  dtlb_reqs(DTLB_PORT_START_BOP) <> io.l2_tlb_req
  dtlb_reqs(DTLB_PORT_START_BOP).resp.ready := true.B
  io.l2_pmp_resp := pmpCheckResp(DTLB_PORT_START_BOP)

  // StoreUnit
  for (i <- 0 until StdCnt) {
    stdExeUnits(i).io.flush <> redirect
    stdExeUnits(i).io.in.valid := io.ooo_to_mem.issueStd(i).valid
    io.ooo_to_mem.issueStd(i).ready := stdExeUnits(i).io.in.ready
    stdExeUnits(i).io.in.bits := io.ooo_to_mem.issueStd(i).bits
  }

  for (i <- 0 until StaCnt) {
    val stu = storeUnits(i)

    stu.io.redirect      <> redirectReg(s"StoreUnit_${i}")
    stu.io.csrCtrl       <> csrCtrl
    stu.io.dcache        <> dcache.io.lsu.sta(i)
    stu.io.feedback_slow <> io.mem_to_ooo.staIqFeedback(i).feedbackSlow
    stu.io.stin         <> io.ooo_to_mem.issueSta(i)
    stu.io.lsq          <> lsq.io.sta.storeAddrIn(i)
    stu.io.lsq_replenish <> lsq.io.sta.storeAddrInRe(i)
    // dtlb
    stu.io.tlb          <> dtlbIO.requestor(DTLB_PORT_START_ST + i)
    stu.io.pmp          <> pmpCheckResp(DTLB_PORT_START_ST + i)

    // -------------------------
    // Store Triggers
    // -------------------------
    stu.io.fromCsrTrigger.tdataVec := tdata
    stu.io.fromCsrTrigger.tEnableVec := tEnable
    stu.io.fromCsrTrigger.triggerCanRaiseBpExp := triggerCanRaiseBpExp
    stu.io.fromCsrTrigger.debugMode := debugMode

    // prefetch
    stu.io.prefetch_req <> sbuffer.io.store_prefetch(i)

    // store unit does not need fast feedback
    io.mem_to_ooo.staIqFeedback(i).feedbackFast := DontCare

    // Lsq to sta unit
    lsq.io.sta.storeMaskIn(i) <> stu.io.st_mask_out

    // connect misalignBuffer
    storeMisalignBuffer.io.req(i) <> stu.io.misalign_buf

    if (i == 0) {
      stu.io.misalign_stin  <> storeMisalignBuffer.io.splitStoreReq
      stu.io.misalign_stout <> storeMisalignBuffer.io.splitStoreResp
    } else {
      stu.io.misalign_stin.valid := false.B
      stu.io.misalign_stin.bits := DontCare
    }

    // Lsq to std unit's rs
    if (i < VstuCnt){
      when (vsSplit(i).io.vstd.get.valid) {
        lsq.io.std.storeDataIn(i).valid := true.B
        lsq.io.std.storeDataIn(i).bits := vsSplit(i).io.vstd.get.bits
        stData(i).ready := false.B
      }.otherwise {
        lsq.io.std.storeDataIn(i).valid := stData(i).valid
        lsq.io.std.storeDataIn(i).bits.uop := stData(i).bits.uop
        lsq.io.std.storeDataIn(i).bits.data := stData(i).bits.data
        lsq.io.std.storeDataIn(i).bits.mask.map(_ := 0.U)
        lsq.io.std.storeDataIn(i).bits.vdIdx.map(_ := 0.U)
        lsq.io.std.storeDataIn(i).bits.vdIdxInField.map(_ := 0.U)
        stData(i).ready := true.B
      }
    } else {
        lsq.io.std.storeDataIn(i).valid := stData(i).valid
        lsq.io.std.storeDataIn(i).bits.uop := stData(i).bits.uop
        lsq.io.std.storeDataIn(i).bits.data := stData(i).bits.data
        lsq.io.std.storeDataIn(i).bits.mask.map(_ := 0.U)
        lsq.io.std.storeDataIn(i).bits.vdIdx.map(_ := 0.U)
        lsq.io.std.storeDataIn(i).bits.vdIdxInField.map(_ := 0.U)
        stData(i).ready := true.B
    }
    lsq.io.std.storeDataIn.map(_.bits.debug := 0.U.asTypeOf(new DebugBundle))
    lsq.io.std.storeDataIn.foreach(_.bits.isFromLoadUnit := DontCare)


    // store prefetch train
    l1PrefetcherOpt.foreach(pf => {
      // stream will train on all load sources
      pf.io.st_in(i).valid := false.B
      pf.io.st_in(i).bits := DontCare
    })

    prefetcherOpt.foreach(pf => {
      pf.io.st_in(i).valid := Mux(pf_train_on_hit,
        stu.io.prefetch_train.valid,
        stu.io.prefetch_train.valid && stu.io.prefetch_train.bits.isFirstIssue && (
          stu.io.prefetch_train.bits.miss
          )
      )
      pf.io.st_in(i).bits := stu.io.prefetch_train.bits
      pf.io.st_in(i).bits.uop.pc := RegEnable(RegEnable(io.ooo_to_mem.storePc(i), stu.io.s1_prefetch_spec), stu.io.s2_prefetch_spec)
    })

    io.mem_to_ooo.stIn(i).valid := stu.io.issue.valid
    io.mem_to_ooo.stIn(i).bits := stu.io.issue.bits

    stu.io.stout.ready := true.B

    // vector
    if (i < VstuCnt) {
      stu.io.vecstin <> DontCare
      // vsFlowQueue.io.pipeFeedback(i) <> stu.io.vec_feedback_slow // need connect
    } else {
      stu.io.vecstin.valid := false.B
      stu.io.vecstin.bits := DontCare
      stu.io.vecstout.ready := false.B
    }
    stu.io.vec_isFirstIssue := true.B // TODO
  }

  // mmio/cbo and cbozero store writeback will use store writeback port 0
  // mmio store writeback will use store writeback port 0
  val mmioStout = WireInit(0.U.asTypeOf(lsq.io.mmioStout))
  NewPipelineConnect(
    lsq.io.mmioStout, mmioStout, mmioStout.fire,
    false.B,
    Option("mmioStOutConnect")
  )
  mmioStout.ready := false.B
  when (mmioStout.valid && !storeUnits(0).io.stout.valid) {
    stOut(0).valid := true.B
    stOut(0).bits  := mmioStout.bits
    mmioStout.ready := true.B
  }
  // vec mmio writeback
  lsq.io.vecmmioStout.ready := false.B
  when (lsq.io.vecmmioStout.valid && !storeUnits(0).io.vecstout.valid) {
    stOut(0).valid := true.B
    stOut(0).bits  := lsq.io.vecmmioStout.bits
    lsq.io.vecmmioStout.ready := true.B
  }
  // miss align buffer will overwrite stOut(0)
  storeMisalignBuffer.io.writeBack.ready := true.B
  when (storeMisalignBuffer.io.writeBack.valid) {
    stOut(0).valid := true.B
    stOut(0).bits  := storeMisalignBuffer.io.writeBack.bits
  }

  // Uncache
  uncache.io.hartId := io.hartId

  // Lsq
  io.mem_to_ooo.lsqio.mmio       := lsq.io.rob.mmio
  io.mem_to_ooo.lsqio.uop        := lsq.io.rob.uop
  lsq.io.rob.lcommit             := io.ooo_to_mem.lsqio.lcommit
  lsq.io.rob.scommit             := io.ooo_to_mem.lsqio.scommit
  lsq.io.rob.pendingld           := io.ooo_to_mem.lsqio.pendingld
  lsq.io.rob.pendingst           := io.ooo_to_mem.lsqio.pendingst
  lsq.io.rob.pendingVst          := io.ooo_to_mem.lsqio.pendingVst
  lsq.io.rob.commit              := io.ooo_to_mem.lsqio.commit
  lsq.io.rob.pendingPtr          := io.ooo_to_mem.lsqio.pendingPtr
  lsq.io.rob.pendingPtrNext      := io.ooo_to_mem.lsqio.pendingPtrNext

  //  lsq.io.rob            <> io.lsqio.rob
  lsq.io.enq            <> io.ooo_to_mem.enqLsq
  lsq.io.redirect := io.redirect

  //  violation rollback
  def selectOldestRedirect(xs: Seq[Valid[Redirect]]): Vec[Bool] = {
    val compareVec = (0 until xs.length).map(i => (0 until i).map(j => isAfter(xs(j).bits.robIdx, xs(i).bits.robIdx)))
    val resultOnehot = VecInit((0 until xs.length).map(i => Cat((0 until xs.length).map(j =>
      (if (j < i) !xs(j).valid || compareVec(i)(j)
      else if (j == i) xs(i).valid
      else !xs(j).valid || !compareVec(j)(i))
    )).andR))
    resultOnehot
  }
  val allRedirect = loadUnits.map(_.io.rollback) ++ lsq.io.nuke_rollback
  val oldestOneHot = selectOldestRedirect(allRedirect)
  val oldestRedirect = WireDefault(Mux1H(oldestOneHot, allRedirect))
  // memory replay would not cause IAF/IPF/IGPF
  oldestRedirect.bits.cfiUpdate.backendIAF := false.B
  oldestRedirect.bits.cfiUpdate.backendIPF := false.B
  oldestRedirect.bits.cfiUpdate.backendIGPF := false.B
  io.mem_to_ooo.memoryViolation := oldestRedirect
  io.mem_to_ooo.lsqio.lqCanAccept  := lsq.io.lqCanAccept
  io.mem_to_ooo.lsqio.sqCanAccept  := lsq.io.sqCanAccept

  lsq.io.uncache        <> uncache.io.lsq
  lsq.io.release        := dcache.io.lsu.release
  lsq.io.lqCancelCnt <> io.mem_to_ooo.lqCancelCnt
  lsq.io.sqCancelCnt <> io.mem_to_ooo.sqCancelCnt
  lsq.io.lqDeq <> io.mem_to_ooo.lqDeq
  lsq.io.sqDeq <> io.mem_to_ooo.sqDeq
  // Todo: assign these
  io.mem_to_ooo.sqDeqPtr := lsq.io.sqDeqPtr
  io.mem_to_ooo.lqDeqPtr := lsq.io.lqDeqPtr
  lsq.io.tl_d_channel <> dcache.io.lsu.tl_d_channel

  // LSQ to store buffer
  lsq.io.sbuffer        <> sbuffer.io.in
  sbuffer.io.in(0).valid := lsq.io.sbuffer(0).valid || vSegmentUnit.io.sbuffer.valid
  sbuffer.io.in(0).bits  := Mux1H(Seq(
    vSegmentUnit.io.sbuffer.valid -> vSegmentUnit.io.sbuffer.bits,
    lsq.io.sbuffer(0).valid       -> lsq.io.sbuffer(0).bits
  ))
  vSegmentUnit.io.sbuffer.ready := sbuffer.io.in(0).ready
  lsq.io.sqEmpty        <> sbuffer.io.sqempty
  dcache.io.force_write := lsq.io.force_write

  // Initialize when unenabled difftest.
  sbuffer.io.vecDifftestInfo      := DontCare
  lsq.io.sbufferVecDifftestInfo   := DontCare
  vSegmentUnit.io.vecDifftestInfo := DontCare
  if (env.EnableDifftest) {
    sbuffer.io.vecDifftestInfo .zipWithIndex.map{ case (sbufferPort, index) =>
      if (index == 0) {
        val vSegmentDifftestValid = vSegmentUnit.io.vecDifftestInfo.valid
        sbufferPort.valid := Mux(vSegmentDifftestValid, vSegmentUnit.io.vecDifftestInfo.valid, lsq.io.sbufferVecDifftestInfo(0).valid)
        sbufferPort.bits  := Mux(vSegmentDifftestValid, vSegmentUnit.io.vecDifftestInfo.bits, lsq.io.sbufferVecDifftestInfo(0).bits)

        vSegmentUnit.io.vecDifftestInfo.ready  := sbufferPort.ready
        lsq.io.sbufferVecDifftestInfo(0).ready := sbufferPort.ready
      } else {
         sbufferPort <> lsq.io.sbufferVecDifftestInfo(index)
      }
    }
  }

  // vector
  val vLoadCanAccept  = (0 until VlduCnt).map(i =>
    vlSplit(i).io.in.ready && VlduType.isVecLd(io.ooo_to_mem.issueVldu(i).bits.uop.fuOpType)
  )
  val vStoreCanAccept = (0 until VstuCnt).map(i =>
    vsSplit(i).io.in.ready && VstuType.isVecSt(io.ooo_to_mem.issueVldu(i).bits.uop.fuOpType)
  )
  val isSegment     = io.ooo_to_mem.issueVldu.head.valid && isVsegls(io.ooo_to_mem.issueVldu.head.bits.uop.fuType)
  val isFixVlUop    = io.ooo_to_mem.issueVldu.map{x =>
    x.bits.uop.vpu.isVleff && x.bits.uop.vpu.lastUop && x.valid
  }

  // init port
  /**
   * TODO: splited vsMergebuffer maybe remove, if one RS can accept two feedback, or don't need RS replay uop
   * for now:
   *  RS0 -> VsSplit0 -> stu0 -> vsMergebuffer0 -> feedback -> RS0
   *  RS1 -> VsSplit1 -> stu1 -> vsMergebuffer1 -> feedback -> RS1
   *
   * vector load don't need feedback
   *
   *  RS0 -> VlSplit0  -> ldu0 -> |
   *  RS1 -> VlSplit1  -> ldu1 -> |  -> vlMergebuffer
   *        replayIO   -> ldu3 -> |
   * */
  (0 until VstuCnt).foreach{i =>
    vsMergeBuffer(i).io.fromPipeline := DontCare
    vsMergeBuffer(i).io.fromSplit := DontCare
  }

  (0 until VstuCnt).foreach{i =>
    vsSplit(i).io.redirect <> redirect
    vsSplit(i).io.in <> io.ooo_to_mem.issueVldu(i)
    vsSplit(i).io.in.valid := io.ooo_to_mem.issueVldu(i).valid &&
                              vStoreCanAccept(i) && !isSegment
    vsSplit(i).io.toMergeBuffer <> vsMergeBuffer(i).io.fromSplit.head
    NewPipelineConnect(
      vsSplit(i).io.out, storeUnits(i).io.vecstin, storeUnits(i).io.vecstin.fire,
      Mux(vsSplit(i).io.out.fire, vsSplit(i).io.out.bits.uop.robIdx.needFlush(io.redirect), storeUnits(i).io.vecstin.bits.uop.robIdx.needFlush(io.redirect)),
      Option("VsSplitConnectStu")
    )
    vsSplit(i).io.vstd.get := DontCare // Todo: Discuss how to pass vector store data

  }
  (0 until VlduCnt).foreach{i =>
    vlSplit(i).io.redirect <> redirect
    vlSplit(i).io.in <> io.ooo_to_mem.issueVldu(i)
    vlSplit(i).io.in.valid := io.ooo_to_mem.issueVldu(i).valid &&
                              vLoadCanAccept(i) && !isSegment && !isFixVlUop(i)
    vlSplit(i).io.toMergeBuffer <> vlMergeBuffer.io.fromSplit(i)
    NewPipelineConnect(
      vlSplit(i).io.out, loadUnits(i).io.vecldin, loadUnits(i).io.vecldin.fire,
      Mux(vlSplit(i).io.out.fire, vlSplit(i).io.out.bits.uop.robIdx.needFlush(io.redirect), loadUnits(i).io.vecldin.bits.uop.robIdx.needFlush(io.redirect)),
      Option("VlSplitConnectLdu")
    )

    //Subsequent instrction will be blocked
    vfofBuffer.io.in(i).valid := io.ooo_to_mem.issueVldu(i).valid
    vfofBuffer.io.in(i).bits  := io.ooo_to_mem.issueVldu(i).bits
  }
  (0 until LduCnt).foreach{i=>
    vlMergeBuffer.io.fromPipeline(i) <> loadUnits(i).io.vecldout
  }

  (0 until StaCnt).foreach{i=>
    if(i < VstuCnt){
      vsMergeBuffer(i).io.fromPipeline.head <> storeUnits(i).io.vecstout
    }
  }

  (0 until VlduCnt).foreach{i=>
    io.ooo_to_mem.issueVldu(i).ready := vLoadCanAccept(i) || vStoreCanAccept(i)
  }

  vlMergeBuffer.io.redirect <> redirect
  vsMergeBuffer.map(_.io.redirect <> redirect)
  (0 until VlduCnt).foreach{i=>
    vlMergeBuffer.io.toLsq(i) <> lsq.io.ldvecFeedback(i)
  }
  (0 until VstuCnt).foreach{i=>
    vsMergeBuffer(i).io.toLsq.head <> lsq.io.stvecFeedback(i)
  }

  (0 until VlduCnt).foreach{i=>
    // send to RS
    vlMergeBuffer.io.feedback(i) <> io.mem_to_ooo.vlduIqFeedback(i).feedbackSlow
    io.mem_to_ooo.vlduIqFeedback(i).feedbackFast := DontCare
  }
  (0 until VstuCnt).foreach{i =>
    // send to RS
    if (i == 0){
      io.mem_to_ooo.vstuIqFeedback(i).feedbackSlow.valid := vsMergeBuffer(i).io.feedback.head.valid || vSegmentUnit.io.feedback.valid
      io.mem_to_ooo.vstuIqFeedback(i).feedbackSlow.bits := Mux1H(Seq(
        vSegmentUnit.io.feedback.valid -> vSegmentUnit.io.feedback.bits,
        vsMergeBuffer(i).io.feedback.head.valid ->  vsMergeBuffer(i).io.feedback.head.bits
      ))
      io.mem_to_ooo.vstuIqFeedback(i).feedbackFast := DontCare
    } else {
      vsMergeBuffer(i).io.feedback.head <> io.mem_to_ooo.vstuIqFeedback(i).feedbackSlow
      io.mem_to_ooo.vstuIqFeedback(i).feedbackFast := DontCare
    }
  }

  (0 until VlduCnt).foreach{i=>
    if (i == 0){ // for segmentUnit, segmentUnit use port0 writeback
      io.mem_to_ooo.writebackVldu(i).valid := vlMergeBuffer.io.uopWriteback(i).valid || vsMergeBuffer(i).io.uopWriteback.head.valid || vSegmentUnit.io.uopwriteback.valid || vfofBuffer.io.uopWriteback.valid
      io.mem_to_ooo.writebackVldu(i).bits := PriorityMux(Seq(
        vSegmentUnit.io.uopwriteback.valid          -> vSegmentUnit.io.uopwriteback.bits,
        vlMergeBuffer.io.uopWriteback(i).valid      -> vlMergeBuffer.io.uopWriteback(i).bits,
        vsMergeBuffer(i).io.uopWriteback.head.valid -> vsMergeBuffer(i).io.uopWriteback.head.bits,
        vfofBuffer.io.uopWriteback.valid            -> vfofBuffer.io.uopWriteback.bits,
      ))
      vSegmentUnit.io.uopwriteback.ready := io.mem_to_ooo.writebackVldu(i).ready
      vlMergeBuffer.io.uopWriteback(i).ready := io.mem_to_ooo.writebackVldu(i).ready && !vSegmentUnit.io.uopwriteback.valid
      vsMergeBuffer(i).io.uopWriteback.head.ready := io.mem_to_ooo.writebackVldu(i).ready && !vlMergeBuffer.io.uopWriteback(i).valid && !vSegmentUnit.io.uopwriteback.valid
      vfofBuffer.io.uopWriteback.ready := io.mem_to_ooo.writebackVldu(i).ready && !vlMergeBuffer.io.uopWriteback(i).valid && !vSegmentUnit.io.uopwriteback.valid && !vsMergeBuffer(i).io.uopWriteback.head.valid
    } else if (i == 1) {
      io.mem_to_ooo.writebackVldu(i).valid := vlMergeBuffer.io.uopWriteback(i).valid || vsMergeBuffer(i).io.uopWriteback.head.valid || vfofBuffer.io.uopWriteback.valid
      io.mem_to_ooo.writebackVldu(i).bits := PriorityMux(Seq(
        vfofBuffer.io.uopWriteback.valid            -> vfofBuffer.io.uopWriteback.bits,
        vlMergeBuffer.io.uopWriteback(i).valid      -> vlMergeBuffer.io.uopWriteback(i).bits,
        vsMergeBuffer(i).io.uopWriteback.head.valid -> vsMergeBuffer(i).io.uopWriteback.head.bits,
      ))
      vlMergeBuffer.io.uopWriteback(i).ready := io.mem_to_ooo.writebackVldu(i).ready && !vfofBuffer.io.uopWriteback.valid
      vsMergeBuffer(i).io.uopWriteback.head.ready := io.mem_to_ooo.writebackVldu(i).ready && !vlMergeBuffer.io.uopWriteback(i).valid && !vfofBuffer.io.uopWriteback.valid
      vfofBuffer.io.uopWriteback.ready := io.mem_to_ooo.writebackVldu(i).ready
    } else {
      io.mem_to_ooo.writebackVldu(i).valid := vlMergeBuffer.io.uopWriteback(i).valid || vsMergeBuffer(i).io.uopWriteback.head.valid
      io.mem_to_ooo.writebackVldu(i).bits := PriorityMux(Seq(
        vlMergeBuffer.io.uopWriteback(i).valid -> vlMergeBuffer.io.uopWriteback(i).bits,
        vsMergeBuffer(i).io.uopWriteback.head.valid -> vsMergeBuffer(i).io.uopWriteback.head.bits,
      ))
      vlMergeBuffer.io.uopWriteback(i).ready := io.mem_to_ooo.writebackVldu(i).ready
      vsMergeBuffer(i).io.uopWriteback.head.ready := io.mem_to_ooo.writebackVldu(i).ready && !vlMergeBuffer.io.uopWriteback(i).valid
    }

    vfofBuffer.io.mergeUopWriteback(i).valid := vlMergeBuffer.io.uopWriteback(i).valid
    vfofBuffer.io.mergeUopWriteback(i).bits  := vlMergeBuffer.io.uopWriteback(i).bits
  }


  vfofBuffer.io.redirect <> redirect

  // Sbuffer
  sbuffer.io.csrCtrl    <> csrCtrl
  sbuffer.io.dcache     <> dcache.io.lsu.store
  sbuffer.io.memSetPattenDetected := dcache.io.memSetPattenDetected
  sbuffer.io.force_write <> lsq.io.force_write
  // flush sbuffer
  val cmoFlush = lsq.io.flushSbuffer.valid
  val fenceFlush = io.ooo_to_mem.flushSb
  val shutOffFlush = io.power.flushSb
  val atomicsFlush = atomicsUnit.io.flush_sbuffer.valid || vSegmentUnit.io.flush_sbuffer.valid
  val stIsEmpty = sbuffer.io.flush.empty && uncache.io.flush.empty
  io.mem_to_ooo.sbIsEmpty := RegNext(stIsEmpty)
  io.power.sbIsEmpty := RegNext(stIsEmpty)
  io.mem_to_ooo.cmoFinish := dcache.io.cmofinish //todo

  // if both of them tries to flush sbuffer at the same time
  // something must have gone wrong
  assert(!(fenceFlush && atomicsFlush && cmoFlush))
  sbuffer.io.flush.valid := RegNext(fenceFlush || atomicsFlush || cmoFlush || shutOffFlush)
  uncache.io.flush.valid := sbuffer.io.flush.valid

  // AtomicsUnit: AtomicsUnit will override other control signials,
  // as atomics insts (LR/SC/AMO) will block the pipeline
  val s_normal +: s_atomics = Enum(StaCnt + HyuCnt + 1)
  val state = RegInit(s_normal)

  val st_atomics = Seq.tabulate(StaCnt)(i =>
    io.ooo_to_mem.issueSta(i).valid && FuType.storeIsAMO((io.ooo_to_mem.issueSta(i).bits.uop.fuType))
  ) ++ Seq.tabulate(HyuCnt)(i =>
    io.ooo_to_mem.issueHya(i).valid && FuType.storeIsAMO((io.ooo_to_mem.issueHya(i).bits.uop.fuType))
  )

  val st_data_atomics = Seq.tabulate(StdCnt)(i =>
    stData(i).valid && FuType.storeIsAMO(stData(i).bits.uop.fuType)
  )

  for (i <- 0 until StaCnt) when(st_atomics(i)) {
    io.ooo_to_mem.issueSta(i).ready := atomicsUnit.io.in.ready
    storeUnits(i).io.stin.valid := false.B

    state := s_atomics(i)
  }
  for (i <- 0 until HyuCnt) when(st_atomics(StaCnt + i)) {
    io.ooo_to_mem.issueHya(i).ready := atomicsUnit.io.in.ready

    state := s_atomics(StaCnt + i)
    assert(!st_atomics.zipWithIndex.filterNot(_._2 == StaCnt + i).unzip._1.reduce(_ || _))
  }
  when (atomicsUnit.io.out.valid) {
    assert((0 until StaCnt + HyuCnt).map(state === s_atomics(_)).reduce(_ || _))
    state := s_normal
  }

  atomicsUnit.io.in.valid := st_atomics.reduce(_ || _)
  atomicsUnit.io.in.bits  := Mux1H(Seq.tabulate(StaCnt)(i =>
    st_atomics(i) -> io.ooo_to_mem.issueSta(i).bits) ++
    Seq.tabulate(HyuCnt)(i => st_atomics(StaCnt+i) -> io.ooo_to_mem.issueHya(i).bits))
  atomicsUnit.io.storeDataIn.valid := st_data_atomics.reduce(_ || _)
  atomicsUnit.io.storeDataIn.bits  := Mux1H(Seq.tabulate(StdCnt)(i =>
    st_data_atomics(i) -> stData(i).bits))
  atomicsUnit.io.redirect <> redirect

  // TODO: complete amo's pmp support
  val amoTlb = dtlbIO.requestor(DTLB_PORT_START_LD)
  atomicsUnit.io.dtlb.resp.valid := false.B
  atomicsUnit.io.dtlb.resp.bits  := DontCare
  atomicsUnit.io.dtlb.req.ready  := amoTlb.req.ready
  atomicsUnit.io.pmpResp := pmpCheckResp(0)

  atomicsUnit.io.dcache <> dcache.io.lsu.atomics
  atomicsUnit.io.flush_sbuffer.empty := stIsEmpty

  atomicsUnit.io.csrCtrl := csrCtrl

  // for atomicsUnit, it uses loadUnit(0)'s TLB port

  when (state =/= s_normal) {
    // use store wb port instead of load
    loadUnits(0).io.ldout.ready := false.B
    // use load_0's TLB
    atomicsUnit.io.dtlb <> amoTlb

    // hw prefetch should be disabled while executing atomic insts
    loadUnits.map(i => i.io.prefetch_req.valid := false.B)

    // make sure there's no in-flight uops in load unit
    assert(!loadUnits(0).io.ldout.valid)
  }

  lsq.io.flushSbuffer.empty := sbuffer.io.sbempty

  for (i <- 0 until StaCnt) {
    when (state === s_atomics(i)) {
      io.mem_to_ooo.staIqFeedback(i).feedbackSlow := atomicsUnit.io.feedbackSlow
      assert(!storeUnits(i).io.feedback_slow.valid)
    }
  }
  for (i <- 0 until HyuCnt) {
    when (state === s_atomics(StaCnt + i)) {
      io.mem_to_ooo.hyuIqFeedback(i).feedbackSlow := atomicsUnit.io.feedbackSlow
    }
  }

  lsq.io.exceptionAddr.isStore := io.ooo_to_mem.isStoreException
  // Exception address is used several cycles after flush.
  // We delay it by 10 cycles to ensure its flush safety.
  val atomicsException = RegInit(false.B)
  when (DelayN(redirect.valid, 10) && atomicsException) {
    atomicsException := false.B
  }.elsewhen (atomicsUnit.io.exceptionInfo.valid) {
    atomicsException := true.B
  }

  val misalignBufExceptionOverwrite = loadMisalignBuffer.io.overwriteExpBuf.valid || storeMisalignBuffer.io.overwriteExpBuf.valid
  val misalignBufExceptionVaddr = Mux(loadMisalignBuffer.io.overwriteExpBuf.valid,
    loadMisalignBuffer.io.overwriteExpBuf.vaddr,
    storeMisalignBuffer.io.overwriteExpBuf.vaddr
  )
  val misalignBufExceptionIsHyper = Mux(loadMisalignBuffer.io.overwriteExpBuf.valid,
    loadMisalignBuffer.io.overwriteExpBuf.isHyper,
    storeMisalignBuffer.io.overwriteExpBuf.isHyper
  )
  val misalignBufExceptionGpaddr = Mux(loadMisalignBuffer.io.overwriteExpBuf.valid,
    loadMisalignBuffer.io.overwriteExpBuf.gpaddr,
    storeMisalignBuffer.io.overwriteExpBuf.gpaddr
  )
  val misalignBufExceptionIsForVSnonLeafPTE = Mux(loadMisalignBuffer.io.overwriteExpBuf.valid,
    loadMisalignBuffer.io.overwriteExpBuf.isForVSnonLeafPTE,
    storeMisalignBuffer.io.overwriteExpBuf.isForVSnonLeafPTE
  )

  val vSegmentException = RegInit(false.B)
  when (DelayN(redirect.valid, 10) && vSegmentException) {
    vSegmentException := false.B
  }.elsewhen (vSegmentUnit.io.exceptionInfo.valid) {
    vSegmentException := true.B
  }
  // val atomicsExceptionAddress = RegEnable(atomicsUnit.io.exceptionInfo.bits.vaddr, atomicsUnit.io.exceptionInfo.valid)
  // val vSegmentExceptionVstart = RegEnable(vSegmentUnit.io.exceptionInfo.bits.vstart, vSegmentUnit.io.exceptionInfo.valid)
  // val vSegmentExceptionVl     = RegEnable(vSegmentUnit.io.exceptionInfo.bits.vl, vSegmentUnit.io.exceptionInfo.valid)
  // val vSegmentExceptionAddress = RegEnable(vSegmentUnit.io.exceptionInfo.bits.vaddr, vSegmentUnit.io.exceptionInfo.valid)
  // val atomicsExceptionGPAddress = RegEnable(atomicsUnit.io.exceptionInfo.bits.gpaddr, atomicsUnit.io.exceptionInfo.valid)
  // val vSegmentExceptionGPAddress = RegEnable(vSegmentUnit.io.exceptionInfo.bits.gpaddr, vSegmentUnit.io.exceptionInfo.valid)
  // val atomicsExceptionIsForVSnonLeafPTE = RegEnable(atomicsUnit.io.exceptionInfo.bits.isForVSnonLeafPTE, atomicsUnit.io.exceptionInfo.valid)
  // val vSegmentExceptionIsForVSnonLeafPTE = RegEnable(vSegmentUnit.io.exceptionInfo.bits.isForVSnonLeafPTE, vSegmentUnit.io.exceptionInfo.valid)

  val atomicsExceptionAddress = atomicsUnit.io.exceptionInfo.bits.vaddr
  val vSegmentExceptionVstart = vSegmentUnit.io.exceptionInfo.bits.vstart
  val vSegmentExceptionVl     = vSegmentUnit.io.exceptionInfo.bits.vl
  val vSegmentExceptionAddress = vSegmentUnit.io.exceptionInfo.bits.vaddr
  val atomicsExceptionGPAddress = atomicsUnit.io.exceptionInfo.bits.gpaddr
  val vSegmentExceptionGPAddress = vSegmentUnit.io.exceptionInfo.bits.gpaddr
  val atomicsExceptionIsForVSnonLeafPTE = atomicsUnit.io.exceptionInfo.bits.isForVSnonLeafPTE
  val vSegmentExceptionIsForVSnonLeafPTE = vSegmentUnit.io.exceptionInfo.bits.isForVSnonLeafPTE

  val exceptionVaddr = Mux(
    atomicsException,
    atomicsExceptionAddress,
    Mux(misalignBufExceptionOverwrite,
      misalignBufExceptionVaddr,
      Mux(vSegmentException,
        vSegmentExceptionAddress,
        lsq.io.exceptionAddr.vaddr
      )
    )
  )
  // whether vaddr need ext or is hyper inst:
  // VaNeedExt: atomicsException -> false; misalignBufExceptionOverwrite -> true; vSegmentException -> false
  // IsHyper: atomicsException -> false; vSegmentException -> false
  val exceptionVaNeedExt = !atomicsException &&
    (misalignBufExceptionOverwrite ||
      (!vSegmentException && lsq.io.exceptionAddr.vaNeedExt))
  val exceptionIsHyper = !atomicsException &&
    (misalignBufExceptionOverwrite && misalignBufExceptionIsHyper ||
      (!vSegmentException && lsq.io.exceptionAddr.isHyper && !misalignBufExceptionOverwrite))

  def GenExceptionVa(mode: UInt, isVirt: Bool, vaNeedExt: Bool,
                     satp: TlbSatpBundle, vsatp: TlbSatpBundle, hgatp: TlbHgatpBundle,
                     vaddr: UInt) = {
    require(VAddrBits >= 50)

    val satpNone = satp.mode === 0.U
    val satpSv39 = satp.mode === 8.U
    val satpSv48 = satp.mode === 9.U
 
    val vsatpNone = vsatp.mode === 0.U
    val vsatpSv39 = vsatp.mode === 8.U
    val vsatpSv48 = vsatp.mode === 9.U
 
    val hgatpNone = hgatp.mode === 0.U
    val hgatpSv39x4 = hgatp.mode === 8.U
    val hgatpSv48x4 = hgatp.mode === 9.U
 
    // For !isVirt, mode check is necessary, as we don't want virtual memory in M-mode.
    // For isVirt, mode check is unnecessary, as virt won't be 1 in M-mode.
    // Also, isVirt includes Hyper Insts, which don't care mode either.
 
    val useBareAddr = 
      (isVirt && vsatpNone && hgatpNone) ||
      (!isVirt && (mode === CSRConst.ModeM)) || 
      (!isVirt && (mode =/= CSRConst.ModeM) && satpNone)
    val useSv39Addr =
      (isVirt && vsatpSv39) ||
      (!isVirt && (mode =/= CSRConst.ModeM) && satpSv39)
    val useSv48Addr =
      (isVirt && vsatpSv48) ||
      (!isVirt && (mode =/= CSRConst.ModeM) && satpSv48)
    val useSv39x4Addr = isVirt && vsatpNone && hgatpSv39x4
    val useSv48x4Addr = isVirt && vsatpNone && hgatpSv48x4

    val bareAddr   = ZeroExt(vaddr(PAddrBits - 1, 0), XLEN)
    val sv39Addr   = SignExt(vaddr.take(39), XLEN)
    val sv39x4Addr = ZeroExt(vaddr.take(39 + 2), XLEN)
    val sv48Addr   = SignExt(vaddr.take(48), XLEN)
    val sv48x4Addr = ZeroExt(vaddr.take(48 + 2), XLEN)

    val ExceptionVa = Wire(UInt(XLEN.W))
    when (vaNeedExt) {
      ExceptionVa := Mux1H(Seq(
          (useBareAddr)   -> bareAddr,
          (useSv39Addr)   -> sv39Addr,
          (useSv48Addr)   -> sv48Addr,
          (useSv39x4Addr) -> sv39x4Addr,
          (useSv48x4Addr) -> sv48x4Addr,
      ))
    } .otherwise {
      ExceptionVa := vaddr
    }

    ExceptionVa
  }

  io.mem_to_ooo.lsqio.vaddr := GenExceptionVa(tlbcsr.priv.dmode, tlbcsr.priv.virt || exceptionIsHyper, exceptionVaNeedExt,
    tlbcsr.satp, tlbcsr.vsatp, tlbcsr.hgatp, exceptionVaddr)
  

  // vsegment instruction is executed atomic, which mean atomicsException and vSegmentException should not raise at the same time.
  XSError(atomicsException && vSegmentException, "atomicsException and vSegmentException raise at the same time!")
  io.mem_to_ooo.lsqio.vstart := Mux(vSegmentException,
                                            vSegmentExceptionVstart,
                                            lsq.io.exceptionAddr.vstart)
  io.mem_to_ooo.lsqio.vl     := Mux(vSegmentException,
                                            vSegmentExceptionVl,
                                            lsq.io.exceptionAddr.vl)

  XSError(atomicsException && atomicsUnit.io.in.valid, "new instruction before exception triggers\n")
  io.mem_to_ooo.lsqio.gpaddr := Mux(
    atomicsException,
    atomicsExceptionGPAddress,
    Mux(misalignBufExceptionOverwrite,
      misalignBufExceptionGpaddr,
      Mux(vSegmentException,
        vSegmentExceptionGPAddress,
        lsq.io.exceptionAddr.gpaddr
      )
    )
  )
  io.mem_to_ooo.lsqio.isForVSnonLeafPTE := Mux(
    atomicsException,
    atomicsExceptionIsForVSnonLeafPTE,
    Mux(misalignBufExceptionOverwrite,
      misalignBufExceptionIsForVSnonLeafPTE,
      Mux(vSegmentException,
        vSegmentExceptionIsForVSnonLeafPTE,
        lsq.io.exceptionAddr.isForVSnonLeafPTE
      )
    )
  )
  io.mem_to_ooo.topToBackendBypass match { case x =>
    x.hartId            := io.hartId
    x.externalInterrupt.msip  := outer.clint_int_sink.in.head._1(0)
    x.externalInterrupt.mtip  := outer.clint_int_sink.in.head._1(1)
    x.externalInterrupt.meip  := outer.plic_int_sink.in.head._1(0)
    x.externalInterrupt.seip  := outer.plic_int_sink.in.last._1(0)
    x.externalInterrupt.debug := outer.debug_int_sink.in.head._1(0)
    x.externalInterrupt.nmi.nmi_31 := outer.nmi_int_sink.in.head._1(0)
    x.externalInterrupt.nmi.nmi_43 := outer.nmi_int_sink.in.head._1(1)
    x.msiInfo           := DelayNWithValid(io.fromTopToBackend.msiInfo, 1)
    x.clintTime         := DelayNWithValid(io.fromTopToBackend.clintTime, 1)
  }

  io.memInfo.sqFull := RegNext(lsq.io.sqFull)
  io.memInfo.lqFull := RegNext(lsq.io.lqFull)
  io.memInfo.dcacheMSHRFull := RegNext(dcache.io.mshrFull)

  io.inner_hartId := io.hartId
  io.inner_reset_vector := RegNext(io.outer_reset_vector)
  io.outer_cpu_halt := io.ooo_to_mem.backendToTopBypass.cpuHalted
  io.outer_beu_errors_icache := RegNext(io.inner_beu_errors_icache)
  io.outer_l2_pf_enable := io.inner_l2_pf_enable
  io.inner_hc_perfEvents <> io.outer_hc_perfEvents

  // vector segmentUnit
  vSegmentUnit.io.in.bits <> io.ooo_to_mem.issueVldu.head.bits
  vSegmentUnit.io.in.valid := isSegment && io.ooo_to_mem.issueVldu.head.valid// is segment instruction
  vSegmentUnit.io.dtlb.resp.bits <> dtlb_reqs.take(LduCnt).head.resp.bits
  vSegmentUnit.io.dtlb.resp.valid <> dtlb_reqs.take(LduCnt).head.resp.valid
  vSegmentUnit.io.pmpResp <> pmpCheckResp.head
  vSegmentUnit.io.flush_sbuffer.empty := stIsEmpty
  vSegmentUnit.io.redirect <> redirect
  vSegmentUnit.io.rdcache.resp.bits := dcache.io.lsu.load(0).resp.bits
  vSegmentUnit.io.rdcache.resp.valid := dcache.io.lsu.load(0).resp.valid
  vSegmentUnit.io.rdcache.s2_bank_conflict := dcache.io.lsu.load(0).s2_bank_conflict
  // -------------------------
  // Vector Segment Triggers
  // -------------------------
  vSegmentUnit.io.fromCsrTrigger.tdataVec := tdata
  vSegmentUnit.io.fromCsrTrigger.tEnableVec := tEnable
  vSegmentUnit.io.fromCsrTrigger.triggerCanRaiseBpExp := triggerCanRaiseBpExp
  vSegmentUnit.io.fromCsrTrigger.debugMode := debugMode

  val clk_tmp = RegInit(false.B)
  clk_tmp := !clk_tmp
  dontTouch(clk_tmp)


  // reset tree of MemBlock
  if (p(DebugOptionsKey).ResetGen) {
    val leftResetTree = ResetGenNode(
      Seq(
        ModuleNode(ptw),
        ModuleNode(ptw_to_l2_buffer),
        ModuleNode(lsq),
        ModuleNode(dtlb_all),
//        ModuleNode(dtlb_st_tlb_st),
//        ModuleNode(dtlb_prefetch_tlb_prefetch),
        ModuleNode(pmpWrapper)
      )
      ++ (if (prefetcherOpt.isDefined) Seq(ModuleNode(prefetcherOpt.get)) else Nil)
      ++ (if (l1PrefetcherOpt.isDefined) Seq(ModuleNode(l1PrefetcherOpt.get)) else Nil)
    )
    val rightResetTree = ResetGenNode(
      Seq(
        ModuleNode(sbuffer),
        ModuleNode(dtlb_all),
        ModuleNode(dcache),
        ModuleNode(l1d_to_l2_buffer),
        CellNode(io.reset_backend)
      )
    )
    ResetGen(leftResetTree, reset, io.dftBypass.fromL2Top.reset, !p(DebugOptionsKey).ResetGen)
    ResetGen(rightResetTree, reset, io.dftBypass.fromL2Top.reset, !p(DebugOptionsKey).ResetGen)
  } else {
    io.reset_backend := DontCare
  }
  io.resetInFrontendBypass.toL2Top := io.resetInFrontendBypass.fromFrontend

  // top-down info
  dcache.io.debugTopDown.robHeadVaddr := io.debugTopDown.robHeadVaddr
  dtlbRepeater.io.debugTopDown.robHeadVaddr := io.debugTopDown.robHeadVaddr
  lsq.io.debugTopDown.robHeadVaddr := io.debugTopDown.robHeadVaddr
  io.debugTopDown.toCore.robHeadMissInDCache := dcache.io.debugTopDown.robHeadMissInDCache
  io.debugTopDown.toCore.robHeadTlbReplay := lsq.io.debugTopDown.robHeadTlbReplay
  io.debugTopDown.toCore.robHeadTlbMiss := lsq.io.debugTopDown.robHeadTlbMiss
  io.debugTopDown.toCore.robHeadLoadVio := lsq.io.debugTopDown.robHeadLoadVio
  io.debugTopDown.toCore.robHeadLoadMSHR := lsq.io.debugTopDown.robHeadLoadMSHR
  dcache.io.debugTopDown.robHeadOtherReplay := lsq.io.debugTopDown.robHeadOtherReplay
  dcache.io.debugRolling := io.debugRolling

  val hyLdDeqCount = PopCount(io.ooo_to_mem.issueHya.map(x => x.valid && FuType.isLoad(x.bits.uop.fuType)))
  val hyStDeqCount = PopCount(io.ooo_to_mem.issueHya.map(x => x.valid && FuType.isStore(x.bits.uop.fuType)))
  val ldDeqCount = PopCount(io.ooo_to_mem.issueLda.map(_.valid)) +& hyLdDeqCount
  val stDeqCount = PopCount(io.ooo_to_mem.issueSta.take(StaCnt).map(_.valid)) +& hyStDeqCount
  val iqDeqCount = ldDeqCount +& stDeqCount
  XSPerfAccumulate("load_iq_deq_count", ldDeqCount)
  XSPerfHistogram("load_iq_deq_count", ldDeqCount, true.B, 0, LdExuCnt + 1)
  XSPerfAccumulate("store_iq_deq_count", stDeqCount)
  XSPerfHistogram("store_iq_deq_count", stDeqCount, true.B, 0, StAddrCnt + 1)
  XSPerfAccumulate("ls_iq_deq_count", iqDeqCount)

  val pfevent = Module(new PFEvent)
  pfevent.io.distribute_csr := csrCtrl.distribute_csr
  val csrevents = pfevent.io.hpmevent.slice(16,24)

  val memBlockPerfEvents = Seq(
    ("ldDeqCount", ldDeqCount),
    ("stDeqCount", stDeqCount),
  )

  val perfFromUnits = (loadUnits ++ Seq(sbuffer, lsq, dcache)).flatMap(_.getPerfEvents)
  val perfFromPTW    = perfEventsPTW.map(x => ("perfEventsPTW", x.value))
  val perfBlock     = Seq(("ldDeqCount", ldDeqCount),
                          ("stDeqCount", stDeqCount))
  // let index = 0 be no event
  val allPerfEvents = Seq(("noEvent", 0.U)) ++ perfFromUnits ++ perfFromPTW ++ perfBlock

  if (printEventCoding) {
    for (((name, inc), i) <- allPerfEvents.zipWithIndex) {
      println("MemBlock perfEvents Set", name, inc, i)
    }
  }

  val allPerfInc = allPerfEvents.map(_._2.asTypeOf(new PerfEvent))
  val perfEvents = HPerfMonitor(csrevents, allPerfInc).getPerfEvents
  generatePerfEvent()
  private val cg = ClockGate.getTop
  dontTouch(cg)
  if (hasMbist) {
    io.dftBypass.toFrontend.func.get := io.dftBypass.fromL2Top.func.get
    io.dftBypass.toFrontend.reset.get := io.dftBypass.fromL2Top.reset.get
    io.dftBypass.toBackend.func.get := io.dftBypass.fromL2Top.func.get
    io.dftBypass.toBackend.reset.get := io.dftBypass.fromL2Top.reset.get
    cg.te := io.dftBypass.fromL2Top.func.get.cgen
  } else {
    cg.te := false.B
  }
}

class MemBlock()(implicit p: Parameters) extends LazyModule
  with HasXSParameter {
  override def shouldBeInlined: Boolean = false

  val inner = LazyModule(new MemBlockInlined())

  lazy val module = new MemBlockImp(this)
}

class MemBlockImp(wrapper: MemBlock) extends LazyModuleImp(wrapper) with HasXSParameter {
  val io = IO(wrapper.inner.module.io.cloneType)
  val io_perf = IO(wrapper.inner.module.io_perf.cloneType)
  io <> wrapper.inner.module.io
  io_perf <> wrapper.inner.module.io_perf


  if (p(DebugOptionsKey).ResetGen) {
    ResetGen(ResetGenNode(Seq(ModuleNode(wrapper.inner.module))), reset, io.dftBypass.fromL2Top.reset, !p(DebugOptionsKey).ResetGen)
  }
}