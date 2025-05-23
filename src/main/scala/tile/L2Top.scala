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

package tile

import chisel3._
import chisel3.util._
import coupledL2.tl2chi.{CHIIssue, PortIO, TL2CHICoupledL2}
import coupledL2.tl2tl.TL2TLCoupledL2
import xs.utils.cache.common.PrefetchCtrlFromCore
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams, BusErrors, MaxHartIdBits}
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config._
import system.HasSoCParameter
import top.BusPerfMonitor
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan._
import xs.utils._
import xs.utils.perf.{DebugOptionsKey, LogUtilsOptionsKey, PerfCounterOptionsKey, PerfEvent}
import xs.utils.tl._
import xs.utils.cache.common._
import xs.utils.debug.{HardwareAssertion, HardwareAssertionKey, HwaParams}
import freechips.rocketchip.util.DontTouch
import xs.utils.sram._
import xs.utils.cache.EnableCHI

class L1BusErrorUnitInfo(implicit val p: Parameters) extends Bundle {
  val ecc_error = Valid(UInt(48.W)) //Valid(UInt(soc.PAddrBits.W))
}

class XSL1BusErrors()(implicit val p: Parameters) extends BusErrors {
  val icache = new L1BusErrorUnitInfo
  val dcache = new L1BusErrorUnitInfo
  val l2 = new L1BusErrorUnitInfo

  override def toErrorList: List[Option[(ValidIO[UInt], String, String)]] =
    List(
      Some(icache.ecc_error, "I_ECC", "Icache ecc error"),
      Some(dcache.ecc_error, "D_ECC", "Dcache ecc error"),
      Some(l2.ecc_error, "L2_ECC", "L2Cache ecc error")
    )
}

/**
  *   L2Top contains everything between Core and XSTile-IO
  */
class L2TopInlined()(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasSoCParameter
{
  override def shouldBeInlined: Boolean = true

  def chainBuffer(depth: Int, n: String): (Seq[LazyModule], TLNode) = {
    val buffers = Seq.fill(depth){ LazyModule(new TLBuffer()) }
    buffers.zipWithIndex.foreach{ case (b, i) => {
      b.suggestName(s"${n}_${i}")
    }}
    val node = buffers.map(_.node.asInstanceOf[TLNode]).reduce(_ :*=* _)
    (buffers, node)
  }
  val enableL2 = true
  // =========== Components ============
  val l1_xbar = TLXbar()
  val mmio_xbar = TLXbar()
  val mmio_port = TLIdentityNode() // to L3
  val memory_port = if (enableCHI && enableL2) None else Some(TLIdentityNode())
  val beu = LazyModule(new BusErrorUnit(
    new XSL1BusErrors(),
    BusErrorUnitParams(soc.BEURange.base, soc.BEURange.mask.toInt + 1),
    beatBytes = 8
  ))

  val i_mmio_port = TLTempNode()
  val d_mmio_port = TLTempNode()

  val misc_l2_pmu = BusPerfMonitor(name = "Misc_L2", enable = !debugOpts.FPGAPlatform) // l1D & l1I & PTW
  val l2_l3_pmu = BusPerfMonitor(name = "L2_L3", enable = !debugOpts.FPGAPlatform && !enableCHI, stat_latency = true)
  val xbar_l2_buffer = TLBuffer()

  val enbale_tllog = !debugOpts.FPGAPlatform && debugOpts.AlwaysBasicDB
  val l1d_logger = TLLogger(s"L2_L1D_${coreParams.HartId}", enbale_tllog)
  val l1i_logger = TLLogger(s"L2_L1I_${coreParams.HartId}", enbale_tllog)
  val ptw_logger = TLLogger(s"L2_PTW_${coreParams.HartId}", enbale_tllog)
  val ptw_to_l2_buffer = LazyModule(new TLBuffer)
  val i_mmio_buffer = LazyModule(new TLBuffer)

  val clint_int_node = IntIdentityNode()
  val debug_int_node = IntIdentityNode()
  val plic_int_node = IntIdentityNode()
  val nmi_int_node = IntIdentityNode()

  println(s"L2top enableCHI: ${enableCHI}")
  val l2cache = if (enableL2) {
    val config = new Config((_, _, _) => {
      case L2ParamKey => p(L2ParamKey).copy(
        hartId = p(XSCoreParamsKey).HartId,
        FPGAPlatform = debugOpts.FPGAPlatform,
        hasMbist = hasMbist
      )
      case EnableCHI => p(EnableCHI)
      case CHIIssue => p(CHIIssue)
      case BankBitsKey => log2Ceil(coreParams.L2NBanks)
      case MaxHartIdBits => p(MaxHartIdBits)
      case LogUtilsOptionsKey => p(LogUtilsOptionsKey)
      case PerfCounterOptionsKey => p(PerfCounterOptionsKey)
      case HardwareAssertionKey => HwaParams(enable = false)
    })
    if (enableCHI) Some(LazyModule(new TL2CHICoupledL2()(new Config(config))))
    else Some(LazyModule(new TL2TLCoupledL2()(new Config(config))))
  } else None
  val l2_binder = BankBinder(coreParams.L2NBanks, 64)

  // =========== Connection ============
  // l2 to l2_binder, then to memory_port
  l2cache match {
    case Some(l2) =>
      l2_binder :*= l2.node :*= xbar_l2_buffer :*= l1_xbar :=* misc_l2_pmu
      l2 match {
        case l2: TL2TLCoupledL2 =>
          memory_port.get := l2_l3_pmu := TLClientsMerger() := TLXbar() :=* l2_binder
        case l2: TL2CHICoupledL2 =>
          l2.managerNode := TLXbar() :=* l2_binder
          l2.mmioNode := mmio_port
      }
    case None =>
      memory_port.get := l1_xbar
  }
  
  mmio_xbar := TLBuffer.chainNode(2) := i_mmio_port
  mmio_xbar := TLBuffer.chainNode(2) := d_mmio_port
  beu.node := TLBuffer.chainNode(1) := mmio_xbar
  mmio_port := TLBuffer() := mmio_xbar

  class Imp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val io = IO(new Bundle {
      val beu_errors = Input(chiselTypeOf(beu.module.io.errors))
      val reset_vector = new Bundle {
        val fromTile = Input(UInt(PAddrBits.W))
        val toCore = Output(UInt(PAddrBits.W))
      }
      val hartId = new Bundle() {
        val fromTile = Input(UInt(64.W))
        val toCore = Output(UInt(64.W))
      }
      val cpu_halt = new Bundle() {
        val fromCore = Input(Bool())
        val toTile = Output(Bool())
      }
      val hartIsInReset = new Bundle() {
        val resetInFrontend = Input(Bool())
        val toTile = Output(Bool())
      }
      val debugTopDown = new Bundle() {
        val robTrueCommit = Input(UInt(64.W))
        val robHeadPaddr = Flipped(Valid(UInt(36.W)))
        val l2MissMatch = Output(Bool())
      }
      val chi = if (enableCHI) Some(new PortIO) else None
      val nodeID = if (enableCHI) Some(Input(UInt(NodeIDWidth.W))) else None
      val l2_tlb_req = new TlbRequestIO(nRespDups = 2)
      val l2_pmp_resp = Flipped(new PMPRespBundle)
      val l2_hint = ValidIO(new L2ToL1Hint())
      val pfCtrlFromCore = Input(new PrefetchCtrlFromCore)
      val perfEvents = Output(Vec(numPCntHc * coreParams.L2NBanks + 1, new PerfEvent))
      val dftIn = new Bundle() {
        val func      = Option.when(hasMbist)(Input(new SramBroadcastBundle))
        val reset = Option.when(hasMbist)(Input(new DFTResetSignals()))
      }
      val dftOut = new Bundle() {
        val func      = Option.when(hasMbist)(Output(new SramBroadcastBundle))
        val reset = Option.when(hasMbist)(Output(new DFTResetSignals()))
      }
      // val reset_core = IO(Output(Reset()))
    })

    io.dftOut.func.zip(io.dftIn.func).foreach({case(a, b) => a := b})
    io.dftOut.reset.zip(io.dftIn.reset).foreach({case(a, b) => a := b})

    val resetDelayN = Module(new DelayN(UInt(PAddrBits.W), 5))

    beu.module.io.errors <> io.beu_errors
    resetDelayN.io.in := io.reset_vector.fromTile
    io.reset_vector.toCore := resetDelayN.io.out
    io.hartId.toCore := io.hartId.fromTile
    io.cpu_halt.toTile := io.cpu_halt.fromCore
    // dft_reset_out := dft_reset
    dontTouch(io.hartId)
    dontTouch(io.cpu_halt)
    if (!io.chi.isEmpty) { dontTouch(io.chi.get) }

    val hartIsInReset = RegInit(true.B)
    hartIsInReset := io.hartIsInReset.resetInFrontend || reset.asBool
    io.hartIsInReset.toTile := hartIsInReset

    if (l2cache.isDefined) {
      val l2 = l2cache.get.module
      l2.io.dft := io.dftIn
      l2.io.ramctl := DontCare
      dontTouch(l2.io)
      l2.io.pfCtrlFromCore := io.pfCtrlFromCore
      io.l2_hint := l2.io.l2_hint
      l2.io.debugTopDown.robHeadPaddr := DontCare
      l2.io.hartId := io.hartId.fromTile
      l2.io.debugTopDown.robHeadPaddr := io.debugTopDown.robHeadPaddr
      l2.io.debugTopDown.robTrueCommit := io.debugTopDown.robTrueCommit
      io.debugTopDown.l2MissMatch := l2.io.debugTopDown.l2MissMatch

      // l2.dft_reset := dft_reset
      // dft_reset_out := l2.dft_reset_out

      // if(hasMbist) {
      //   l2.dft.get := dft.get
      //   dft_out.get := l2.dft_out.get
      // }

      /* l2 tlb */
      io.l2_tlb_req.req.bits := DontCare
      io.l2_tlb_req.req.valid := l2.io.l2_tlb_req.req.valid
      io.l2_tlb_req.resp.ready := l2.io.l2_tlb_req.resp.ready
      io.l2_tlb_req.req.bits.vaddr := l2.io.l2_tlb_req.req.bits.vaddr
      io.l2_tlb_req.req.bits.cmd := l2.io.l2_tlb_req.req.bits.cmd
      io.l2_tlb_req.req.bits.size := l2.io.l2_tlb_req.req.bits.size
      io.l2_tlb_req.req.bits.kill := l2.io.l2_tlb_req.req.bits.kill
      io.l2_tlb_req.req.bits.no_translate := l2.io.l2_tlb_req.req.bits.no_translate
      io.l2_tlb_req.req_kill := l2.io.l2_tlb_req.req_kill
      io.perfEvents := l2.io_perf

      val allPerfEvents = l2.getPerfEvents
      if (printEventCoding) {
        for (((name, inc), i) <- allPerfEvents.zipWithIndex) {
          println("L2 Cache perfEvents Set", name, inc, i)
        }
      }

      l2.io.l2_tlb_req.resp.valid := io.l2_tlb_req.resp.valid
      l2.io.l2_tlb_req.req.ready := io.l2_tlb_req.req.ready
      l2.io.l2_tlb_req.resp.bits.paddr.head := io.l2_tlb_req.resp.bits.paddr.head
      l2.io.l2_tlb_req.resp.bits.pbmt := io.l2_tlb_req.resp.bits.pbmt.head
      l2.io.l2_tlb_req.resp.bits.miss := io.l2_tlb_req.resp.bits.miss
      l2.io.l2_tlb_req.resp.bits.excp.head.gpf := io.l2_tlb_req.resp.bits.excp.head.gpf
      l2.io.l2_tlb_req.resp.bits.excp.head.pf := io.l2_tlb_req.resp.bits.excp.head.pf
      l2.io.l2_tlb_req.resp.bits.excp.head.af := io.l2_tlb_req.resp.bits.excp.head.af
      l2.io.l2_tlb_req.pmp_resp.ld := io.l2_pmp_resp.ld
      l2.io.l2_tlb_req.pmp_resp.st := io.l2_pmp_resp.st
      l2.io.l2_tlb_req.pmp_resp.instr := io.l2_pmp_resp.instr
      l2.io.l2_tlb_req.pmp_resp.mmio := io.l2_pmp_resp.mmio
      l2.io.l2_tlb_req.pmp_resp.atomic := io.l2_pmp_resp.atomic
      l2cache.get match {
        case l2cache: TL2CHICoupledL2 =>
          val l2 = l2cache.module
          l2.io_nodeID := io.nodeID.get
          io.chi.get <> l2.io_chi
        case l2cache: TL2TLCoupledL2 =>
      }
    } else {
      io.l2_hint := 0.U.asTypeOf(io.l2_hint)
      io.debugTopDown <> DontCare

      io.l2_tlb_req.req.valid := false.B
      io.l2_tlb_req.req.bits := DontCare
      io.l2_tlb_req.req_kill := DontCare
      io.l2_tlb_req.resp.ready := true.B
      io.perfEvents := DontCare
    }
  }

  lazy val module = new Imp(this)
}

class L2Top()(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasSoCParameter {

  override def shouldBeInlined: Boolean = false

  val inner = LazyModule(new L2TopInlined())

  class Imp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    // override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
    val io = IO(inner.module.io.cloneType)
    val reset_core = IO(Output(Reset()))
    io <> inner.module.io
    dontTouch(inner.module.io)
    // val dft_reset = IO(Input(new DFTResetSignals()))
    // val dft_reset_out = IO(Output(new DFTResetSignals()))
    // inner.module.dft_reset := dft_reset
    // dft_reset_out := inner.module.dft_reset_out

    // val dft = if(hasMbist) Some(IO(Input(new SramBroadcastBundle))) else None
    // val dft_out = if(hasMbist) Some(IO(Output(new SramBroadcastBundle))) else None
    // if(hasMbist) {
    //   inner.module.dft.get := dft.get
    //   dft_out.get := inner.module.dft_out.get
    // }

    if (debugOpts.ResetGen) {
      ResetGen(ResetGenNode(Seq(
        CellNode(reset_core),
        ModuleNode(inner.module)
      )), reset, None, !p(DebugOptionsKey).ResetGen)
    } else {
      reset_core := DontCare
    }
  }

  lazy val module = new Imp(this)
}
