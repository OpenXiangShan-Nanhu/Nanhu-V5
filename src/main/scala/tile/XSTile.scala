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
import chisel3.util.{Valid, ValidIO}
import coupledL2.tl2chi.PortIO
import device.MsiInfoBundle
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
import system.HasSoCParameter
import xiangshan.backend.trace.TraceCoreInterface
import xiangshan.{HasXSParameter, XSCore}
import xs.utils._
import xs.utils.sram._
class XSTile()(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasSoCParameter
{
  override def shouldBeInlined: Boolean = false
  val core = LazyModule(new XSCore())
  val l2top = LazyModule(new L2Top())

  val enableL2 = true // p(L2ParamKey).isDefined
  // =========== Public Ports ============
  val memBlock = core.memBlock.inner
  val core_l3_pf_port = memBlock.l3_pf_sender_opt
  val memory_port = if (enableCHI && enableL2) None else Some(l2top.inner.memory_port.get)
  val tl_uncache = l2top.inner.mmio_port
  // val axi4_uncache = if (enableCHI) Some(AXI4UserYanker()) else None
  val beu_int_source = l2top.inner.beu.intNode
  val core_reset_sink = BundleBridgeSink(Some(() => Reset()))
  val clint_int_node = l2top.inner.clint_int_node
  val plic_int_node = l2top.inner.plic_int_node
  val debug_int_node = l2top.inner.debug_int_node
  val nmi_int_node = l2top.inner.nmi_int_node
  memBlock.clint_int_sink := clint_int_node
  memBlock.plic_int_sink :*= plic_int_node
  memBlock.debug_int_sink := debug_int_node
  memBlock.nmi_int_sink := nmi_int_node

  // =========== Components' Connection ============
  // L1 to l1_xbar
  coreParams.dcacheParametersOpt.map { _ =>
    l2top.inner.misc_l2_pmu := l2top.inner.l1d_logger := memBlock.dcache_port :=
      memBlock.l1d_to_l2_buffer.node := memBlock.dcache.clientNode
  }

  l2top.inner.misc_l2_pmu := l2top.inner.l1i_logger := memBlock.frontendBridge.icache_node
  if (!coreParams.softPTW) {
    l2top.inner.misc_l2_pmu := l2top.inner.ptw_logger := l2top.inner.ptw_to_l2_buffer.node := memBlock.ptw_to_l2_buffer.node
  }

  // L2 Prefetch
  l2top.inner.l2cache match {
    case Some(l2) =>
      l2.pf_recv_node.foreach(recv => {
        println("Connecting L1 prefetcher to L2!")
        recv := memBlock.l2_pf_sender_opt.get
      })
    case None =>
  }

  val core_l3_tpmeta_source_port = l2top.inner.l2cache match {
    case Some(l2) => l2.tpmeta_source_node
    case None => None
  }
  val core_l3_tpmeta_sink_port = l2top.inner.l2cache match {
    case Some(l2) => l2.tpmeta_sink_node
    case None => None
  }

  // mmio
  l2top.inner.i_mmio_port := l2top.inner.i_mmio_buffer.node := memBlock.frontendBridge.instr_uncache_node
  l2top.inner.d_mmio_port := memBlock.uncache.clientNode

  // =========== IO Connection ============
  class XSTileImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val io = IO(new Bundle {
      val hartId = Input(UInt(hartIdLen.W))
      val msiInfo = Input(ValidIO(new MsiInfoBundle))
      val reset_vector = Input(UInt(PAddrBits.W))
      val cpu_halt = Output(Bool())
      val hartIsInReset = Output(Bool())
      val traceCoreInterface = new TraceCoreInterface
      val debugTopDown = new Bundle {
        val robHeadPaddr = Valid(UInt(PAddrBits.W))
        val l3MissMatch = Input(Bool())
      }
      val chi = if (enableCHI) Some(new PortIO) else None
      val nodeID = if (enableCHI) Some(Input(UInt(NodeIDWidth.W))) else None
      val clintTime = Input(ValidIO(UInt(64.W)))
      val dft = new Bundle() {
        val func  = Option.when(hasMbist)(Input(new SramBroadcastBundle))
        val reset = Option.when(hasMbist)(Input(new DFTResetSignals()))
      }
      val sram_ctrl = Input(new SramCtrlBundle)
    })

    dontTouch(io.hartId)
    dontTouch(io.msiInfo)
    dontTouch(io.reset_vector)
    if (!io.chi.isEmpty) { dontTouch(io.chi.get) }

    val core_soft_rst = core_reset_sink.in.head._1 // unused

    // use for linknan
    core.module.io.power.wfiCtrRst := false.B
    core.module.io.power.flushSb := false.B
    core.module.io.power.fencei := false.B

    l2top.module.io.hartId.fromTile := io.hartId
    core.module.io.hartId := l2top.module.io.hartId.toCore
    core.module.io.reset_vector := l2top.module.io.reset_vector.toCore
    core.module.io.msiInfo := io.msiInfo
    core.module.io.clintTime := io.clintTime
    l2top.module.io.reset_vector.fromTile := io.reset_vector
    l2top.module.io.cpu_halt.fromCore := core.module.io.cpu_halt
    io.cpu_halt := l2top.module.io.cpu_halt.toTile
    // l2top.module.dft_reset := dft_reset
    // core.module.dft_reset := l2top.module.dft_reset_out
    //core.module.dft_reset := dft_reset

    l2top.module.io.hartIsInReset.resetInFrontend := core.module.io.resetInFrontend
    io.hartIsInReset := l2top.module.io.hartIsInReset.toTile
    io.traceCoreInterface <> core.module.io.traceCoreInterface

    l2top.module.io.beu_errors.icache <> core.module.io.beu_errors.icache
    l2top.module.io.beu_errors.dcache <> core.module.io.beu_errors.dcache
    //val dft = if (hasMbist) Some(IO(Input(new SramBroadcastBundle))) else None
    if (hasMbist) {
      l2top.module.io.dftIn := io.dft
      core.module.io.dft := l2top.module.io.dftOut
    }
    if (enableL2) {
      // TODO: add ECC interface of L2


      l2top.module.io.pfCtrlFromCore := core.module.io.l2PfCtrl
      l2top.module.io.beu_errors.l2 <> 0.U.asTypeOf(l2top.module.io.beu_errors.l2)
      core.module.io.l2_hint.bits.sourceId := l2top.module.io.l2_hint.bits.sourceId
      core.module.io.l2_hint.bits.isKeyword := l2top.module.io.l2_hint.bits.isKeyword
      core.module.io.l2_hint.valid := l2top.module.io.l2_hint.valid

      core.module.io.l2PfqBusy := false.B
      core.module.io.debugTopDown.l2MissMatch := l2top.module.io.debugTopDown.l2MissMatch
      l2top.module.io.debugTopDown.robHeadPaddr := core.module.io.debugTopDown.robHeadPaddr
      l2top.module.io.debugTopDown.robTrueCommit := core.module.io.debugTopDown.robTrueCommit
      l2top.module.io.l2_pmp_resp := core.module.io.l2_pmp_resp
      core.module.io.l2_tlb_req <> l2top.module.io.l2_tlb_req

      core.module.io.perfEvents <> l2top.module.io.perfEvents
    } else {

      l2top.module.io.beu_errors.l2 <> 0.U.asTypeOf(l2top.module.io.beu_errors.l2)
      core.module.io.l2_hint.bits.sourceId := l2top.module.io.l2_hint.bits.sourceId
      core.module.io.l2_hint.bits.isKeyword := l2top.module.io.l2_hint.bits.isKeyword
      core.module.io.l2_hint.valid := l2top.module.io.l2_hint.valid

      core.module.io.l2PfqBusy := false.B
      core.module.io.debugTopDown.l2MissMatch := false.B

      core.module.io.l2_tlb_req.req.valid := false.B
      core.module.io.l2_tlb_req.req.bits := DontCare
      core.module.io.l2_tlb_req.req_kill := DontCare
      core.module.io.l2_tlb_req.resp.ready := true.B

      core.module.io.perfEvents <> DontCare
    }

    io.debugTopDown.robHeadPaddr := core.module.io.debugTopDown.robHeadPaddr
    core.module.io.debugTopDown.l3MissMatch := io.debugTopDown.l3MissMatch

    io.chi.foreach(_ <> l2top.module.io.chi.get)
    l2top.module.io.nodeID.foreach(_ := io.nodeID.get)

    if (debugOpts.ResetGen && enableL2) {
      core.module.reset := l2top.module.reset_core
    }
    private val sramCtrl = SramHelper.genSramCtrlBundleTop()
    sramCtrl := io.sram_ctrl
  }

  lazy val module = new XSTileImp(this)
}
