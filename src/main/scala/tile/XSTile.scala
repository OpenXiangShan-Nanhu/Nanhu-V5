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
import freechips.rocketchip.tilelink._
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
//  val l2top = LazyModule(new L2Top())

  val enableL2 = false // p(L2ParamKey).isDefined
  // =========== Public Ports ============
  val memBlock = core.memBlock.inner
  val core_reset_sink = BundleBridgeSink(Some(() => Reset()))

  val mmio_xbar = TLXbar()
  val mmio_port = TLIdentityNode() // to L3

  val memory_xbar = TLXbar()
  val memory_port = TLIdentityNode() // to L3


  memory_xbar := TLBuffer.chainNode(2) := memBlock.frontendBridge.icache_node
  memory_xbar := TLBuffer.chainNode(2) := memBlock.dcache.clientNode
  memory_xbar := TLBuffer.chainNode(2) := memBlock.ptw_to_l2_buffer.node
  memory_port := TLBuffer() := memory_xbar
  // mmio
  mmio_xbar := TLBuffer.chainNode(2) := memBlock.frontendBridge.instr_uncache_node
  mmio_xbar := TLBuffer.chainNode(2) := memBlock.uncache.clientNode
  mmio_port := TLBuffer() := mmio_xbar

  // =========== IO Connection ============
  class XSTileImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val io = IO(new Bundle {
      val hartId = Input(UInt(hartIdLen.W))
      val msiInfo = Input(ValidIO(new MsiInfoBundle))
      val reset_vector = Input(UInt(PAddrBits.W))
      val cpu_halt = Output(Bool())
      val hartIsInReset = Output(Bool())
      val traceCoreInterface = new TraceCoreInterface
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

    val core_soft_rst = core_reset_sink.in.head._1 // unused

    // use for linknan
    core.module.io.power.wfiCtrRst := false.B
    core.module.io.power.flushSb := false.B

    core.module.io.hartId := io.hartId
    core.module.io.reset_vector := io.reset_vector
    core.module.io.msiInfo := io.msiInfo
    core.module.io.clintTime := io.clintTime
    io.cpu_halt := core.module.io.cpu_halt

    io.hartIsInReset := core.module.io.resetInFrontend
    io.traceCoreInterface <> core.module.io.traceCoreInterface

//    l2top.module.io.beu_errors.icache <> core.module.io.beu_errors.icache
//    l2top.module.io.beu_errors.dcache <> core.module.io.beu_errors.dcache
    //val dft = if (hasMbist) Some(IO(Input(new SramBroadcastBundle))) else None
    if (hasMbist) {
      core.module.io.dft := io.dft
    }


    core.module.io.l2_hint.bits.sourceId := false.B
    core.module.io.l2_hint.bits.isKeyword := false.B
    core.module.io.l2_hint.valid := false.B

    core.module.io.l2PfqBusy := false.B
    core.module.io.debugTopDown.l2MissMatch := false.B

    core.module.io.l2_tlb_req.req.valid := false.B
    core.module.io.l2_tlb_req.req.bits := DontCare
    core.module.io.l2_tlb_req.req_kill := DontCare
    core.module.io.l2_tlb_req.resp.ready := true.B

    core.module.io.perfEvents <> DontCare

    core.module.io.debugTopDown.l3MissMatch := false.B

    private val sramCtrl = SramHelper.genSramCtrlBundleTop()
    sramCtrl := io.sram_ctrl
  }

  lazy val module = new XSTileImp(this)
}
