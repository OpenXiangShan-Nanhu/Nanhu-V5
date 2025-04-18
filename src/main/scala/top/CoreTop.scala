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
****************************************************************************************/
package top

import chisel3._
import chisel3.util._
import device.MsiInfoBundle
import freechips.rocketchip.diplomacy.{AddressSet, RegionType, TransferSizes}
import freechips.rocketchip.interrupts.{IntSourceNode, IntSourcePortSimple}
import freechips.rocketchip.resources.BindingScope
import freechips.rocketchip.tilelink.{TLBuffer, TLManagerNode, TLSlaveParameters, TLSlavePortParameters, TLXbar}
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.DisableMonitors
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.trace.TraceCoreInterface
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan._
import xs.utils.cache.common.{AliasKey, PrefetchKey, VaddrKey}
import xs.utils.perf.{DebugOptionsKey, PerfEvent}
import xs.utils.sram.{SramBroadcastBundle, SramCtrlBundle}
import xs.utils.tl.ReqSourceKey
import xs.utils.{ChiselDB, Constantin, DFTResetSignals, IntBuffer}

class CoreTop(implicit p:Parameters) extends LazyModule with BindingScope {
  private val core = LazyModule(new XSCore)
  private val memBlock = core.memBlock.inner
  private val cacheXBar = LazyModule(new TLXbar)
  private val mmioXBar = LazyModule(new TLXbar)

  private val clintIntNode = IntSourceNode(IntSourcePortSimple(1, 1, 2))
  private val debugIntNode = IntSourceNode(IntSourcePortSimple(1, 1, 1))
  private val plicIntNode = IntSourceNode(IntSourcePortSimple(1, 2, 1))
  private val nmiIntNode = IntSourceNode(IntSourcePortSimple(1, 1, (new NonmaskableInterruptIO).elements.size))
  private val l2pfSink = memBlock.l2_pf_sender_opt.map(_.makeSink())
  private val l3pfSink = memBlock.l3_pf_sender_opt.map(_.makeSink())

  memBlock.clint_int_sink := IntBuffer(3, cdc = true) := clintIntNode
  memBlock.debug_int_sink := IntBuffer(3, cdc = true) := debugIntNode
  memBlock.plic_int_sink :*= IntBuffer(3, cdc = true) :*= plicIntNode
  memBlock.nmi_int_sink := IntBuffer(3, cdc = true) := nmiIntNode

  cacheXBar.node :*= TLBuffer.chainNode(1, Some(s"l1d_buffer")) :*= memBlock.dcache_port :*= memBlock.l1d_to_l2_buffer.node :*= memBlock.dcache.clientNode
  cacheXBar.node :*= TLBuffer.chainNode(1, Some(s"ptw_buffer")) :*= memBlock.ptw_to_l2_buffer.node
  cacheXBar.node :*= TLBuffer.chainNode(1, Some(s"l1i_buffer")) :*= memBlock.frontendBridge.icache_node

  mmioXBar.node :*= TLBuffer.chainNode(1, Some(s"mem_uc_buffer")) :*= memBlock.uncache.clientNode
  mmioXBar.node :*= TLBuffer.chainNode(1, Some(s"frd_uc_buffer")) :*= memBlock.frontendBridge.instr_uncache_node

  private val mmioSlvParams = TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = Seq(AddressSet(0x0L, (0x1L << 48) - 1)),
      supportsGet = TransferSizes(1, 8),
      supportsPutFull = TransferSizes(1, 8),
      supportsPutPartial = TransferSizes(1, 8),
      regionType = RegionType.GET_EFFECTS
    )),
    beatBytes = 8
  )

  private val l2NodeParameters = TLSlavePortParameters.v1(
    managers = Seq(
      TLSlaveParameters.v1(
        address = Seq(AddressSet(0x0L, (0x1L << 48) - 1)),
        regionType = RegionType.CACHED,
        supportsAcquireT = TransferSizes(64, 64),
        supportsAcquireB = TransferSizes(64, 64),
        supportsArithmetic = TransferSizes(1, 64),
        supportsGet = TransferSizes(1, 64),
        supportsLogical = TransferSizes(1, 64),
        supportsPutFull = TransferSizes(1, 64),
        supportsPutPartial = TransferSizes(1, 64),
        executable = true
      )
    ),
    beatBytes = 32,
    minLatency = 2,
    responseFields = Nil,
    requestKeys = Seq(AliasKey, VaddrKey, PrefetchKey, ReqSourceKey),
    endSinkId = 256 * (1 << 2)
  )

  private val cioNode = TLManagerNode(Seq(mmioSlvParams))
  private val l2Node = TLManagerNode(Seq(l2NodeParameters))
  cioNode :*= TLBuffer.chainNode(1, Some(s"uncache_buffer")) :*= mmioXBar.node
  l2Node :*= TLBuffer.chainNode(1, Some(s"l2_buffer")) :*= cacheXBar.node

  lazy val module = new CoreTopImpl
  class CoreTopImpl extends LazyModuleImp(this) with HasXSParameter {
    override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
    val cio = cioNode.makeIOs()
    val l2 = l2Node.makeIOs()
    val clint = clintIntNode.makeIOs()
    val debug = debugIntNode.makeIOs()
    val plic = plicIntNode.makeIOs()
    val nmi = nmiIntNode.makeIOs()
    val l2pf = l2pfSink.map(_.makeIOs())
    val l3pf = l3pfSink.map(_.makeIOs())

    val io = IO(core.module.io.cloneType)
    core.module.io <> io
  }
}

object CoreMain extends App {
  private val firtoolOpts = Seq(
    "-O=release",
    "--disable-annotation-unknown",
    "--strip-debug-info",
    "--lower-memories",
    "--add-vivado-ram-address-conflict-synthesis-bug-workaround",
    "--lowering-options=noAlwaysComb," +
      " disallowPortDeclSharing, disallowLocalVariables," +
      " emittedLineLength=120, explicitBitcast," +
      " locationInfoStyle=plain, disallowMuxInlining",
    "--disable-all-randomization"
  )
  private val config = new DefaultConfig
  private val firrtlOpts = args
  private val envInFPGA = config(DebugOptionsKey).FPGAPlatform
  private val enableDifftest = config(DebugOptionsKey).EnableDifftest || config(DebugOptionsKey).AlwaysBasicDiff
  private val enableChiselDB = config(DebugOptionsKey).EnableChiselDB
  private val enableConstantin = config(DebugOptionsKey).EnableConstantin
  Constantin.init(enableConstantin && !envInFPGA)
  ChiselDB.init(enableChiselDB && !envInFPGA)

  private val core = DisableMonitors(p => LazyModule(new CoreTop()(p)))(config)
  Generator.execute(firrtlOpts, core.module, firtoolOpts.toArray)
}