/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

package top

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import difftest.DifftestModule
import xiangshan._
import utils._
import xs.utils._
import system._
import device._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.resources.{DTS, JSON}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.jtag.JTAGIO
import chisel3.experimental.{ChiselAnnotation, annotate}
import sifive.enterprise.firrtl.NestedPrefixModulesAnnotation
import xs.utils.perf.{DebugOptions, DebugOptionsKey, LogUtilsOptionsKey, PerfCounterOptionsKey}
import xs.utils.sram.{SramCtrlBundle, SramHelper}

abstract class BaseXSSoc()(implicit p: Parameters) extends LazyModule
  with BindingScope
{
  // val misc = LazyModule(new SoCMisc())
  lazy val dts = DTS(bindingTree)
  lazy val json = JSON(bindingTree)
}

class TLCtoTLUL(implicit p: Parameters) extends LazyModule {
  val node = TLAdapterNode(
//    clientFn  = { cp => cp.copy(clients = cp.clients.map(_.copy(supportsProbe = TransferSizes.none))) },
    clientFn  = { cp => cp },
    managerFn = { mp => mp.copy(
      managers = mp.managers.map(_.copy(
      supportsAcquireT = TransferSizes(64, 64),  // Manager不支持AcquireT
      supportsAcquireB = TransferSizes(64, 64),  // Manager不支持AcquireB
      supportsArithmetic = TransferSizes(1, 64),
      supportsLogical = TransferSizes(1, 64),
      supportsGet = TransferSizes(1, 64),
      supportsPutFull = TransferSizes(1, 64),
      supportsPutPartial = TransferSizes(1, 64),
      supportsHint  = TransferSizes(1, 64)
      )),
      beatBytes = 32,
      minLatency = 2,
      endSinkId = 256
    )}
  )
  lazy val module = new LazyModuleImp(this) {
    val (in, edgeIn) = node.in.head // TL-C 输入（A/B/C/E）
    val (out, edgeOut) = node.out.head // TL-UL 输出（A/D）

    val aChn = in.a.bits
    aChn.opcode := TLMessages.Get

    val cChn = WireInit(0.U.asTypeOf(in.a.bits))
    for((name, d) <- in.c.bits.elements){
      cChn.elements(name) := d
    }
    cChn.opcode := TLMessages.PutFullData
    cChn.param := DontCare
    cChn.mask := (-1).S.asUInt

    
    // 输出A通道选择
    out.a.bits := Mux(in.c.valid,cChn,aChn)
    out.a.valid := in.a.valid || in.c.valid
    
    // 输入端口ready信号
    in.a.ready := out.a.ready
    in.c.ready := out.a.ready
    
    // ----------------------------
    // B 通道：TL-UL不支持B通道
    // ----------------------------
    in.b.valid := false.B
    in.b.bits := DontCare
    
    // ----------------------------
    // D 通道：透传响应
    // ----------------------------
    in.d.bits := out.d.bits
    in.d.bits.opcode := Mux(out.d.bits.opcode === TLMessages.AccessAckData, TLMessages.GrantData, 
      Mux(out.d.bits.opcode === TLMessages.AccessAck, TLMessages.ReleaseAck, out.d.bits.opcode))
    in.d.valid := out.d.valid
    out.d.ready := in.d.ready
    
    // ----------------------------
    // E 通道：TL-UL无E通道
    // ----------------------------
    in.e.ready := true.B
    out.e.valid := false.B
  }
}


class XSTop()(implicit p: Parameters) extends BaseXSSoc() with HasSoCParameter
{
  val nocMisc = if (enableCHI) Some(LazyModule(new MemMisc())) else None
  val socMisc = if (!enableCHI) Some(LazyModule(new SoCMisc())) else None
  val misc: MemMisc = if (enableCHI) nocMisc.get else socMisc.get

  ResourceBinding {
    val width = ResourceInt(2)
    val model = "xiangshan," + os.read(os.resource / "publishVersion")
    val compatible = "freechips,rocketchip-unknown"
    Resource(ResourceAnchors.root, "model").bind(ResourceString(model))
    Resource(ResourceAnchors.root, "compat").bind(ResourceString(compatible + "-dev"))
    Resource(ResourceAnchors.soc, "compat").bind(ResourceString(compatible + "-soc"))
    Resource(ResourceAnchors.root, "width").bind(width)
    Resource(ResourceAnchors.soc, "width").bind(width)
    Resource(ResourceAnchors.cpus, "width").bind(ResourceInt(1))
    def bindManagers(xbar: TLNexusNode) = {
      ManagerUnification(xbar.edges.in.head.manager.managers).foreach{ manager =>
        manager.resources.foreach(r => r.bind(manager.toResource))
      }
    }
    if (!enableCHI) {
      bindManagers(misc.l3_xbar.get.asInstanceOf[TLNexusNode])
      bindManagers(misc.peripheralXbar.asInstanceOf[TLNexusNode])
    }
  }

  println(s"FPGASoC cores: $NumCores banks: $L3NBanks block size: $L3BlockSize bus size: $L3OuterBusWidth")

  val core = LazyModule(new XSCore()(p.alterPartial({
    case XSCoreParamsKey => tiles.head
  })))
  private val memBlock = core.memBlock.inner
  private val cacheXBar = LazyModule(new TLXbar)
  private val mmioXBar = LazyModule(new TLXbar)
  private val tlCvt = LazyModule(new TLCtoTLUL)

  val nmiIntNode = IntSourceNode(IntSourcePortSimple(1, NumCores, (new NonmaskableInterruptIO).elements.size))
  val nmi = InModuleBody(nmiIntNode.makeIOs())

  cacheXBar.node := TLBuffer.chainNode(2) := memBlock.frontendBridge.icache_node
  cacheXBar.node := TLBuffer.chainNode(2) := tlCvt.node := memBlock.dcache.clientNode
  cacheXBar.node := TLBuffer.chainNode(2) := memBlock.ptw_to_l2_buffer.node

  mmioXBar.node := TLBuffer.chainNode(2) := memBlock.frontendBridge.instr_uncache_node
  mmioXBar.node := TLBuffer.chainNode(2) := memBlock.uncache.clientNode

  memBlock.clint_int_sink := misc.clint.intnode
  memBlock.plic_int_sink :*= misc.plic.intnode
  memBlock.debug_int_sink := misc.debugModule.debug.dmOuter.dmOuter.intnode
  memBlock.nmi_int_sink := nmiIntNode

  misc.peripheralXbar := TLBuffer() := mmioXBar.node
  misc.l3_banked_xbar := TLBuffer() := cacheXBar.node

  class XSTopImp(wrapper: LazyModule) extends LazyRawModuleImp(wrapper) {
    override def provideImplicitClockToLazyChildren = true
    soc.XSTopPrefix.foreach { prefix =>
      val mod = this.toNamed
      annotate(new ChiselAnnotation {
        def toFirrtl = NestedPrefixModulesAnnotation(mod, prefix, true)
      })
    }
    FileRegisters.add("dts", dts)
    FileRegisters.add("graphml", graphML)
    FileRegisters.add("json", json)
    FileRegisters.add("plusArgs", freechips.rocketchip.util.PlusArgArtefacts.serialize_cHeader())

    val dma = socMisc.map(m => IO(Flipped(new VerilogAXI4Record(m.dma.elts.head.params))))
    val peripheral = IO(new VerilogAXI4Record(misc.peripheral.elts.head.params))
    val memory = IO(new VerilogAXI4Record(misc.memory.elts.head.params))

    socMisc match {
      case Some(m) =>
        m.dma.elements.head._2 <> dma.get.viewAs[AXI4Bundle]
        dontTouch(dma.get)
      case None =>
    }

    memory.viewAs[AXI4Bundle] <> misc.memory.elements.head._2
    peripheral.viewAs[AXI4Bundle] <> misc.peripheral.elements.head._2

    val io = IO(new Bundle {
      val clock = Input(Clock())
      val reset = Input(AsyncReset())
      val sram_config = Input(UInt(16.W))
      val extIntrs = Input(UInt(NrExtIntr.W))
      val pll0_lock = Input(Bool())
      val pll0_ctrl = Output(Vec(6, UInt(32.W)))
      val systemjtag = new Bundle {
        val jtag = Flipped(new JTAGIO(hasTRSTn = false))
        val reset = Input(AsyncReset()) // No reset allowed on top
        val mfr_id = Input(UInt(11.W))
        val part_number = Input(UInt(16.W))
        val version = Input(UInt(4.W))
      }
      val debug_reset = Output(Bool())
      val rtc_clock = Input(Bool())
      val cacheable_check = new TLPMAIO()
      val riscv_halt = Output(Bool())
      val riscv_rst_vec = Input(UInt(soc.PAddrBits.W))
      val traceCoreInterface = new Bundle {
        val fromEncoder = Input(new Bundle {
          val enable = Bool()
          val stall  = Bool()
        })
        val toEncoder   = Output(new Bundle {
          val cause     = UInt(TraceCauseWidth.W)
          val tval      = UInt(TraceTvalWidth.W)
          val priv      = UInt(TracePrivWidth.W)
          val iaddr     = UInt((TraceTraceGroupNum * TraceIaddrWidth).W)
          val itype     = UInt((TraceTraceGroupNum * TraceItypeWidth).W)
          val iretire   = UInt((TraceTraceGroupNum * TraceIretireWidthCompressed).W)
          val ilastsize = UInt((TraceTraceGroupNum * TraceIlastsizeWidth).W)
        })
      }
    })
    val reset_sync = withClockAndReset(io.clock, io.reset) { ResetGen(2, None) }
    val jtag_reset_sync = withClockAndReset(io.systemjtag.jtag.TCK, io.systemjtag.reset) { ResetGen(2, None) }

    // override LazyRawModuleImp's clock and reset
    childClock := io.clock
    childReset := reset_sync

    // output
    io.debug_reset := misc.module.debug_module_io.debugIO.ndreset

    // input
    dontTouch(io)
    dontTouch(memory)
    misc.module.ext_intrs := io.extIntrs
    misc.module.rtc_clock := io.rtc_clock
    misc.module.pll0_lock := io.pll0_lock
    misc.module.cacheable_check <> io.cacheable_check

    io.pll0_ctrl <> misc.module.pll0_ctrl

    val msiInfo = WireInit(0.U.asTypeOf(ValidIO(new MsiInfoBundle)))


    core.module.io := DontCare
    core.module.io.hartId := 0.U
    core.module.io.msiInfo := msiInfo
    core.module.io.clintTime := misc.module.clintTime
    io.riscv_halt := core.module.io.cpu_halt
    io.traceCoreInterface <> core.module.io.traceCoreInterface
    core.module.io.reset_vector := io.riscv_rst_vec
    dontTouch(core.module.io.dft) := 0.U.asTypeOf(core.module.io.dft)

    misc.module.debug_module_io.resetCtrl.hartIsInReset.foreach( _ :=core.module.io.resetInFrontend )
    misc.module.debug_module_io.clock := io.clock
    misc.module.debug_module_io.reset := reset_sync

    misc.module.debug_module_io.debugIO.reset := misc.module.reset
    misc.module.debug_module_io.debugIO.clock := io.clock
    // TODO: delay 3 cycles?
    misc.module.debug_module_io.debugIO.dmactiveAck := misc.module.debug_module_io.debugIO.dmactive
    // jtag connector
    misc.module.debug_module_io.debugIO.systemjtag.foreach { x =>
      x.jtag        <> io.systemjtag.jtag
      x.reset       := jtag_reset_sync
      x.mfr_id      := io.systemjtag.mfr_id
      x.part_number := io.systemjtag.part_number
      x.version     := io.systemjtag.version
    }

    withClockAndReset(io.clock, reset_sync) {
      // Modules are reset one by one
      // reset ----> SYNC --> {SoCMisc, L3 Cache, Cores}
      val resetChain = Seq(Seq(misc.module))
      ResetGen(resetChain, reset_sync, None, !debugOpts.ResetGen)
      // Ensure that cores could be reset when DM disable `hartReset` or l3cacheOpt.isEmpty.
      val dmResetReqVec = misc.module.debug_module_io.resetCtrl.hartResetReq.getOrElse(0.U.asTypeOf(Vec(1, Bool())))
      val syncResetCores = misc.module.reset.asBool
      ResetGen(Seq(Seq(core.module)), (syncResetCores || dmResetReqVec.head).asAsyncReset, None, !debugOpts.ResetGen)
    }

  }

  lazy val module = new XSTopImp(this)
}

object TopMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)

  // tools: init to close dpi-c when in fpga
  val envInFPGA = config(DebugOptionsKey).FPGAPlatform
  val enableDifftest = config(DebugOptionsKey).EnableDifftest || config(DebugOptionsKey).AlwaysBasicDiff
  val enableChiselDB = config(DebugOptionsKey).EnableChiselDB
  val enableConstantin = config(DebugOptionsKey).EnableConstantin
  Constantin.init(enableConstantin && !envInFPGA)
  ChiselDB.init(enableChiselDB && !envInFPGA)


  val soc = DisableMonitors(p => LazyModule(new XSTop()(p)))(config)

  Generator.execute(firrtlOpts, soc.module, firtoolOpts)

  // generate difftest bundles (w/o DifftestTopIO)
  if (enableDifftest) {
    DifftestModule.finish("XiangShan", false, Seq())
  }


  // FileRegisters.add("dts", soc.dts)
  // FileRegisters.add("graphml", soc.graphML)
  // FileRegisters.add("json", soc.json)
  // FileRegisters.add("plusArgs", freechips.rocketchip.util.PlusArgArtefacts.serialize_cHeader())
  FileRegisters.write(fileDir = "./build", filePrefix = "XSTop.")
}
