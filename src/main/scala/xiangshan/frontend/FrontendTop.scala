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

package xiangshan.frontend

import chisel3._
import freechips.rocketchip.tilelink.{TLManagerNode, TLSlavePortParameters, TLSlaveParameters, TLBuffer}
import freechips.rocketchip.diplomacy.{RegionType, TransferSizes, AddressSet}
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.DisableMonitors
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}
import top.{ArgParser, Generator}
import xiangshan.{HasXSParameter, XSCoreParamsKey, XSTileKey}

class FrontendTop()(implicit p: Parameters) extends LazyModule with HasXSParameter {

  private val mmioSlvParams = TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = Seq(AddressSet(0x0L, (0x1L << 48) - 1)),
      regionType = RegionType.UNCACHED,
      supportsGet = TransferSizes(1, 8),
      supportsPutFull = TransferSizes(1, 8),
      supportsPutPartial = TransferSizes(1, 8)
    )),
    beatBytes = 8,
  )

  private val l2NodeParameters = TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = Seq(AddressSet(0x0L, (0x1L << 48) - 1)),
      regionType = RegionType.CACHED,
      supportsAcquireT = TransferSizes(64, 64),
      supportsAcquireB = TransferSizes(64, 64),
      supportsArithmetic = TransferSizes(1, 64),
      supportsLogical = TransferSizes(1, 64),
      supportsGet = TransferSizes(1, 64),
      supportsPutFull = TransferSizes(1, 64),
      supportsPutPartial = TransferSizes(1, 64),
      executable = true
    )),
    beatBytes = 32,
    minLatency = 2,
    endSinkId = 256 * (1 << 2)
  )

  private val frontend = LazyModule(new Frontend())
  private val mmioNode = TLManagerNode(Seq(mmioSlvParams))
  private val managerNode = TLManagerNode(Seq(l2NodeParameters))
  mmioNode := TLBuffer.chainNode(1, Some(s"l1d_buffer")) :=* frontend.inner.instrUncache.clientNode
  managerNode := TLBuffer.chainNode(1, Some(s"l1d_buffer")) :=* frontend.inner.icache.clientNode

  lazy val module = new FrontendTopImp
  class FrontendTopImp extends LazyModuleImp(this) with HasXSParameter {
    override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
    val mmio = mmioNode.makeIOs()
    val l2 = managerNode.makeIOs()

    val io = IO(frontend.module.io.cloneType)
    frontend.module.io <> io
  }
}

// mill -i xiangshan.runMain xiangshan.frontend.FrontendMain
object FrontendMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(
    args :+ "--disable-always-basic-diff" :+ "--fpga-platform" :+ "--target" :+ "systemverilog" :+ "--split-verilog")

  val defaultConfig = config.alterPartial({ case XSCoreParamsKey => config(XSTileKey).head })
  private val frontend = DisableMonitors(p => LazyModule(new FrontendTop()(p)))(defaultConfig)

  Generator.execute(
    firrtlOpts :+ "--full-stacktrace" :+ "--target-dir" :+ "frontend",
    frontend.module,
    firtoolOpts ++ Seq(
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
  )
}

