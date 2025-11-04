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

package xiangshan.cache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utils._
import xs.utils._
import xs.utils.perf._
import xiangshan._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink.{TLArbiter, TLBundleA, TLBundleD, TLClientNode, TLEdgeOut, TLMasterParameters, TLMasterPortParameters}
import difftest._
import xs.utils.cache.{DeviceType, DeviceTypeField}


class UncacheFlushBundle extends Bundle {
  val valid = Output(Bool())
  val empty = Input(Bool())
}


class UncacheIO(implicit p: Parameters) extends DCacheBundle {
  val hartId = Input(UInt())
  val flush = Flipped(new UncacheFlushBundle)
  val lsq = Flipped(new UncacheWordIO)
  val monitorInfo = if(env.EnableHWMoniter) Some(Output(Vec(UncacheBufferSize, new UnCacheStuckInfo))) else None
}

class Uncache()(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false
  def idRange: Int = UncacheBufferSize

  val clientParameters = TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      "uncache",
      sourceId = IdRange(0, idRange)
    )),
    requestFields = Seq(DeviceTypeField())
  )
  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new UncacheImp(this)
}

class UncacheImp(outer: Uncache)extends LazyModuleImp(outer)
  with HasTLDump
  with HasXSParameter
  with HasPerfEvents
{
 val io = IO(new UncacheIO)

  val (bus, edge) = outer.clientNode.out.head
  val mem_acquire = bus.a
  val mem_grant   = bus.d

  // assign default values to output signals
  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.d.ready := true.B
  bus.e.valid := false.B
  bus.e.bits  := DontCare

  def getFirstOneOH(input: UInt): UInt = {
    assert(input.getWidth > 1)
    val output = WireInit(VecInit(input.asBools))
    (1 until input.getWidth).map(i => {
      output(i) := !input(i - 1, 0).orR && input(i)
    })
    output.asUInt
  }

  val entries = Seq.fill(UncacheBufferSize) { Module(new MMIOEntry(edge)) }
  val acquireArbiter = Module(new Arbiter(new TLBundleA(edge.bundle), UncacheBufferSize))

  val entriesReadyVec = entries.map(_.io.req.ready)
  val lsqReqEntrySel = getFirstOneOH(VecInit(entriesReadyVec).asUInt)

  val enqHasSamePA = io.lsq.req.valid && entries.map({ case e =>
    e.io.entryInfo.valid && (e.io.entryInfo.paddr(PAddrBits - 1, 3) === io.lsq.req.bits.addr(PAddrBits - 1, 3))
  }).reduce(_|_)

  io.lsq.req.ready := entriesReadyVec.reduce(_|_) && !enqHasSamePA

  for ((entry, i) <- entries.zipWithIndex) {
    entry.io.sourceId := i.U
    entry.io.mem_grant.valid := mem_grant.valid && (mem_grant.bits.source === i.U)
    entry.io.mem_grant.bits <> mem_grant.bits

    entry.io.req.valid := io.lsq.req.fire && lsqReqEntrySel(i)
    entry.io.req.bits := io.lsq.req.bits

    entry.io.resp.ready := true.B

    acquireArbiter.io.in(i) <> entry.io.mem_acquire
  }

  mem_acquire <> acquireArbiter.io.out

  val lsqRespSeq = entries.map(_.io.resp)
  io.lsq.resp.valid := lsqRespSeq.map(_.valid).reduce(_|_)
  io.lsq.resp.bits := PriorityMux(lsqRespSeq.map(_.valid),lsqRespSeq.map(_.bits))

  io.flush.empty := !entries.map(_.io.entryInfo.valid).reduce(_|_)

  // uncache store but memBackTypeMM should update the golden memory
  if (env.EnableDifftest) {
    val difftest = DifftestModule(new DiffUncacheMMStoreEvent, delay = 1)
    difftest.coreid := io.hartId
    difftest.index  := 0.U
    difftest.valid  := mem_acquire.fire && (mem_acquire.bits.opcode === MemoryOpConstants.M_XWR) && !mem_acquire.bits.user.lift(DeviceType).getOrElse(true.B)
    difftest.addr   := mem_acquire.bits.address >> log2Ceil(XLEN/8).U << log2Ceil(XLEN/8).U
    difftest.data   := mem_acquire.bits.data.asTypeOf(Vec(DataBytes, UInt(8.W)))
    difftest.mask   := mem_acquire.bits.mask
  }

  if(env.EnableHWMoniter){
    for(i <- 0 until UncacheBufferSize){
      io.monitorInfo.get(i).validVec := entries(i).io.entryInfo.valid
      io.monitorInfo.get(i).paddrVec := entries(i).io.entryInfo.paddr
    }
  }



  println(s"Uncahe Buffer Size: $UncacheBufferSize entries")
  def isStore: Bool = io.lsq.req.bits.cmd === MemoryOpConstants.M_XWR
  XSPerfAccumulate("mmio_store", io.lsq.req.fire && isStore)
  XSPerfAccumulate("mmio_load", io.lsq.req.fire && !isStore)
  val perfEvents = Seq(
    ("mmio_store", io.lsq.req.fire && isStore),
    ("mmio_load", io.lsq.req.fire && !isStore),
  )

  generatePerfEvent()
}
