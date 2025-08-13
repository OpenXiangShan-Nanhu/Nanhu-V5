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

package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import utils._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLArbiter, TLBundleA, TLBundleD, TLClientNode, TLEdgeOut, TLMasterParameters, TLMasterPortParameters}
import xs.utils.cache.{MemBackTypeMM, MemBackTypeMMField, MemPageTypeNC, MemPageTypeNCField}
import xiangshan._
import xiangshan.frontend._

class InsUncacheReq(implicit p: Parameters) extends ICacheBundle {
  val addr: UInt = UInt(PAddrBits.W)
  val memBackTypeMM: Bool = Bool() // !pmp.mmio, pbmt.nc/io on a main memory region
  val memPageTypeNC: Bool = Bool() // pbmt.nc
}

class InsUncacheResp(implicit p: Parameters) extends ICacheBundle {
  val data:    UInt = UInt(maxInstrLen.W)
  val corrupt: Bool = Bool()
}

class InstrMMIOEntry(edge: TLEdgeOut)(implicit p: Parameters) extends XSModule
  with HasICacheParameters with HasIFUConst {
  val io = IO(new Bundle {
    val id = Input(UInt(log2Up(cacheParams.nMMIOs).W))
    val flush = Input(Bool())

    val req = Flipped(DecoupledIO(new InsUncacheReq))
    val resp = DecoupledIO(new InsUncacheResp)

    val mmio_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mmio_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
  })

  private val s_invalid :: s_refill_req :: s_refill_resp :: s_send_resp :: Nil = Enum(4)
  private val state          = RegInit(s_invalid)

  private val req            = RegEnable(io.req.bits, io.req.fire)
  private val respDataReg    = RegEnable(io.mmio_grant.bits.data, io.mmio_grant.fire)
  private val respCorruptReg = RegEnable(io.mmio_grant.bits.corrupt, false.B, io.mmio_grant.fire)

  private val needFlush      = RegInit(false.B)
  when(io.flush && (state =/= s_invalid) && (state =/= s_send_resp)){
    needFlush := true.B
  }.elsewhen((state === s_send_resp) && needFlush){
    needFlush := false.B
  }

  switch(state) {
    is(s_invalid) {
      state := Mux(io.req.fire, s_refill_req, s_invalid)
    }
    is(s_refill_req) {
      state := Mux(io.mmio_acquire.fire, s_refill_resp, s_refill_req)
    }
    is(s_refill_resp) {
      state := Mux(io.mmio_grant.fire, s_send_resp, s_refill_resp)
    }
    is(s_send_resp) {
      state := Mux(io.resp.fire || needFlush, s_invalid, s_send_resp)
    }
  }

  io.req.ready         := state === s_invalid
  io.resp.valid        := state === s_send_resp && !needFlush
  io.resp.bits.corrupt := respCorruptReg
  io.resp.bits.data    := MuxLookup(req.addr(2,1), respDataReg(31,0))(Seq(
    "b00".U -> respDataReg(31,0),
    "b01".U -> respDataReg(47,16),
    "b10".U -> respDataReg(63,32),
    "b11".U -> Cat(0.U(16.W), respDataReg(63,48))
  ))

  private val address_aligned = req.addr(req.addr.getWidth - 1, log2Ceil(mmioBusBytes))

  io.mmio_grant.ready   := state === s_refill_resp
  io.mmio_acquire.valid := state === s_refill_req
  io.mmio_acquire.bits  := edge.Get(
    fromSource = io.id,
    toAddress = Cat(address_aligned, 0.U(log2Ceil(mmioBusBytes).W)),
    lgSize = log2Ceil(mmioBusBytes).U
  )._2
  io.mmio_acquire.bits.user.lift(MemBackTypeMM).foreach(_ := req.memBackTypeMM)
  io.mmio_acquire.bits.user.lift(MemPageTypeNC).foreach(_ := req.memPageTypeNC)
}

class InstrUncache()(implicit p: Parameters) extends LazyModule with HasICacheParameters {
  override def shouldBeInlined: Boolean = false

  val clientParameters = TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      "InstrUncache",
      sourceId = IdRange(0, cacheParams.nMMIOs)
    )),
    requestFields = Seq(MemBackTypeMMField(), MemPageTypeNCField())
  )
  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new InstrUncacheImp(this)
}

class InstrUncacheImp(outer: InstrUncache) extends LazyModuleImp(outer) with HasICacheParameters with HasTLDump {
  val io = IO(new Bundle{
    val req = Flipped(DecoupledIO(new InsUncacheReq))
    val resp = DecoupledIO(new InsUncacheResp)
    val flush = Input(Bool())
  })

  val (bus, edge) = outer.clientNode.out.head

  private val mmio_acquire = bus.a
  private val mmio_grant   = bus.d

  private val entry_alloc_idx = Wire(UInt())
  private val resp_arb = Module(new Arbiter(new InsUncacheResp, cacheParams.nMMIOs))

  // assign default values to output signals
  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.e.valid := false.B
  bus.e.bits  := DontCare

  private val entries = (0 until cacheParams.nMMIOs) map { i =>
    val entry = Module(new InstrMMIOEntry(edge))

    entry.io.id := i.U(log2Up(cacheParams.nMMIOs).W)
    entry.io.flush := io.flush

    // entry req
    entry.io.req.valid := io.req.valid && (i.U === entry_alloc_idx)
    entry.io.req.bits  := io.req.bits

    entry.io.mmio_grant.valid := mmio_grant.valid && (i.U === mmio_grant.bits.source)
    entry.io.mmio_grant.bits  := mmio_grant.bits

    entry
  }

  entry_alloc_idx := PriorityEncoder(entries.map(_.io.req.ready))

  io.req.ready := entries.map(_.io.req.ready).reduce(_||_)
  resp_arb.io.in <> entries.map(_.io.resp)
  io.resp <> resp_arb.io.out

  TLArbiter.lowestFromSeq(edge, mmio_acquire, entries.map(_.io.mmio_acquire))
  mmio_grant.ready := true.B
}
