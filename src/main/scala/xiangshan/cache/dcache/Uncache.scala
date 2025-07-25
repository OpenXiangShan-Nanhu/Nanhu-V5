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
import xs.utils.cache.{MemBackTypeMM, MemBackTypeMMField, MemPageTypeNC, MemPageTypeNCField}
import difftest._

class UncachePtr(implicit p: Parameters) extends CircularQueuePtr[UncachePtr](
  p => p(XSCoreParamsKey).UncacheBufferSize
){

}

object UncachePtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): UncachePtr = {
    val ptr = Wire(new UncachePtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class UncacheFlushBundle extends Bundle {
  val valid = Output(Bool())
  val empty = Input(Bool())
}

// One miss entry deals with one mmio request
class MMIOEntry(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule
{
  val io = IO(new Bundle {
    //  MSHR ID
    val hartId = Input(UInt())
    //  Client requests
    val req = Flipped(DecoupledIO(new UncacheWordReq))
    val resp = DecoupledIO(new DCacheWordRespWithError)

    //  TileLink
    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    // This entry is valid.
    val invalid = Output(Bool())
    //  This entry is selected.
    val select = Input(Bool())
  })
  //  ================================================
  //  FSM state description:
  //  s_invalid     : Entry is invalid.
  //  s_refill_req  : Send Acquire request.
  //  s_refill_resp : Wait for Grant response.
  //  s_send_resp   : Send Uncache response.
  val s_invalid :: s_refill_req :: s_refill_resp :: s_send_resp :: Nil = Enum(4)
  val state = RegInit(s_invalid)

  val req = Reg(new UncacheWordReq)
  val resp_data = Reg(UInt(DataBits.W))
  val resp_nderr = Reg(Bool())
  def storeReq = req.cmd === MemoryOpConstants.M_XWR

  io.invalid := state === s_invalid
  //  Assign default values to output signals.
  io.req.ready := false.B
  io.resp.valid := false.B
  io.resp.bits := DontCare
  io.resp.bits.isStore := req.cmd === MemoryOpConstants.M_XWR

  io.mem_acquire.valid := false.B
  io.mem_acquire.bits := DontCare
  io.mem_grant.ready := false.B

  //  Receive request
  when (state === s_invalid) {
    io.req.ready := true.B

    when (io.req.fire) {
      req := io.req.bits
      req.addr := io.req.bits.addr
      resp_nderr := false.B
      state := s_refill_req
    }
  }

  //  Refill
  //  TODO: determine 'lgSize' in memend
  val size = PopCount(req.mask)
  val (lgSize, legal) = PriorityMuxWithFlag(Seq(
    1.U -> 0.U,
    2.U -> 1.U,
    4.U -> 2.U,
    8.U -> 3.U
  ).map(m => (size===m._1) -> m._2))
  assert(!(io.mem_acquire.valid && !legal))

  val load = edge.Get(
    fromSource      = io.hartId,
    toAddress       = req.addr,
    lgSize          = lgSize
  )._2

  val store = edge.Put(
    fromSource      = io.hartId,
    toAddress       = req.addr,
    lgSize          = lgSize,
    data            = req.data,
    mask            = req.mask
  )._2

  XSDebug("entry: %d state: %d\n", io.hartId, state)

  when (state === s_refill_req) {
    io.mem_acquire.valid := true.B && io.select
    io.mem_acquire.bits := Mux(storeReq, store, load)
    io.mem_acquire.bits.user.lift(MemBackTypeMM).foreach(_ := req.nc)
    io.mem_acquire.bits.user.lift(MemPageTypeNC).foreach(_ := req.nc)
    when (io.mem_acquire.fire) {
      state := s_refill_resp
    }
  }

  val (_, _, refill_done, _) = edge.addr_inc(io.mem_grant)
  when (state === s_refill_resp) {
    io.mem_grant.ready := true.B

    when (io.mem_grant.fire) {
      resp_data := io.mem_grant.bits.data
      resp_nderr := io.mem_grant.bits.denied
      io.resp.valid := req.nc && storeReq
      io.resp.bits.isNC  := req.nc
      assert(refill_done, "Uncache response should be one beat only!")
      state := Mux(storeReq && req.nc, s_invalid, s_send_resp)
    }
  }

  //  Response
  when (state === s_send_resp) {
    io.resp.valid := true.B
    io.resp.bits.data   := resp_data
    // meta data should go with the response
    io.resp.bits.id     := req.id
    io.resp.bits.miss   := false.B
    io.resp.bits.replay := false.B
    io.resp.bits.tag_error := false.B
    io.resp.bits.error := false.B
    io.resp.bits.nderr := resp_nderr
    io.resp.bits.isNC  := req.nc

    when (io.resp.fire) {
      state := s_invalid
    }
  }

  //  End
}

class UncacheIO(implicit p: Parameters) extends DCacheBundle {
  val hartId = Input(UInt())
  val flush = Flipped(new UncacheFlushBundle)
  val lsq = Flipped(new UncacheWordIO)
}

// convert DCacheIO to TileLink
// for Now, we only deal with TL-UL

class Uncache()(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false
  def idRange: Int = UncacheBufferSize

  val clientParameters = TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      "uncache",
      sourceId = IdRange(0, idRange)
    )),
    requestFields = Seq(MemBackTypeMMField(), MemPageTypeNCField())
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

  val req  = io.lsq.req
  val resp = io.lsq.resp
  val mem_acquire = bus.a
  val mem_grant   = bus.d

  val req_ready = WireInit(false.B)

  // assign default values to output signals
  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.d.ready := false.B
  bus.e.valid := false.B
  bus.e.bits  := DontCare

  val enqPtr = RegInit(0.U.asTypeOf(new UncachePtr))
  val issPtr = RegInit(0.U.asTypeOf(new UncachePtr))
  val deqPtr = RegInit(0.U.asTypeOf(new UncachePtr))

  io.lsq.resp.valid := false.B
  io.lsq.resp.bits := DontCare

  val entries = Seq.fill(UncacheBufferSize) { Module(new MMIOEntry(edge)) }
  for ((entry, i) <- entries.zipWithIndex) {
    entry.io.hartId := io.hartId

   //  Enqueue
    entry.io.req.valid := (i.U === enqPtr.value) && req.valid
    entry.io.req.bits := req.bits

    when (i.U === enqPtr.value) {
      req_ready := entry.io.req.ready
    }

    //  Acquire
    entry.io.select := (i.U === issPtr.value)

    //  Grant
    entry.io.mem_grant.valid := false.B
    entry.io.mem_grant.bits := DontCare
    when (i.U === deqPtr.value) {
      entry.io.mem_grant <> mem_grant
    }

    entry.io.resp.ready := false.B
    when (i.U === deqPtr.value) {
      io.lsq.resp <> entry.io.resp
    }

  }

  io.lsq.req.ready := req_ready

    //  Enqueue
    when (req.fire) {
      enqPtr := enqPtr + 1.U
    }

    //  Issue
    when (mem_acquire.fire) {
      issPtr := issPtr + 1.U
    }

    when (io.lsq.resp.fire) {
      deqPtr := deqPtr + 1.U
    }

  TLArbiter.lowestFromSeq(edge, mem_acquire, entries.map(_.io.mem_acquire))
  val invalid_entries = PopCount(entries.map(_.io.invalid))
  io.flush.empty := invalid_entries === UncacheBufferSize.U

  println(s"Uncahe Buffer Size: $UncacheBufferSize entries")

  // uncache store but memBackTypeMM should update the golden memory
  if (env.EnableDifftest) {
    val difftest = DifftestModule(new DiffUncacheMMStoreEvent, delay = 1)
    difftest.coreid := io.hartId
    difftest.index  := 0.U
    difftest.valid  := mem_acquire.fire && (mem_acquire.bits.opcode === MemoryOpConstants.M_XWR) && mem_acquire.bits.user.lift(MemBackTypeMM).getOrElse(false.B)
    difftest.addr   := mem_acquire.bits.address
    difftest.data   := mem_acquire.bits.data.asTypeOf(Vec(DataBytes, UInt(8.W)))
    difftest.mask   := mem_acquire.bits.mask
  }

  // print all input/output requests for debug purpose
  // print req/resp
  XSDebug(req.fire, "req cmd: %x addr: %x data: %x mask: %x\n",
    req.bits.cmd, req.bits.addr, req.bits.data, req.bits.mask)
  XSDebug(resp.fire, "data: %x\n", req.bits.data)

  //  Performance Counters
  def isStore: Bool = io.lsq.req.bits.cmd === MemoryOpConstants.M_XWR
  XSPerfAccumulate("mmio_store", io.lsq.req.fire && isStore)
  XSPerfAccumulate("mmio_load", io.lsq.req.fire && !isStore)
  XSPerfAccumulate("mmio_outstanding", mem_acquire.fire && (deqPtr =/= issPtr))
  val perfEvents = Seq(
    ("mmio_store", io.lsq.req.fire && isStore),
    ("mmio_load", io.lsq.req.fire && !isStore),
    ("mmio_outstanding", mem_acquire.fire && (deqPtr =/= issPtr))
  )

  generatePerfEvent()
  //  End
}
