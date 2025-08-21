package xiangshan.cache

import chisel3.util._
import chisel3._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import xs.utils.PriorityMuxWithFlag
import xs.utils.cache.{DeviceType}
import xs.utils.perf.XSDebug


// One miss entry deals with one mmio request
class MMIOEntry(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule
{
  val io = IO(new Bundle {
    //  MSHR ID
    val sourceId = Input(UInt(log2Up(UncacheBufferSize).W))
    //  Client requests
    val req = Flipped(DecoupledIO(new UncacheWordReq))
    val resp = DecoupledIO(new DCacheWordRespWithError)

    //  TileLink
    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    val entryInfo = Output(new Bundle(){
      val valid = Bool()
      val paddr = UInt(PAddrBits.W)
    })

//    val difftestInfo = Output(new Bundle(){
//      val valid = Bool()
//      val isDevice = Bool()
//      val isStore = Bool()
//      val addr = UInt(PAddrBits.W)
//      val data = UInt(XLEN.W)
//      val mask = UInt((XLEN/8).W)
//    })
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

  //  Assign default values to output signals.
  io.req.ready := false.B
  io.resp.valid := false.B
  io.resp.bits := DontCare
  io.resp.bits.isStore := req.cmd === MemoryOpConstants.M_XWR

  io.mem_acquire.valid := false.B
  io.mem_acquire.bits := DontCare
  io.mem_grant.ready := false.B

  io.entryInfo.valid := state =/= s_invalid
  io.entryInfo.paddr := req.addr

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
    fromSource      = io.sourceId,
    toAddress       = req.addr,
    lgSize          = lgSize
  )._2

  val store = edge.Put(
    fromSource      = io.sourceId,
    toAddress       = req.addr,
    lgSize          = lgSize,
    data            = req.data,
    mask            = req.mask
  )._2

  XSDebug("entry: %d state: %d\n", io.sourceId, state)

  when (state === s_refill_req) {
    io.mem_acquire.valid := true.B
    io.mem_acquire.bits := Mux(storeReq, store, load)
    io.mem_acquire.bits.user.lift(DeviceType).foreach(_ := req.device)
    when (io.mem_acquire.fire) {
      state := s_refill_resp
    }
  }

  val (_, _, refill_done, _) = edge.addr_inc(io.mem_grant)
  when (state === s_refill_resp) {
    io.mem_grant.ready := true.B

    when (io.mem_grant.fire) {
      resp_data := io.mem_grant.bits.data
      resp_nderr := io.mem_grant.bits.denied | io.mem_grant.bits.corrupt
      assert(refill_done, "Uncache response should be one beat only!")
      state := Mux(storeReq && !req.device, s_invalid, s_send_resp)
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
    io.resp.bits.isNC  := !req.device

    when (io.resp.fire) {
      state := s_invalid
    }
  }

//  io.difftestInfo.valid := state =/= s_invalid
//  io.difftestInfo.isStore := storeReq
//  io.difftestInfo.addr := Mux(storeReq, store.address, load.address)
//  io.difftestInfo.data := Mux(storeReq, store.data, load.data)
//  io.difftestInfo.mask := Mux(storeReq, store.mask, load.mask)
//  io.difftestInfo.isDevice := req.device


}