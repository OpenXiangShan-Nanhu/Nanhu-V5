package xiangshan.cache

import chisel3._
import chisel3.util._
import difftest._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.mem.trace._
import xiangshan.mem.LqPtr
import xs.utils._
import xs.utils.perf._
import xs.utils.tl._
import xs.utils.cache.common._
import xs.utils.cache.common.{AliasKey, DirtyKey, IsKeywordKey, PrefetchKey, VaddrKey}

class MissEntry(edge: TLEdgeOut, reqNum: Int)(implicit p: Parameters) extends DCacheModule
  with HasCircularQueuePtrHelper
{
  val io = IO(new Bundle() {
    val hartId = Input(UInt(hartIdLen.W))
    // MSHR ID
    val id = Input(UInt(log2Up(cfg.nMissEntries).W))
    // client requests
    // MSHR update request, MSHR state and addr will be updated when req.fire
    val req = Flipped(ValidIO(new MissReqWoStoreData))
    val wbq_block_miss_req = Input(Bool())
    // pipeline reg
    val miss_req_pipe_reg = Input(new MissReqPipeRegBundle(edge))
    // allocate this entry for new req
    val primary_valid = Input(Bool())
    // this entry is free and can be allocated to new reqs
    val primary_ready = Output(Bool())
    // this entry is busy, but it can merge the new req
    val secondary_ready = Output(Bool())
    // this entry is busy and it can not merge the new req
    val secondary_reject = Output(Bool())
    // way selected for replacing, used to support plru update
    // bus
    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val mem_finish = DecoupledIO(new TLBundleE(edge.bundle))

    //    val queryME = Vec(reqNum, Flipped(new DCacheMEQueryIOBundle))

    // send refill info to load queue, useless now
    val refill_to_ldq = ValidIO(new Refill)

    val sbuffer_id = Output(UInt(reqIdWidth.W))
    val req_source = Output(UInt(sourceTypeWidth.W))

    // replace pipe
    val l2_hint = Input(Valid(new L2ToL1Hint())) // Hint from L2 Cache
    val isCMO = Output(Bool())

    // main pipe: amo miss
    val main_pipe_req = DecoupledIO(new MainPipeReq)
    val main_pipe_resp = Input(Bool())
    val main_pipe_refill_resp = Input(Bool())
    val main_pipe_replay = Input(Bool())

    // for main pipe s2
    val refill_info = ValidIO(new MissQueueRefillInfo)

    val block_addr = ValidIO(UInt(PAddrBits.W))
    val replace = Flipped(new MissQueueBlockIO)

    val req_addr = ValidIO(UInt(PAddrBits.W))

    val req_handled_by_this_entry = Output(Bool())

    val forwardInfo = Output(new MissEntryForwardIO)
    val l2_pf_store_only = Input(Bool())

    // whether the pipeline reg has send out an acquire
    val acquire_fired_by_pipe_reg = Input(Bool())

    val perf_pending_prefetch = Output(Bool())
    val perf_pending_normal   = Output(Bool())

    val rob_head_query = new DCacheBundle {
      val vaddr = Input(UInt(VAddrBits.W))
      val query_valid = Input(Bool())

      val resp = Output(Bool())

      def hit(e_vaddr: UInt): Bool = {
        require(e_vaddr.getWidth == VAddrBits)
        query_valid && vaddr(VAddrBits - 1, DCacheLineOffset) === e_vaddr(VAddrBits - 1, DCacheLineOffset)
      }
    }

    val latency_monitor = new DCacheBundle {
      val load_miss_refilling  = Output(Bool())
      val store_miss_refilling = Output(Bool())
      val amo_miss_refilling   = Output(Bool())
      val pf_miss_refilling    = Output(Bool())
    }

    val prefetch_info = new DCacheBundle {
      val late_prefetch = Output(Bool())
    }
    val nMaxPrefetchEntry = Input(UInt(64.W))
    val matched = Output(Bool())
  })

  assert(!RegNext(io.primary_valid && !io.primary_ready))

  val req = Reg(new MissReqWoStoreData)
  val req_primary_fire = Reg(new MissReqWoStoreData) // for perf use
  val req_store_mask = Reg(UInt(cfg.blockBytes.W))
  val req_valid = RegInit(false.B)
  io.isCMO := req.isCMO && req_valid

  val set = addr_to_dcache_set(req.vaddr)
  // initial keyword
  val isKeyword = RegInit(false.B)

  val miss_req_pipe_reg_bits = io.miss_req_pipe_reg.req

  val input_req_is_prefetch = isPrefetch(miss_req_pipe_reg_bits.cmd)

  val s_acquire = RegInit(true.B)
  val s_grantack = RegInit(true.B)
  val s_mainpipe_req = RegInit(true.B)

  val w_grantfirst = RegInit(true.B)
  val w_grantlast = RegInit(true.B)
  val w_mainpipe_resp = RegInit(true.B)
  val w_refill_resp = RegInit(true.B)
  val w_l2hint = RegInit(true.B)

  val mainpipe_req_fired = RegInit(true.B)

  val release_entry = s_grantack && w_mainpipe_resp && w_refill_resp && !req.isCMO || req.isCMO && w_grantlast

  val acquire_not_sent = !s_acquire && !io.mem_acquire.ready
  val data_not_refilled = !w_grantfirst

  val error = RegInit(false.B)
  val prefetch = RegInit(false.B)
  val access = RegInit(false.B)

  val should_refill_data_reg =  Reg(Bool())
  val should_refill_data = WireInit(should_refill_data_reg)

  val should_replace = RegInit(false.B)

  val full_overwrite = Reg(Bool())

//  val (_, _, refill_done, refill_count) = edge.count(io.mem_grant)
//  val hasData = RegInit(true.B)
  val hasData = !req.full_overwrite
  val isDirty = RegInit(false.B)
  val refill_count = RegInit(0.U(2.W))
  val refill_done = Mux(hasData, refill_count === 1.U && io.mem_grant.fire, refill_count === 0.U && io.mem_grant.fire)
  when(io.mem_grant.fire){
    refill_count := refill_count + 1.U
  }

  val grant_param = Reg(UInt(TLPermissions.bdWidth.W))

  // refill data with store data, this reg will be used to store:
  // 1. store data (if needed), before l2 refill data
  // 2. store data and l2 refill data merged result (i.e. new cacheline taht will be write to data array)
  // raw data refilled to l1 by l2
  val refill_data_raw = Reg(Vec(blockBytes/beatBytes, UInt(beatBits.W)))

  // allocate current miss queue entry for a miss req
  val primary_fire = WireInit(io.req.valid && io.primary_ready && io.primary_valid && !io.req.bits.cancel && !io.wbq_block_miss_req)
  val primary_accept = WireInit(io.req.valid && io.primary_ready && io.primary_valid && !io.req.bits.cancel)
  // merge miss req to current miss queue entry
  val secondary_fire = WireInit(io.req.valid && io.secondary_ready && !io.req.bits.cancel && !io.wbq_block_miss_req)
  val secondary_accept = WireInit(io.req.valid && io.secondary_ready && !io.req.bits.cancel)

  val req_handled_by_this_entry = primary_accept || secondary_accept

  // for perf use
  val secondary_fired = RegInit(false.B)

  io.perf_pending_prefetch := req_valid && prefetch && !secondary_fired
  io.perf_pending_normal   := req_valid && (!prefetch || secondary_fired)

  io.rob_head_query.resp   := io.rob_head_query.hit(req.vaddr) && req_valid

  io.req_handled_by_this_entry := req_handled_by_this_entry


  io.replace.block := req_valid &&
    get_block_addr(req.addr) === get_block_addr(io.replace.req.bits.addr) &&
    is_alias_match(req.vaddr, io.replace.req.bits.vaddr)

  when (release_entry && req_valid) {
    req_valid := false.B
  }

  when (io.miss_req_pipe_reg.alloc && !io.miss_req_pipe_reg.cancel) {
    assert(RegNext(primary_fire), "after 1 cycle of primary_fire, entry will be allocated")
    req_valid := true.B
    refill_count := 0.U

    req := miss_req_pipe_reg_bits.toMissReqWoStoreData()
    req_primary_fire := miss_req_pipe_reg_bits.toMissReqWoStoreData()
    req.addr := get_block_addr(miss_req_pipe_reg_bits.addr)
    //only  load miss need keyword
    isKeyword := Mux(miss_req_pipe_reg_bits.isFromLoad, miss_req_pipe_reg_bits.vaddr(5).asBool,false.B)

    s_acquire := io.acquire_fired_by_pipe_reg
    s_grantack := false.B
    s_mainpipe_req := false.B

    w_grantfirst := false.B
    w_grantlast := false.B
    w_l2hint := false.B
    mainpipe_req_fired := false.B

    full_overwrite := miss_req_pipe_reg_bits.isFromStore && miss_req_pipe_reg_bits.full_overwrite

    when (!miss_req_pipe_reg_bits.isFromAMO) {
      w_refill_resp := false.B
    }

    when (miss_req_pipe_reg_bits.isFromAMO) {
      w_mainpipe_resp := false.B
    }

    should_refill_data_reg := miss_req_pipe_reg_bits.isFromLoad
    error := false.B
    prefetch := input_req_is_prefetch && !io.miss_req_pipe_reg.prefetch_late_en(io.req.bits, io.req.valid)
    access := false.B
    secondary_fired := false.B
  }

  when (io.miss_req_pipe_reg.merge && !io.miss_req_pipe_reg.cancel) {
    assert(RegNext(secondary_fire) || RegNext(RegNext(primary_fire)), "after 1 cycle of secondary_fire or 2 cycle of primary_fire, entry will be merged")
    assert(miss_req_pipe_reg_bits.req_coh.state <= req.req_coh.state || (prefetch && !access))
    assert(!(miss_req_pipe_reg_bits.isFromAMO || req.isFromAMO))
    // use the most uptodate meta
    req.req_coh := miss_req_pipe_reg_bits.req_coh

    isKeyword := Mux(
      before_req_sent_can_merge(miss_req_pipe_reg_bits),
      before_req_sent_merge_iskeyword(miss_req_pipe_reg_bits),
      isKeyword)
    assert(!miss_req_pipe_reg_bits.isFromPrefetch, "can not merge a prefetch req, late prefetch should always be ignored!")

    when (miss_req_pipe_reg_bits.isFromStore) {
      req := miss_req_pipe_reg_bits
      req.addr := get_block_addr(miss_req_pipe_reg_bits.addr)
      full_overwrite := miss_req_pipe_reg_bits.isFromStore && miss_req_pipe_reg_bits.full_overwrite
      assert(is_alias_match(req.vaddr, miss_req_pipe_reg_bits.vaddr), "alias bits should be the same when merging store")
    }

    should_refill_data := should_refill_data_reg || miss_req_pipe_reg_bits.isFromLoad
    should_refill_data_reg := should_refill_data
    when (!input_req_is_prefetch) {
      access := true.B // when merge non-prefetch req, set access bit
    }
    secondary_fired := true.B
  }

  when (io.mem_acquire.fire) {
    s_acquire := true.B
    when (req.isCMO) {
      assert(!req.cancel, "CMO Acquire: entry must not be canceled")
    }
  }

  when (io.mem_grant.fire) {
    w_grantfirst := true.B
    grant_param := io.mem_grant.bits.param

    when(req.isCMO && io.mem_grant.bits.opcode === TLMessages.CBOAck) {
      w_grantfirst := true.B
      w_grantlast  := true.B
//      hasData      := false.B
    } .elsewhen (edge.hasData(io.mem_grant.bits)) {
      w_grantlast := w_grantlast || refill_done
//      hasData := true.B
    }.otherwise {
      // Grant
      assert(full_overwrite)
      w_grantlast := true.B
//      hasData := false.B
    }

    error := io.mem_grant.bits.denied || io.mem_grant.bits.corrupt || error

    refill_data_raw(refill_count ^ isKeyword) := io.mem_grant.bits.data
    isDirty := io.mem_grant.bits.echo.lift(DirtyKey).getOrElse(false.B)
  }

  when (io.mem_finish.fire) {
    s_grantack := true.B
  }

  when (io.main_pipe_req.fire) {
    s_mainpipe_req := true.B
    mainpipe_req_fired := true.B
  }

  when (io.main_pipe_replay) {
    s_mainpipe_req := false.B
  }

  when (io.main_pipe_resp) {
    w_mainpipe_resp := true.B
  }

  when(io.main_pipe_refill_resp) {
    w_refill_resp := true.B
  }

  w_l2hint :=Mux(req.isFromStore || req.isFromAMO, RegNext(io.mem_grant.fire) && io.mem_grant.fire, io.l2_hint.valid)

  def before_req_sent_can_merge(new_req: MissReqWoStoreData): Bool = {
    // acquire_not_sent && (new_req.isFromLoad || new_req.isFromStore)

    // Since most acquire requests have been issued from pipe_reg,
    // the number of such merge situations is currently small,
    // So dont Merge anything for better timing.
    false.B
  }

  def before_data_refill_can_merge(new_req: MissReqWoStoreData): Bool = {
    data_not_refilled && new_req.isFromLoad
  }

  // Note that late prefetch will be ignored

  def should_merge(new_req: MissReqWoStoreData): Bool = {
    val block_match = get_block(req.addr) === get_block(new_req.addr)
    val alias_match = is_alias_match(req.vaddr, new_req.vaddr)
    block_match && alias_match &&
      (
        before_req_sent_can_merge(new_req) ||
          before_data_refill_can_merge(new_req)
        )
  }

  def before_req_sent_merge_iskeyword(new_req: MissReqWoStoreData): Bool = {
    val need_check_isKeyword = acquire_not_sent && req.isFromLoad && new_req.isFromLoad && should_merge(new_req)
    val use_new_req_isKeyword = isAfter(req.lqIdx, new_req.lqIdx)
    Mux(
      need_check_isKeyword,
      Mux(
        use_new_req_isKeyword,
        new_req.vaddr(5).asBool,
        req.vaddr(5).asBool
      ),
      isKeyword
    )
  }

  // store can be merged before io.mem_acquire.fire
  // store can not be merged the cycle that io.mem_acquire.fire
  // load can be merged before io.mem_grant.fire
  //
  // TODO: merge store if possible? mem_acquire may need to be re-issued,
  // but sbuffer entry can be freed
  def should_reject(new_req: MissReqWoStoreData): Bool = {
    val block_match = get_block(req.addr) === get_block(new_req.addr)
    val set_match = set === addr_to_dcache_set(new_req.vaddr)
    val alias_match = is_alias_match(req.vaddr, new_req.vaddr)

    req_valid && Mux(
      block_match,
      (!before_req_sent_can_merge(new_req) && !before_data_refill_can_merge(new_req)) || !alias_match,
      false.B
    )
  }

  // req_valid will be updated 1 cycle after primary_fire, so next cycle, this entry cannot accept a new req
  when(GatedValidRegNext(io.id >= ((cfg.nMissEntries).U - io.nMaxPrefetchEntry))) {
    // can accept prefetch req
    io.primary_ready := !req_valid && !GatedValidRegNext(primary_fire)
  }.otherwise {
    // cannot accept prefetch req except when a memset patten is detected
    io.primary_ready := !req_valid && (!io.req.bits.isFromPrefetch) && !GatedValidRegNext(primary_fire)
  }
  io.secondary_ready := should_merge(io.req.bits) && !req.isCMO
  io.secondary_reject := should_reject(io.req.bits) && !req.isCMO

  // should not allocate, merge or reject at the same time
  assert(RegNext(PopCount(Seq(io.primary_ready, io.secondary_ready, io.secondary_reject)) <= 1.U || !io.req.valid))

  // when granted data is all ready, wakeup lq's miss load
  val refill_to_ldq_en = !w_grantlast && io.mem_grant.fire
  io.refill_to_ldq.valid := GatedValidRegNext(refill_to_ldq_en)
  io.refill_to_ldq.bits.addr := RegEnable(req.addr + ((refill_count ^ isKeyword) << refillOffBits), refill_to_ldq_en)
  //  io.refill_to_ldq.bits.data := refill_data_splited(RegEnable(refill_count ^ isKeyword, refill_to_ldq_en))
  io.refill_to_ldq.bits.data := DontCare
  io.refill_to_ldq.bits.error := RegEnable(io.mem_grant.bits.corrupt || io.mem_grant.bits.denied, refill_to_ldq_en)
  io.refill_to_ldq.bits.refill_done := RegEnable(refill_done && io.mem_grant.fire, refill_to_ldq_en)
  io.refill_to_ldq.bits.hasdata := hasData
  io.refill_to_ldq.bits.data_raw := refill_data_raw.asUInt
  io.refill_to_ldq.bits.id := io.id

  // if the entry has a pending merge req, wait for it
  // Note: now, only wait for store, because store may acquire T
  io.mem_acquire.valid := !s_acquire && !(io.miss_req_pipe_reg.merge && !io.miss_req_pipe_reg.cancel && miss_req_pipe_reg_bits.isFromStore)
  val grow_param = req.req_coh.onAccess(req.cmd)._2
  val acquireBlock = edge.AcquireBlock(
    fromSource = io.id,
    toAddress = req.addr,
    lgSize = (log2Up(cfg.blockBytes)).U,
    growPermissions = grow_param
  )._2
  val acquirePerm = edge.AcquirePerm(
    fromSource = io.id,
    toAddress = req.addr,
    lgSize = (log2Up(cfg.blockBytes)).U,
    growPermissions = grow_param
  )._2
  val acquireCMO = edge.CacheBlockOperation(
    fromSource = io.id, // source is the # of MissEntries + 1
    toAddress = get_block_addr(req.addr),
    lgSize = (log2Up(cfg.blockBytes)).U,
    opcode = req.cmoOpcode
  )._2
  io.mem_acquire.bits := Mux(req.isCMO, acquireCMO, Mux(req.full_overwrite, acquirePerm, acquireBlock))
  // resolve cache alias by L2
  io.mem_acquire.bits.user.lift(AliasKey).foreach( _ := req.vaddr(13, 12))
  // pass vaddr to l2
  io.mem_acquire.bits.user.lift(VaddrKey).foreach( _ := req.vaddr(VAddrBits-1, blockOffBits))
  // pass keyword to L2
  io.mem_acquire.bits.echo.lift(IsKeywordKey).foreach(_ := isKeyword)
  // trigger prefetch
  io.mem_acquire.bits.user.lift(PrefetchKey).foreach(_ := Mux(io.l2_pf_store_only, req.isFromStore, true.B))
  // req source
  when(prefetch && !secondary_fired) {
    io.mem_acquire.bits.user.lift(ReqSourceKey).foreach(_ := MemReqSource.L1DataPrefetch.id.U)
  }.otherwise {
    when(req.isFromStore) {
      io.mem_acquire.bits.user.lift(ReqSourceKey).foreach(_ := MemReqSource.CPUStoreData.id.U)
    }.elsewhen(req.isFromLoad) {
      io.mem_acquire.bits.user.lift(ReqSourceKey).foreach(_ := MemReqSource.CPULoadData.id.U)
    }.elsewhen(req.isFromAMO) {
      io.mem_acquire.bits.user.lift(ReqSourceKey).foreach(_ := MemReqSource.CPUAtomicData.id.U)
    }.otherwise {
      io.mem_acquire.bits.user.lift(ReqSourceKey).foreach(_ := MemReqSource.L1DataPrefetch.id.U)
    }
  }
  require(nSets <= 256)

  // io.mem_grant.ready := !w_grantlast && s_acquire
  io.mem_grant.ready := true.B
  assert(!(io.mem_grant.valid && !(!w_grantlast && s_acquire)), "dcache should always be ready for mem_grant now")

  val grantack = RegEnable(edge.GrantAck(io.mem_grant.bits), io.mem_grant.fire)
  assert(RegNext(!io.mem_grant.fire || edge.isRequest(io.mem_grant.bits)))
  io.mem_finish.valid := !s_grantack && w_grantlast
  io.mem_finish.bits := grantack

  // Send mainpipe_req when receive hint from L2 or receive data without hint
  io.main_pipe_req.valid := (!s_mainpipe_req && (w_l2hint || w_grantlast)) && !req.isCMO
  io.main_pipe_req.bits := DontCare
  io.main_pipe_req.bits.miss := true.B
  io.main_pipe_req.bits.miss_id := io.id
  io.main_pipe_req.bits.probe := false.B
  io.main_pipe_req.bits.source := req.source
  io.main_pipe_req.bits.cmd := req.cmd
  io.main_pipe_req.bits.vaddr := req.vaddr
  io.main_pipe_req.bits.addr := req.addr
  io.main_pipe_req.bits.word_idx := req.word_idx
  io.main_pipe_req.bits.amo_data := req.amo_data
  io.main_pipe_req.bits.amo_mask := req.amo_mask
  io.main_pipe_req.bits.lrsc_isD := req.lrsc_isD
  io.main_pipe_req.bits.id := req.id
  io.main_pipe_req.bits.pf_source := req.pf_source
  io.main_pipe_req.bits.access := access

  io.block_addr.valid := req_valid && w_grantlast
  io.block_addr.bits := req.addr

  io.req_addr.valid := req_valid
  io.req_addr.bits := req.addr

  io.refill_info.valid := req_valid && w_grantlast
  io.refill_info.bits.store_data := DontCare
  io.refill_info.bits.store_mask := ~0.U(blockBytes.W)
  io.refill_info.bits.miss_param := grant_param
  io.refill_info.bits.miss_dirty := isDirty
  io.refill_info.bits.error      := error

  io.sbuffer_id := req.id
  io.req_source := req.source

  XSPerfAccumulate("miss_refill_mainpipe_req", io.main_pipe_req.fire)
  XSPerfAccumulate("miss_refill_without_hint", io.main_pipe_req.fire && !mainpipe_req_fired && !w_l2hint)
  XSPerfAccumulate("miss_refill_replay", io.main_pipe_replay)

  val w_grantfirst_forward_info = Mux(isKeyword, w_grantlast, w_grantfirst)
  val w_grantlast_forward_info = Mux(isKeyword, w_grantfirst, w_grantlast)
  val refill_and_store_data = VecInit(Seq.fill(blockRows)(0.U(rowBits.W)))
  io.forwardInfo.apply(req_valid && (req.isFromLoad || req.isFromPrefetch), req.addr, refill_and_store_data, w_grantfirst_forward_info, w_grantlast_forward_info)

  io.matched := req_valid && (get_block(req.addr) === get_block(io.req.bits.addr)) && !prefetch
  io.prefetch_info.late_prefetch := io.req.valid && !(io.req.bits.isFromPrefetch) && req_valid && (get_block(req.addr) === get_block(io.req.bits.addr)) && prefetch

  when(io.prefetch_info.late_prefetch) {
    prefetch := false.B
  }

  // refill latency monitor
  val start_counting = GatedValidRegNext(io.mem_acquire.fire) || (GatedValidRegNextN(primary_fire, 2) && s_acquire)
  io.latency_monitor.load_miss_refilling  := req_valid && req_primary_fire.isFromLoad     && BoolStopWatch(start_counting, io.mem_grant.fire && !refill_done, true, true)
  io.latency_monitor.store_miss_refilling := req_valid && req_primary_fire.isFromStore    && BoolStopWatch(start_counting, io.mem_grant.fire && !refill_done, true, true)
  io.latency_monitor.amo_miss_refilling   := req_valid && req_primary_fire.isFromAMO      && BoolStopWatch(start_counting, io.mem_grant.fire && !refill_done, true, true)
  io.latency_monitor.pf_miss_refilling    := req_valid && req_primary_fire.isFromPrefetch && BoolStopWatch(start_counting, io.mem_grant.fire && !refill_done, true, true)

  XSPerfAccumulate("miss_req_primary", primary_fire)
  XSPerfAccumulate("miss_req_merged", secondary_fire)
  XSPerfAccumulate("load_miss_penalty_to_use",
    should_refill_data &&
      BoolStopWatch(primary_fire, io.refill_to_ldq.valid, true)
  )
  XSPerfAccumulate("penalty_between_grantlast_and_release",
    BoolStopWatch(!RegNext(w_grantlast) && w_grantlast, release_entry, true)
  )
  XSPerfAccumulate("main_pipe_penalty", BoolStopWatch(io.main_pipe_req.fire, io.main_pipe_resp))
  XSPerfAccumulate("penalty_blocked_by_channel_A", io.mem_acquire.valid && !io.mem_acquire.ready)
  XSPerfAccumulate("penalty_waiting_for_channel_D", s_acquire && !w_grantlast && !io.mem_grant.valid)
  XSPerfAccumulate("penalty_waiting_for_channel_E", io.mem_finish.valid && !io.mem_finish.ready)
  XSPerfAccumulate("prefetch_req_primary", primary_fire && io.req.bits.source === DCACHE_PREFETCH_SOURCE.U)
  XSPerfAccumulate("prefetch_req_merged", secondary_fire && io.req.bits.source === DCACHE_PREFETCH_SOURCE.U)
  XSPerfAccumulate("can_not_send_acquire_because_of_merging_store", !s_acquire && io.miss_req_pipe_reg.merge && io.miss_req_pipe_reg.cancel && miss_req_pipe_reg_bits.isFromStore)

  val (mshr_penalty_sample, mshr_penalty) = TransactionLatencyCounter(GatedValidRegNextN(primary_fire, 2), release_entry)
  XSPerfHistogram("miss_penalty", mshr_penalty, mshr_penalty_sample, 0, 20, 1, true, true)
  XSPerfHistogram("miss_penalty", mshr_penalty, mshr_penalty_sample, 20, 100, 10, true, false)

  val load_miss_begin = primary_fire && io.req.bits.isFromLoad
  val refill_finished = GatedValidRegNext(!w_grantlast && refill_done) && should_refill_data
  val (load_miss_penalty_sample, load_miss_penalty) = TransactionLatencyCounter(load_miss_begin, refill_finished) // not real refill finish time
  XSPerfHistogram("load_miss_penalty_to_use", load_miss_penalty, load_miss_penalty_sample, 0, 20, 1, true, true)
  XSPerfHistogram("load_miss_penalty_to_use", load_miss_penalty, load_miss_penalty_sample, 20, 100, 10, true, false)

  val (a_to_d_penalty_sample, a_to_d_penalty) = TransactionLatencyCounter(start_counting, GatedValidRegNext(io.mem_grant.fire && refill_done))
  XSPerfHistogram("a_to_d_penalty", a_to_d_penalty, a_to_d_penalty_sample, 0, 20, 1, true, true)
  XSPerfHistogram("a_to_d_penalty", a_to_d_penalty, a_to_d_penalty_sample, 20, 100, 10, true, false)
}
