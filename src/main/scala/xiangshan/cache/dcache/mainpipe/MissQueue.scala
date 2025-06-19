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


class MissReqWoStoreData(implicit p: Parameters) extends DCacheBundle {
  val source = UInt(sourceTypeWidth.W)
  val pf_source = UInt(L1PfSourceBits.W)
  val cmd = UInt(M_SZ.W)
  val addr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val pc = UInt(VAddrBits.W)

  val isCMO = Bool()
  val cmoOpcode = UInt(3.W)   // 0-cbo.clean, 1-cbo.flush, 2-cbo.inval, 3-cbo.zero

  val lqIdx = new LqPtr
  // store
  val full_overwrite = Bool()

  // which word does amo work on?
  val word_idx = UInt(log2Up(blockWords).W)
  val amo_data = UInt(DataBits.W)
  val amo_mask = UInt((DataBits / 8).W)

  val req_coh = new ClientMetadata
  val id = UInt(reqIdWidth.W)

  // For now, miss queue entry req is actually valid when req.valid && !cancel
  // * req.valid is fast to generate
  // * cancel is slow to generate, it will not be used until the last moment
  //
  // cancel may come from the following sources:
  // 1. miss req blocked by writeback queue:
  //      a writeback req of the same address is in progress
  // 2. pmp check failed
  val cancel = Bool() // cancel is slow to generate, it will cancel missreq.valid

  // Req source decode
  // Note that req source is NOT cmd type
  // For instance, a req which isFromPrefetch may have R or W cmd
  def isFromLoad = source === LOAD_SOURCE.U
  def isFromStore = source === STORE_SOURCE.U
  def isFromAMO = source === AMO_SOURCE.U
  def isFromCMO = source === CMO_SOURCE.U
  def isFromPrefetch = source >= DCACHE_PREFETCH_SOURCE.U
  def isPrefetchWrite = source === DCACHE_PREFETCH_SOURCE.U && cmd === MemoryOpConstants.M_PFW
  def isPrefetchRead = source === DCACHE_PREFETCH_SOURCE.U && cmd === MemoryOpConstants.M_PFR
  def hit = req_coh.isValid()
}

class MissReqStoreData(implicit p: Parameters) extends DCacheBundle {
  // store data and store mask will be written to miss queue entry
  // 1 cycle after req.fire() and meta write
  val store_data = UInt((cfg.blockBytes * 8).W)
  val store_mask = UInt(cfg.blockBytes.W)
}

class MissQueueRefillInfo(implicit p: Parameters) extends MissReqStoreData {
  // refill_info for mainpipe req awake
  val miss_param = UInt(TLPermissions.bdWidth.W)
  val miss_dirty = Bool()
  val error      = Bool()
}

class MissReq(implicit p: Parameters) extends MissReqWoStoreData {
  // store data and store mask will be written to miss queue entry
  // 1 cycle after req.fire() and meta write
  val store_data = UInt((cfg.blockBytes * 8).W)
  val store_mask = UInt(cfg.blockBytes.W)

  def toMissReqStoreData(): MissReqStoreData = {
    val out = Wire(new MissReqStoreData)
    out.store_data := store_data
    out.store_mask := store_mask
    out
  }

  def toMissReqWoStoreData(): MissReqWoStoreData = {
    val out = Wire(new MissReqWoStoreData)
    out.source := source
    out.pf_source := pf_source
    out.cmd := cmd
    out.addr := addr
    out.vaddr := vaddr
    out.full_overwrite := full_overwrite
    out.word_idx := word_idx
    out.amo_data := amo_data
    out.amo_mask := amo_mask
    out.req_coh := req_coh
    out.id := id
    out.cancel := cancel
    out.pc := pc
    out.lqIdx := lqIdx
    out.isCMO := isCMO
    out.cmoOpcode := cmoOpcode
    out
  }
}

class MissResp(implicit p: Parameters) extends DCacheBundle {
  val id = UInt(log2Up(cfg.nMissEntries).W)
  // cache miss request is handled by miss queue, either merged or newly allocated
  val handled = Bool()
  // cache req missed, merged into one of miss queue entries
  // i.e. !miss_merged means this access is the first miss for this cacheline
  val merged = Bool()
}


/**
  * miss queue enq logic: enq is now splited into 2 cycles
  *  +---------------------------------------------------------------------+    pipeline reg  +-------------------------+
  *  +         s0: enq source arbiter, judge mshr alloc or merge           +     +-------+    + s1: real alloc or merge +
  *  +                      +-----+          primary_fire?       ->        +     | alloc |    +                         +
  *  + mainpipe  -> req0 -> |     |          secondary_fire?     ->        +     | merge |    +                         +
  *  + loadpipe0 -> req1 -> | arb | -> req                       ->        +  -> | req   | -> +                         +
  *  + loadpipe1 -> req2 -> |     |          mshr id             ->        +     | id    |    +                         +
  *  +                      +-----+                                        +     +-------+    +                         +
  *  +---------------------------------------------------------------------+                  +-------------------------+
  */

// a pipeline reg between MissReq and MissEntry
class MissReqPipeRegBundle(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheBundle
 with HasCircularQueuePtrHelper
 {
  val req           = new MissReq
  // this request is about to merge to an existing mshr
  val merge         = Bool()
  // this request is about to allocate a new mshr
  val alloc         = Bool()
  val cancel        = Bool()
  val mshr_id       = UInt(log2Up(cfg.nMissEntries).W)

  def reg_valid(): Bool = {
    (merge || alloc)
  }

  def matched(new_req: MissReq): Bool = {
    val block_match = get_block(req.addr) === get_block(new_req.addr)
    block_match && reg_valid() && !(req.isFromPrefetch)
  }

  def prefetch_late_en(new_req: MissReqWoStoreData, new_req_valid: Bool): Bool = {
    val block_match = get_block(req.addr) === get_block(new_req.addr)
    new_req_valid && alloc && block_match && (req.isFromPrefetch) && !(new_req.isFromPrefetch)
  }

  def reject_req(new_req: MissReq): Bool = {
    val block_match = get_block(req.addr) === get_block(new_req.addr)
    val alias_match = is_alias_match(req.vaddr, new_req.vaddr)
    val merge_load = (req.isFromLoad || req.isFromStore || req.isFromPrefetch) && new_req.isFromLoad
    // store merge to a store is disabled, sbuffer should avoid this situation, as store to same address should preserver their program order to match memory model
    val merge_store = (req.isFromLoad || req.isFromPrefetch) && new_req.isFromStore

    val set_match = addr_to_dcache_set(req.vaddr) === addr_to_dcache_set(new_req.vaddr)

    Mux(
        alloc,
        block_match && (!alias_match || !(merge_load || merge_store)),
        false.B
      )
  }

  def merge_req(new_req: MissReq): Bool = {
    val block_match = get_block(req.addr) === get_block(new_req.addr)
    val alias_match = is_alias_match(req.vaddr, new_req.vaddr)
    val merge_load = (req.isFromLoad || req.isFromStore || req.isFromPrefetch) && new_req.isFromLoad
    // store merge to a store is disabled, sbuffer should avoid this situation, as store to same address should preserver their program order to match memory model
    val merge_store = (req.isFromLoad || req.isFromPrefetch) && new_req.isFromStore
    Mux(
        alloc,
        block_match && alias_match && (merge_load || merge_store),
        false.B
      ) && !req.isCMO
  }
  
  def merge_isKeyword(new_req: MissReq): Bool = {
    val load_merge_load  = merge_req(new_req) && req.isFromLoad  && new_req.isFromLoad
    val store_merge_load = merge_req(new_req) && req.isFromStore && new_req.isFromLoad
    val load_merge_load_use_new_req_isKeyword = isAfter(req.lqIdx, new_req.lqIdx)
    val use_new_req_isKeyword = (load_merge_load && load_merge_load_use_new_req_isKeyword) || store_merge_load
    Mux (
      use_new_req_isKeyword,
        new_req.vaddr(5).asBool,
        req.vaddr(5).asBool
      )
  }

  def isKeyword(): Bool= {
    val alloc_isKeyword = Mux(
                           alloc,
                           Mux(
                            req.isFromLoad,
                            req.vaddr(5).asBool,
                            false.B),
                            false.B)
    Mux(
      merge_req(req),
      merge_isKeyword(req),
      alloc_isKeyword
    )
  }
  // send out acquire as soon as possible
  // if a new store miss req is about to merge into this pipe reg, don't send acquire now
  def can_send_acquire(valid: Bool, new_req: MissReq): Bool = {
    alloc && !(valid && merge_req(new_req) && new_req.isFromStore)
  }

  def get_acquire(l2_pf_store_only: Bool): TLBundleA = {
    val acquire = Wire(new TLBundleA(edge.bundle))
    val grow_param = req.req_coh.onAccess(req.cmd)._2
    val acquireBlock = edge.AcquireBlock(
      fromSource = mshr_id,
      toAddress = get_block_addr(req.addr),
      lgSize = (log2Up(cfg.blockBytes)).U,
      growPermissions = grow_param
    )._2
    val acquirePerm = edge.AcquirePerm(
      fromSource = mshr_id,
      toAddress = get_block_addr(req.addr),
      lgSize = (log2Up(cfg.blockBytes)).U,
      growPermissions = grow_param
    )._2
    val acquireCMO = edge.CacheBlockOperation(
      fromSource = mshr_id, // source is the # of MissEntries + 1
      toAddress = get_block_addr(req.addr),
      lgSize = (log2Up(cfg.blockBytes)).U,
      opcode = req.cmoOpcode
    )._2
    acquire := Mux(req.isCMO, acquireCMO, Mux(req.full_overwrite, acquirePerm, acquireBlock))
    // resolve cache alias by L2
    acquire.user.lift(AliasKey).foreach(_ := req.vaddr(13, 12))
    // pass vaddr to l2
    acquire.user.lift(VaddrKey).foreach(_ := req.vaddr(VAddrBits - 1, blockOffBits))

    // miss req pipe reg pass keyword to L2, is priority
    acquire.echo.lift(IsKeywordKey).foreach(_ := isKeyword())

    // trigger prefetch
    acquire.user.lift(PrefetchKey).foreach(_ := Mux(l2_pf_store_only, req.isFromStore, true.B))

    // for nhl2 user field
    acquire.user.lift(TLNanhuBusKey).foreach { field =>
      field.alias.foreach(_ := req.vaddr(13, 12))
      field.vaddr.foreach(_ := req.vaddr(VAddrBits - 1, blockOffBits))
      field.pfHint := Mux(l2_pf_store_only, req.isFromStore, true.B)
    }

    // req source
    when(req.isFromLoad) {
      acquire.user.lift(ReqSourceKey).foreach(_ := MemReqSource.CPULoadData.id.U)
    }.elsewhen(req.isFromStore) {
      acquire.user.lift(ReqSourceKey).foreach(_ := MemReqSource.CPUStoreData.id.U)
    }.elsewhen(req.isFromAMO) {
      acquire.user.lift(ReqSourceKey).foreach(_ := MemReqSource.CPUAtomicData.id.U)
    }.otherwise {
      acquire.user.lift(ReqSourceKey).foreach(_ := MemReqSource.L1DataPrefetch.id.U)
    }

    acquire
  }

  def block_match(release_addr: UInt): Bool = {
    reg_valid() && get_block(req.addr) === get_block(release_addr)
  }

   def block_and_alias_match(releaseReq: MissQueueBlockReqBundle): Bool = {
     reg_valid() && get_block(req.addr) === get_block(releaseReq.addr) && is_alias_match(req.vaddr, releaseReq.vaddr)
   }
}

// class CMOUnit(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule
// {
//   val io = IO(new Bundle(){
//     val cmo_req  = Flipped(Decoupled(new CMOReq))
//     val cmo_resp = Decoupled(new CMOResp)
//     val req_TLA = DecoupledIO(new TLBundleA(edge.bundle))
//     val resp_TLD = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
//   })

//   val s_idle :: s_req :: s_resp :: s_wb :: Nil = Enum(4)
//   val cmoState = RegInit(s_idle)

//   val req = RegEnable(io.cmo_req.bits, io.cmo_req.fire)
//   val nderr = RegInit(false.B)

//   switch(cmoState) {
//     is(s_idle) {
//       when(io.cmo_req.fire) {
//         cmoState := s_req
//         nderr := false.B
//       }
//     }

//     is(s_req) {
//       when(io.req_TLA.fire) {
//         cmoState := s_resp
//       }
//     }

//     is(s_resp) {
//       when(io.resp_TLD.fire) {
//         cmoState := s_wb
//         nderr := io.resp_TLD.bits.denied || io.resp_TLD.bits.corrupt
//       }
//     }

//     is(s_wb) {
//       when(io.cmo_resp.fire) {
//         cmoState := s_idle
//       }
//     }
//   }

//   io.cmo_req.ready := (cmoState === s_idle)

//   io.req_TLA.valid := (cmoState === s_req)
//   io.req_TLA.bits := edge.CacheBlockOperation(
//     fromSource = (cfg.nMissEntries + 1).U, // source is the # of MissEntries + 1
//     toAddress = req.address,
//     lgSize = (log2Up(cfg.blockBytes)).U,
//     opcode = req.opcode
//   )._2

//   io.resp_TLD.ready := (cmoState === s_resp)
  
//   io.cmo_resp.valid := (cmoState === s_wb)
//   io.cmo_resp.bits.address := req.address
//   io.cmo_resp.bits.nderr   := nderr

//   assert(!(cmoState =/= s_idle && io.cmo_req.valid), "CBO can not continuously execute!")
//   assert(!(cmoState =/= s_resp && io.resp_TLD.valid), "when cmo is executing, TLD can not resp other")
// }

class MissQueueBlockReqBundle(implicit p: Parameters) extends XSBundle {
  val addr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
}


class MissQueueBlockIO(implicit p: Parameters) extends XSBundle {
  val req = ValidIO(new MissQueueBlockReqBundle)
  val block = Input(Bool())
}

class MissQueue(edge: TLEdgeOut, reqNum: Int)(implicit p: Parameters) extends DCacheModule 
  with HasPerfEvents 
  {
  val io = IO(new Bundle {
    val hartId = Input(UInt(hartIdLen.W))
    val req = Flipped(DecoupledIO(new MissReq))
    val resp = Output(new MissResp)
    val refill_to_ldq = ValidIO(new Refill)

//    val queryMQ = Vec(reqNum, Flipped(new DCacheMQQueryIOBundle))

    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val mem_finish = DecoupledIO(new TLBundleE(edge.bundle))

    val refill_to_sbuffer = ValidIO(new RefillToSbuffer)

    val l2_hint = Input(Valid(new L2ToL1Hint())) // Hint from L2 Cache

    val main_pipe_req = DecoupledIO(new MainPipeReq)
    val main_pipe_resp = Flipped(ValidIO(new MainPipeResp))

    val mainpipe_info = Input(new MainPipeInfoToMQ)
    val refill_info = ValidIO(new MissQueueRefillInfo)

    // block probe
    val probe_addr = Input(UInt(PAddrBits.W))
    val probe_block = Output(Bool())

    // block replace when release an addr valid in mshr
//    val replace_addr = Flipped(ValidIO(UInt(PAddrBits.W)))
    val replace_block_query = Flipped(new MissQueueBlockIO)

    // req blocked by wbq
    val wbq_block_miss_req = Input(Bool())

    val full = Output(Bool())

    // forward missqueue
    val forward = Vec(LoadPipelineWidth, new LduToMissqueueForwardIO)
    val l2_pf_store_only = Input(Bool())

    // val cmoOpReq = Flipped(DecoupledIO(new CMOReq))
    // val cmoOpResp = DecoupledIO(new CMOResp)

    val memSetPattenDetected = Output(Bool())
    val lqEmpty = Input(Bool())

    val prefetch_info = new Bundle {
      val naive = new Bundle {
        val late_miss_prefetch = Output(Bool())
      }

      val fdp = new Bundle {
        val late_miss_prefetch = Output(Bool())
        val prefetch_monitor_cnt = Output(Bool())
        val total_prefetch = Output(Bool())
      }
    }
    val cmofinish = Output(Bool())
    val mq_enq_cancel = Output(Bool())

    val debugTopDown = new DCacheTopDownIO
  })
  // 128KBL1: FIXME: provide vaddr for l2

  val entries = Seq.fill(cfg.nMissEntries)(Module(new MissEntry(edge, reqNum)))
  val dataBuffer = Module(new DataBuffer(MissqDataBufferDepth))
  val difftest_data_raw = Reg(Vec(blockBytes/beatBytes, UInt(beatBits.W)))
  // val cmoUnit = Module(new CMOUnit(edge))

  val miss_req_pipe_reg_valid = RegInit(false.B)
  val miss_req_pipe_reg = RegInit(0.U.asTypeOf(new MissReqPipeRegBundle(edge)))
  val acquire_from_pipereg = Wire(chiselTypeOf(io.mem_acquire))

  val primary_ready_vec = entries.map(_.io.primary_ready)
  val secondary_ready_vec = entries.map(_.io.secondary_ready)
  val secondary_reject_vec = entries.map(_.io.secondary_reject)
  val probe_block_vec = entries.map { case e => e.io.block_addr.valid && e.io.block_addr.bits === io.probe_addr }

  // cmo instr will not be merged or rejected
  val isCMOReq = io.req.bits.isCMO
  val merge = !isCMOReq && ParallelORR(Cat(secondary_ready_vec ++ Seq(miss_req_pipe_reg.merge_req(io.req.bits))))
  val reject = !isCMOReq && ParallelORR(Cat(secondary_reject_vec ++ Seq(miss_req_pipe_reg.reject_req(io.req.bits))))
  val alloc = (isCMOReq || (!reject && !merge)) && ParallelORR(Cat(primary_ready_vec))
  val accept = alloc || merge
  assert(!(io.req.valid && isCMOReq && merge), "CMO should never be merged into an existing MSHR")
//   generate req_ready for each miss request for better timing
//  for (i <- 0 until reqNum) {
//    val _primary_ready_vec = entries.map(_.io.queryME(i).primary_ready)
//    val _secondary_ready_vec = entries.map(_.io.queryME(i).secondary_ready)
//    val _secondary_reject_vec = entries.map(_.io.queryME(i).secondary_reject)
//    val _merge = ParallelORR(Cat(_secondary_ready_vec ++ Seq(miss_req_pipe_reg.merge_req(io.queryMQ(i).req.bits))))
//    val _reject = ParallelORR(Cat(_secondary_reject_vec ++ Seq(miss_req_pipe_reg.reject_req(io.queryMQ(i).req.bits))))
//    val _alloc = !_reject && !_merge && ParallelORR(Cat(_primary_ready_vec))
//    val _accept = _alloc || _merge
//
//    io.queryMQ(i).ready := _accept
//  }

  val req_mshr_handled_vec = entries.map(_.io.req_handled_by_this_entry)
  // merged to pipeline reg
  val req_pipeline_reg_handled = miss_req_pipe_reg.merge_req(io.req.bits) && io.req.valid
  assert(PopCount(Seq(req_pipeline_reg_handled, VecInit(req_mshr_handled_vec).asUInt.orR)) <= 1.U, "miss req will either go to mshr or pipeline reg")
  assert(PopCount(req_mshr_handled_vec) <= 1.U, "Only one mshr can handle a req")
  io.resp.id := Mux(!req_pipeline_reg_handled, OHToUInt(req_mshr_handled_vec), miss_req_pipe_reg.mshr_id)
  io.resp.handled := Cat(req_mshr_handled_vec).orR || req_pipeline_reg_handled
  io.resp.merged := merge

  /*  MissQueue enq logic is now splitted into 2 cycles
   *
   */
  when(io.req.valid){
    miss_req_pipe_reg_valid   := true.B
    miss_req_pipe_reg.req     := io.req.bits
  }
  // miss_req_pipe_reg.req     := io.req.bits
  miss_req_pipe_reg.alloc   := alloc && io.req.valid && !io.req.bits.cancel && !io.wbq_block_miss_req
  miss_req_pipe_reg.merge   := merge && io.req.valid && !io.req.bits.cancel && !io.wbq_block_miss_req
  miss_req_pipe_reg.cancel  := io.wbq_block_miss_req && !isCMOReq
  miss_req_pipe_reg.mshr_id := io.resp.id

  assert(PopCount(Seq(alloc && io.req.valid, merge && io.req.valid)) <= 1.U, "allocate and merge a mshr in same cycle!")

  val source_except_load_cnt = RegInit(0.U(10.W))
  when(VecInit(req_mshr_handled_vec).asUInt.orR || req_pipeline_reg_handled) {
    when(io.req.bits.isFromLoad) {
      source_except_load_cnt := 0.U
    }.otherwise {
      when(io.req.bits.isFromStore) {
        source_except_load_cnt := source_except_load_cnt + 1.U
      }
    }
  }
  val Threshold = 8
  val memSetPattenDetected = GatedValidRegNext((source_except_load_cnt >= Threshold.U) && io.lqEmpty)

  io.memSetPattenDetected := memSetPattenDetected

  (0 until LoadPipelineWidth).map(i => {
    dataBuffer.io.forward(i).valid := false.B
    dataBuffer.io.forward(i).bits := DontCare
  })

  //only load or prefetch mshr can be forwarded
  val forwardInfo_vec = VecInit(entries.map(_.io.forwardInfo))
  (0 until LoadPipelineWidth).map(i => {
    val id = io.forward(i).mshrid
    val req_valid = io.forward(i).valid
    val paddr = io.forward(i).paddr
    dataBuffer.io.forward(i).valid := req_valid
    dataBuffer.io.forward(i).bits.rid := id
    val forwardRawData = dataBuffer.io.forwardData(i)

    val (forward_mshr, forwardData) = forwardInfo_vec(id).forward(req_valid, paddr, forwardRawData)
    io.forward(i).forward_result_valid := forwardInfo_vec(id).check(req_valid, paddr)
    io.forward(i).forward_mshr := forward_mshr
    io.forward(i).forwardData := forwardData
  })

  assert(RegNext(PopCount(secondary_ready_vec) <= 1.U || !io.req.valid))
//  assert(RegNext(PopCount(secondary_reject_vec) <= 1.U))
  // It is possible that one mshr wants to merge a req, while another mshr wants to reject it.
  // That is, a coming req has the same paddr as that of mshr_0 (merge),
  // while it has the same set and the same way as mshr_1 (reject).
  // In this situation, the coming req should be merged by mshr_0
//  assert(RegNext(PopCount(Seq(merge, reject)) <= 1.U))

  def select_valid_one[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    name: Option[String] = None): Unit = {

    if (name.nonEmpty) { out.suggestName(s"${name.get}_select") }
    out.valid := Cat(in.map(_.valid)).orR
    out.bits := ParallelMux(in.map(_.valid) zip in.map(_.bits))
    in.map(_.ready := out.ready)
    assert(!RegNext(out.valid && PopCount(Cat(in.map(_.valid))) > 1.U))
  }

  io.mem_grant.ready := true.B
  io.refill_to_sbuffer.valid := false.B
  io.refill_to_sbuffer.bits := DontCare

  val hasData = edge.hasData(io.mem_grant.bits)
  val refill_row_data = io.mem_grant.bits.data
  val isKeyword = io.mem_grant.bits.echo.lift(IsKeywordKey).getOrElse(false.B)
  val (_, _, refill_done, refill_count) = edge.count(io.mem_grant)
  dataBuffer.io.read.valid := false.B
  dataBuffer.io.read.bits := DontCare
  dataBuffer.io.write.valid := false.B
  dataBuffer.io.write.bits := DontCare

  val diff_refill = Wire(Bool())
  diff_refill := merge && io.req.valid && !io.req.bits.cancel && io.req.bits.isFromLoad

  // cmoUnit.io.cmo_req <> io.cmoOpReq
  // cmoUnit.io.cmo_resp <> io.cmoOpResp
  val pipeHasCmo = miss_req_pipe_reg.req.isCMO && miss_req_pipe_reg_valid
  val mshrHasCmo = entries.map { e => e.io.isCMO }.reduce(_ || _)
  io.cmofinish := !pipeHasCmo && !mshrHasCmo

  val nMaxPrefetchEntry = Constantin.createRecord(s"nMaxPrefetchEntry${p(XSCoreParamsKey).HartId}", initValue = (cfg.nMissEntries-cfg.nMissEntries/8))
  entries.zipWithIndex.foreach {
    case (e, i) =>
      val former_primary_ready = if(i == 0)
        false.B
      else
        Cat((0 until i).map(j => entries(j).io.primary_ready)).orR

      e.io.hartId := io.hartId
      e.io.id := i.U
      e.io.l2_pf_store_only := io.l2_pf_store_only
      e.io.req.valid := io.req.valid
      e.io.wbq_block_miss_req := io.wbq_block_miss_req
      e.io.primary_valid := io.req.valid &&
        !merge &&
        !reject &&
        !former_primary_ready &&
        e.io.primary_ready
      e.io.req.bits := io.req.bits.toMissReqWoStoreData()

      e.io.mem_grant.valid := false.B
      e.io.mem_grant.bits := DontCare
      e.io.replace.req <> io.replace_block_query.req

      when(io.mem_grant.valid && io.mem_grant.bits.source === i.U) {
        when(e.io.req_source === STORE_SOURCE.U || e.io.req_source === AMO_SOURCE.U) {
          io.mem_grant.ready := e.io.mem_grant.ready
          when(io.mem_grant.fire && hasData) {
            io.refill_to_sbuffer.valid := true.B
            io.refill_to_sbuffer.bits.data := refill_row_data
            io.refill_to_sbuffer.bits.id := e.io.sbuffer_id
            io.refill_to_sbuffer.bits.isKeyword := isKeyword
            io.refill_to_sbuffer.bits.refill_count := refill_count

            difftest_data_raw(refill_count ^ isKeyword) := refill_row_data
          }
        }.elsewhen(e.io.req_source === LOAD_SOURCE.U || e.io.req_source === DCACHE_PREFETCH_SOURCE.U) {
          io.mem_grant.ready := !dataBuffer.io.full && e.io.mem_grant.ready
          when(io.mem_grant.fire && hasData) {
            dataBuffer.io.write.valid := true.B
            dataBuffer.io.write.bits.wid := io.mem_grant.bits.source
            dataBuffer.io.write.bits.wdata := refill_row_data
            dataBuffer.io.write.bits.wbeat := refill_count ^ isKeyword
            dataBuffer.io.write.bits.refill_count := refill_count

            difftest_data_raw(refill_count ^ isKeyword) := refill_row_data
          }
        }
        e.io.mem_grant.bits := io.mem_grant.bits
        e.io.mem_grant.valid := Mux(io.mem_grant.ready, io.mem_grant.valid, false.B)
      }

      when(miss_req_pipe_reg.reg_valid() && miss_req_pipe_reg.mshr_id === i.U) {
        e.io.miss_req_pipe_reg := miss_req_pipe_reg
        miss_req_pipe_reg_valid := false.B
      }.otherwise {
        e.io.miss_req_pipe_reg       := DontCare
        e.io.miss_req_pipe_reg.merge := false.B
        e.io.miss_req_pipe_reg.alloc := false.B
      }

      e.io.acquire_fired_by_pipe_reg := acquire_from_pipereg.fire

      e.io.main_pipe_resp := io.main_pipe_resp.valid && io.main_pipe_resp.bits.ack_miss_queue && io.main_pipe_resp.bits.miss_id === i.U
      e.io.main_pipe_replay := io.mainpipe_info.s2_valid && io.mainpipe_info.s2_replay_to_mq && io.mainpipe_info.s2_miss_id === i.U
      e.io.main_pipe_refill_resp := io.mainpipe_info.s3_valid && io.mainpipe_info.s3_refill_resp && io.mainpipe_info.s3_miss_id === i.U

      e.io.memSetPattenDetected := memSetPattenDetected
      e.io.nMaxPrefetchEntry := nMaxPrefetchEntry

      e.io.main_pipe_req.ready := io.main_pipe_req.ready

//      for (j <- 0 until reqNum) {
//        e.io.queryME(j).req.valid := io.queryMQ(j).req.valid
//        e.io.queryME(j).req.bits  := io.queryMQ(j).req.bits.toMissReqWoStoreData()
//      }

      when(io.l2_hint.bits.sourceId === i.U) {
        e.io.l2_hint <> io.l2_hint
      } .otherwise {
        e.io.l2_hint.valid := false.B
        e.io.l2_hint.bits := DontCare
      }
  }

  io.req.ready := accept
  io.mq_enq_cancel := io.req.bits.cancel
  io.refill_to_ldq.valid := Cat(entries.map(_.io.refill_to_ldq.valid)).orR
  io.refill_to_ldq.bits := ParallelMux(entries.map(_.io.refill_to_ldq.valid) zip entries.map(_.io.refill_to_ldq.bits))

  io.refill_info.valid := VecInit(entries.zipWithIndex.map{ case(e,i) => e.io.refill_info.valid && io.mainpipe_info.s2_valid && io.mainpipe_info.s2_miss_id === i.U}).asUInt.orR
  io.refill_info.bits := Mux1H(entries.zipWithIndex.map{ case(e,i) => (io.mainpipe_info.s2_miss_id === i.U) -> e.io.refill_info.bits })

  dataBuffer.io.read.valid := io.refill_info.valid
  dataBuffer.io.read.bits.rid := io.mainpipe_info.s2_miss_id
  io.refill_info.bits.store_data := dataBuffer.io.rdata

  dataBuffer.io.free.valid :=  io.mainpipe_info.s3_valid && io.mainpipe_info.s3_refill_resp
  dataBuffer.io.free.bits := io.mainpipe_info.s3_miss_id

  acquire_from_pipereg.valid := miss_req_pipe_reg.can_send_acquire(io.req.valid, io.req.bits)
  acquire_from_pipereg.bits := miss_req_pipe_reg.get_acquire(io.l2_pf_store_only)

  XSPerfAccumulate("acquire_fire_from_pipereg", acquire_from_pipereg.fire)
  XSPerfAccumulate("pipereg_valid", miss_req_pipe_reg.reg_valid())

  val acquire_sources = Seq(acquire_from_pipereg) ++ entries.map(_.io.mem_acquire)
  TLArbiter.lowest(edge, io.mem_acquire, acquire_sources:_*)
  TLArbiter.lowest(edge, io.mem_finish, entries.map(_.io.mem_finish):_*)

  // // cmo resp
  // when (io.mem_grant.valid && io.mem_grant.bits.opcode === TLMessages.CBOAck) {
  //   cmoUnit.io.resp_TLD <> io.mem_grant
  // } .otherwise {
  //   cmoUnit.io.resp_TLD.valid := false.B
  //   cmoUnit.io.resp_TLD.bits  := DontCare
  // }

  // amo's main pipe req out
  fastArbiter(entries.map(_.io.main_pipe_req), io.main_pipe_req, Some("main_pipe_req"))

  io.probe_block := Cat(probe_block_vec).orR

  io.replace_block_query.block := io.replace_block_query.req.valid &&
    Cat(entries.map(e => e.io.replace.block) :+ miss_req_pipe_reg.block_and_alias_match(io.replace_block_query.req.bits)).orR

  io.full := ~Cat(entries.map(_.io.primary_ready)).andR

  // prefetch related
  io.prefetch_info.naive.late_miss_prefetch := io.req.valid && io.req.bits.isPrefetchRead && (miss_req_pipe_reg.matched(io.req.bits) || Cat(entries.map(_.io.matched)).orR)

  io.prefetch_info.fdp.late_miss_prefetch := (miss_req_pipe_reg.prefetch_late_en(io.req.bits.toMissReqWoStoreData(), io.req.valid) || Cat(entries.map(_.io.prefetch_info.late_prefetch)).orR)
  io.prefetch_info.fdp.prefetch_monitor_cnt := io.main_pipe_req.fire
  io.prefetch_info.fdp.total_prefetch := alloc && io.req.valid && !io.req.bits.cancel && isFromL1Prefetch(io.req.bits.pf_source)

  // L1MissTrace Chisel DB
  val debug_miss_trace = Wire(new L1MissTrace)
  debug_miss_trace.vaddr := io.req.bits.vaddr
  debug_miss_trace.paddr := io.req.bits.addr
  debug_miss_trace.source := io.req.bits.source
  debug_miss_trace.pc := io.req.bits.pc

  val isWriteL1MissQMissTable = Constantin.createRecord(s"isWriteL1MissQMissTable${p(XSCoreParamsKey).HartId}")
  val table = ChiselDB.createTable(s"L1MissQMissTrace_hart${p(XSCoreParamsKey).HartId}", new L1MissTrace)
  table.log(debug_miss_trace, isWriteL1MissQMissTable.orR && io.req.valid && !io.req.bits.cancel && alloc, "MissQueue", clock, reset)

  // Difftest
  if (env.EnableDifftest) {
    val difftest = DifftestModule(new DiffRefillEvent, dontCare = true)
    difftest.coreid := io.hartId
    difftest.index := 1.U
    difftest.valid := io.refill_to_ldq.valid && io.refill_to_ldq.bits.hasdata && io.refill_to_ldq.bits.refill_done && (io.refill_to_ldq.bits.addr <= 0x400000000L.U) // temp: for pldm 16G ddr
    difftest.addr := io.refill_to_ldq.bits.addr
    difftest.data := difftest_data_raw.asTypeOf(difftest.data)
    difftest.idtfr := DontCare

    // commit cbo.inval to difftest
    val cmoInvalEvent = DifftestModule(new DiffCMOInvalEvent)
    cmoInvalEvent.coreid := io.hartId
    cmoInvalEvent.valid  := io.mem_acquire.fire && io.mem_acquire.bits.opcode === TLMessages.CBOInval
    cmoInvalEvent.addr   := io.mem_acquire.bits.address

  }

  // Perf count
  XSPerfAccumulate("miss_req", io.req.fire && !io.req.bits.cancel)
  XSPerfAccumulate("miss_req_allocate", io.req.fire && !io.req.bits.cancel && alloc)
  XSPerfAccumulate("miss_req_load_allocate", io.req.fire && !io.req.bits.cancel && alloc && io.req.bits.isFromLoad)
  XSPerfAccumulate("miss_req_store_allocate", io.req.fire && !io.req.bits.cancel && alloc && io.req.bits.isFromStore)
  XSPerfAccumulate("miss_req_amo_allocate", io.req.fire && !io.req.bits.cancel && alloc && io.req.bits.isFromAMO)
  XSPerfAccumulate("miss_req_prefetch_allocate", io.req.fire && !io.req.bits.cancel && alloc && io.req.bits.isFromPrefetch)
  XSPerfAccumulate("miss_req_merge_load", io.req.fire && !io.req.bits.cancel && merge && io.req.bits.isFromLoad)
  XSPerfAccumulate("miss_req_reject_load", io.req.valid && !io.req.bits.cancel && reject && io.req.bits.isFromLoad)
  XSPerfAccumulate("probe_blocked_by_miss", io.probe_block)
  XSPerfAccumulate("prefetch_primary_fire", io.req.fire && !io.req.bits.cancel && alloc && io.req.bits.isFromPrefetch)
  XSPerfAccumulate("prefetch_secondary_fire", io.req.fire && !io.req.bits.cancel && merge && io.req.bits.isFromPrefetch)
  XSPerfAccumulate("memSetPattenDetected", memSetPattenDetected)
  val max_inflight = RegInit(0.U((log2Up(cfg.nMissEntries) + 1).W))
  val num_valids = PopCount(~Cat(primary_ready_vec).asUInt)
  when (num_valids > max_inflight) {
    max_inflight := num_valids
  }
  // max inflight (average) = max_inflight_total / cycle cnt
  XSPerfAccumulate("max_inflight", max_inflight)
  QueuePerf(cfg.nMissEntries, num_valids, num_valids === cfg.nMissEntries.U)
  io.full := num_valids === cfg.nMissEntries.U
  XSPerfHistogram("num_valids", num_valids, true.B, 0, cfg.nMissEntries, 1)

  XSPerfHistogram("L1DMLP_CPUData", PopCount(VecInit(entries.map(_.io.perf_pending_normal)).asUInt), true.B, 0, cfg.nMissEntries, 1)
  XSPerfHistogram("L1DMLP_Prefetch", PopCount(VecInit(entries.map(_.io.perf_pending_prefetch)).asUInt), true.B, 0, cfg.nMissEntries, 1)
  XSPerfHistogram("L1DMLP_Total", num_valids, true.B, 0, cfg.nMissEntries, 1)

  XSPerfAccumulate("miss_load_refill_latency", PopCount(entries.map(_.io.latency_monitor.load_miss_refilling)))
  XSPerfAccumulate("miss_store_refill_latency", PopCount(entries.map(_.io.latency_monitor.store_miss_refilling)))
  XSPerfAccumulate("miss_amo_refill_latency", PopCount(entries.map(_.io.latency_monitor.amo_miss_refilling)))
  XSPerfAccumulate("miss_pf_refill_latency", PopCount(entries.map(_.io.latency_monitor.pf_miss_refilling)))

  val rob_head_miss_in_dcache = VecInit(entries.map(_.io.rob_head_query.resp)).asUInt.orR

  entries.foreach {
    case e => {
      e.io.rob_head_query.query_valid := io.debugTopDown.robHeadVaddr.valid
      e.io.rob_head_query.vaddr := io.debugTopDown.robHeadVaddr.bits
    }
  }

  io.debugTopDown.robHeadMissInDCache := rob_head_miss_in_dcache

  val perfValidCount = RegNext(PopCount(entries.map(entry => (!entry.io.primary_ready))))
  val perfEvents = Seq(
    ("dcache_missq_req      ", io.req.fire),
    ("dcache_missq_1_4_valid", (perfValidCount < (cfg.nMissEntries.U/4.U))),
    ("dcache_missq_2_4_valid", (perfValidCount > (cfg.nMissEntries.U/4.U)) & (perfValidCount <= (cfg.nMissEntries.U/2.U))),
    ("dcache_missq_3_4_valid", (perfValidCount > (cfg.nMissEntries.U/2.U)) & (perfValidCount <= (cfg.nMissEntries.U*3.U/4.U))),
    ("dcache_missq_4_4_valid", (perfValidCount > (cfg.nMissEntries.U*3.U/4.U))),
  )
  generatePerfEvent()
}


class dataBufferReadReq(implicit p: Parameters) extends DCacheBundle {
  val rid = Output(UInt(log2Up(cfg.nMissEntries).W)) //onhot
}

class dataBufferWriteReq(implicit p: Parameters) extends DCacheBundle {
  val wdata = UInt(l1BusDataWidth.W)
  val wid = UInt(log2Up(cfg.nMissEntries).W)
  val wbeat = UInt((blockBytes/beatBytes).W)
  val refill_count = Bool()
}

class DataBuffer(numEntries: Int)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val read = Flipped(ValidIO(new dataBufferReadReq))
    val rdata = Output(UInt(CacheLineSize.W))
    val free = Flipped(ValidIO(UInt(log2Up(cfg.nMissEntries).W)))
    val write = Flipped(ValidIO(new dataBufferWriteReq))
    val forward = Vec(LoadPipelineWidth, Flipped(ValidIO(new dataBufferReadReq)))
    val forwardData = Output(Vec(LoadPipelineWidth, Vec(blockRows, UInt(rowBits.W))))
    val full = Bool()
  })

  def getFirstOneOH(input: UInt): UInt = {
    assert(input.getWidth > 1)
    val output = WireInit(VecInit(input.asBools))
    (1 until input.getWidth).map(i => {
      output(i) := !input(i - 1, 0).orR && input(i)
    })
    output.asUInt
  }

  val data = Reg(Vec(numEntries, Vec(CacheLineSize/l1BusDataWidth, UInt(l1BusDataWidth.W))))
  val valid = RegInit(VecInit(List.fill(numEntries)(false.B)))
  val id = RegInit(VecInit(List.fill(numEntries)(0.U(log2Up(cfg.nMissEntries).W))))
  val waddr = OHToUInt(getFirstOneOH(VecInit(valid.map(!_)).asUInt))
  val raddrOh = WireInit(VecInit(List.fill(numEntries)(false.B)))
  val raddr = WireInit(0.U(log2Up(numEntries).W))
  val freeOh = WireInit(VecInit(List.fill(numEntries)(false.B)))
  val freeAddr = WireInit(0.U(log2Up(numEntries).W))
  val forwardOh =VecInit(Seq.fill(LoadPipelineWidth)(VecInit(Seq.fill(numEntries)(false.B))))
  val forwardAddr = VecInit(Seq.fill(LoadPipelineWidth)(0.U(log2Up(numEntries).W)))
  val infilghtAddr = RegInit(0.U(log2Up(numEntries).W))
  val infilght = RegInit(false.B)
  val infilghtId = Reg(UInt(log2Up(cfg.nMissEntries).W))
  dontTouch(raddrOh)

  id.zipWithIndex.foreach({ case(id, i) =>
    raddrOh(i) := Mux(id === io.read.bits.rid, 1.B, 0.B) && valid(i)
    freeOh(i) := Mux(id === io.free.bits, 1.B, 0.B) && valid(i)
    (0 until LoadPipelineWidth).map{ j =>
      forwardOh(j)(i) := Mux(infilght && infilghtId === io.forward(j).bits.rid && infilghtAddr === i.U, true.B,
      Mux(id === io.forward(j).bits.rid && valid(i), true.B, false.B))
    }
    
  })

  io.rdata := DontCare
  raddr := OHToUInt(raddrOh.asUInt)
  val real_read = raddrOh.reduce(_ | _)
  when(io.read.valid && real_read){
    io.rdata := data(raddr).asUInt
    assert(PopCount(raddrOh.asUInt) <= 1.U)
  }

  freeAddr := OHToUInt(freeOh.asUInt)
  val real_free = freeOh.reduce(_ | _)
  when(io.free.valid && real_free){
    valid(freeAddr) := false.B
    assert(PopCount(freeOh.asUInt) <= 1.U)
  }
  
  (0 until LoadPipelineWidth).map({ i =>
    io.forwardData := DontCare
  })

  (0 until LoadPipelineWidth).map{ i =>
    forwardAddr(i) := OHToUInt(forwardOh(i).asUInt)
    val real_forward = forwardOh(i).reduce(_ | _)
    when(io.forward(i).valid){
      val flatData = data(forwardAddr(i)).reduceLeft((a,b) => Cat(b,a))
      val reshapedData = VecInit(Seq.tabulate(8)(j => flatData(((j + 1) * 64) - 1, j * 64)))
      io.forwardData(i) := reshapedData
      assert(PopCount(forwardOh(i).asUInt) <= 1.U)
    }     
  }

  io.full := valid.reduce(_ && _)

  when(io.write.valid){
    assert(!io.full)
    when(io.write.bits.refill_count === 0.U){
      infilghtAddr := waddr
      infilght := true.B
      infilghtId := io.write.bits.wid
      data(waddr)(io.write.bits.wbeat) := io.write.bits.wdata 
    }.otherwise{
      id(infilghtAddr) := io.write.bits.wid
      valid(infilghtAddr) := true.B
      data(infilghtAddr)(io.write.bits.wbeat) := io.write.bits.wdata
      infilght := false.B
    }
  }
}