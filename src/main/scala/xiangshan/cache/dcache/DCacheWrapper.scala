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
import chisel3.experimental.ExtModule
import chisel3.util._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, TransferSizes}
import freechips.rocketchip.tilelink.{TLBundleD, _}
import freechips.rocketchip.util.BundleFieldBase
import xs.utils.cache.common._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.LazyModuleImp
import xiangshan._
import xiangshan.backend.rob.RobDebugRollingIO
import xiangshan.cache.wpu._
import xiangshan.mem.{AddPipelineReg, HasL1PrefetchSourceParameter}
import xiangshan.mem.prefetch._
import xiangshan.mem.LqPtr
import xs.utils.{ChiselDB, Code, Constantin, FastArbiter, GTimer, PipelineConnect, PipelineConnectPipe, ReplacementPolicy, SRAMQueue}
import xs.utils.perf._
import xs.utils.tl._
import xs.utils.cache.common.{AliasField, IsKeywordField, IsKeywordKey, PrefetchField, VaddrField}

// DCache specific parameters
case class DCacheParameters
(
  nSets: Int = 128,
  nWays: Int = 8,
  rowBits: Int = 64,
  tagECC: Option[String] = None,
  dataECC: Option[String] = None,
  replacer: Option[String] = Some("setplru"),
  updateReplaceOn2ndmiss: Boolean = true,
  nMissEntries: Int = 1,
  nProbeEntries: Int = 1,
  nReleaseEntries: Int = 1,
  nMMIOEntries: Int = 1,
  nMMIOs: Int = 1,
  blockBytes: Int = 64,
  nMaxPrefetchEntry: Int = 1,
  alwaysReleaseData: Boolean = false,
  isKeywordBitsOpt: Option[Boolean] = Some(true),
  enableDataEcc: Boolean = false,
  enableTagEcc: Boolean = false
) extends L1CacheParameters {
  // if sets * blockBytes > 4KB(page size),
  // cache alias will happen,
  // we need to avoid this by recoding additional bits in L2 cache
  val setBytes = nSets * blockBytes
  val aliasBitsOpt = if(setBytes > pageSize) Some(log2Ceil(setBytes / pageSize)) else None

  def tagCode: Code = Code.fromString(tagECC)

  def dataCode: Code = Code.fromString(dataECC)
}

//           Physical Address
// --------------------------------------
// |   Physical Tag |  PIndex  | Offset |
// --------------------------------------
//                  |
//                  DCacheTagOffset
//
//           Virtual Address
// --------------------------------------
// | Above index  | Set | Bank | Offset |
// --------------------------------------
//                |     |      |        |
//                |     |      |        0
//                |     |      DCacheBankOffset
//                |     DCacheSetOffset
//                DCacheAboveIndexOffset

// Default DCache size = 64 sets * 8 ways * 8 banks * 8 Byte = 32K Byte

trait HasDCacheParameters extends HasL1CacheParameters with HasL1PrefetchSourceParameter{
  val cacheParams = dcacheParameters
  val cfg = cacheParams

  def encWordBits = cacheParams.dataCode.width(wordBits)
  def encRowBits = encWordBits * rowWords // for DuplicatedDataArray only
  def eccBits = encWordBits - wordBits

  def encTagBits = if(EnableTagEcc) cacheParams.tagCode.width(tagBits + ClientStates.width) else tagBits + ClientStates.width
  def tagECCBits = encTagBits - tagBits - ClientStates.width

  def encDataBits = if(EnableDataEcc) cacheParams.dataCode.width(DCacheSRAMRowBits) else DCacheSRAMRowBits
  def dataECCBits = encDataBits - DCacheSRAMRowBits

  def blockProbeAfterGrantCycles = 8 // give the processor some time to issue a request after a grant

  def nSourceType = 11
  def sourceTypeWidth = log2Up(nSourceType)
  // non-prefetch source < 3
  def LOAD_SOURCE = 0
  def STORE_SOURCE = 1
  def AMO_SOURCE = 2
  // prefetch source >= 3
  def DCACHE_PREFETCH_SOURCE = 3
  def SOFT_PREFETCH = 4
  // the following sources are only used inside SMS
  def HW_PREFETCH_AGT = 5
  def HW_PREFETCH_PHT_CUR = 6
  def HW_PREFETCH_PHT_INC = 7
  def HW_PREFETCH_PHT_DEC = 8
  def HW_PREFETCH_BOP = 9
  def HW_PREFETCH_STRIDE = 10
  def CMO_SOURCE = 11
  def BLOOM_FILTER_ENTRY_NUM = 4096

  // each source use a id to distinguish its multiple reqs
  def reqIdWidth = log2Up(nEntries) max log2Up(StoreBufferSize)

  require(isPow2(cfg.nMissEntries)) // TODO
  val nEntries = cfg.nMissEntries + cfg.nReleaseEntries
  val releaseIdBase = cfg.nMissEntries
  val EnableDataEcc = cacheParams.enableDataEcc
  val EnableTagEcc = cacheParams.enableTagEcc

  // banked dcache support
  val DCacheSetDiv = 1
  val DCacheSets = cacheParams.nSets
  val DCacheWays = cacheParams.nWays
  val DCacheBanks = 8 // hardcoded
  val DCacheDupNum = 16
  val DCacheSRAMRowBits = cacheParams.rowBits // hardcoded
  val DCacheWordBits = 64 // hardcoded
  val DCacheWordBytes = DCacheWordBits / 8
  val MaxPrefetchEntry = cacheParams.nMaxPrefetchEntry
  val DCacheVWordBytes = VLEN / 8
  require(DCacheSRAMRowBits == 64)

  val DCacheSetDivBits = log2Ceil(DCacheSetDiv)
  val DCacheSetBits = log2Ceil(DCacheSets)
  val DCacheSizeBits = DCacheSRAMRowBits * DCacheBanks * DCacheWays * DCacheSets
  val DCacheSizeBytes = DCacheSizeBits / 8
  val DCacheSizeWords = DCacheSizeBits / 64 // TODO

  val DCacheSameVPAddrLength = 12

  val DCacheSRAMRowBytes = DCacheSRAMRowBits / 8
  val DCacheWordOffset = log2Up(DCacheWordBytes)
  val DCacheVWordOffset = log2Up(DCacheVWordBytes)

  val DCacheBankOffset = log2Up(DCacheSRAMRowBytes)
  val DCacheSetOffset = DCacheBankOffset + log2Up(DCacheBanks)
  val DCacheAboveIndexOffset = DCacheSetOffset + log2Up(DCacheSets)
  val DCacheTagOffset = DCacheAboveIndexOffset min DCacheSameVPAddrLength
  val DCacheLineOffset = DCacheSetOffset

  // uncache
  val uncacheIdxBits = log2Up(VirtualLoadQueueMaxStoreQueueSize + 1)
  // hardware prefetch parameters
  // high confidence hardware prefetch port
  val HighConfHWPFLoadPort = LoadPipelineWidth - 1 // use the last load port by default
  val IgnorePrefetchConfidence = false

  // parameters about duplicating regs to solve fanout
  // In Main Pipe:
    // tag_write.ready -> data_write.valid * 8 banks
    // tag_write.ready -> meta_write.valid
    // tag_write.ready -> tag_write.valid
    // tag_write.ready -> err_write.valid
    // tag_write.ready -> wb.valid
  val nDupTagWriteReady = DCacheBanks + 4
  // In Main Pipe:
    // data_write.ready -> data_write.valid * 8 banks
    // data_write.ready -> meta_write.valid
    // data_write.ready -> tag_write.valid
    // data_write.ready -> err_write.valid
    // data_write.ready -> wb.valid
  val nDupDataWriteReady = DCacheBanks + 4
  val nDupWbReady = DCacheBanks + 4
  val nDupStatus = nDupTagWriteReady + nDupDataWriteReady
  val dataWritePort = 0
  val metaWritePort = DCacheBanks
  val tagWritePort = metaWritePort + 1
  val errWritePort = tagWritePort + 1
  val wbPort = errWritePort + 1

  val ProbeReplayDelayCycles = 2
  val ProbeqReplayCountBits = log2Up(ProbeReplayDelayCycles)

  val MissqDataBufferDepth = 2 
  val nGrantDataEntries = (cfg.nMissEntries - MissqDataBufferDepth) * refillCycles

  def set_to_dcache_div(set: UInt) = {
    require(set.getWidth >= DCacheSetBits)
    if (DCacheSetDivBits == 0) 0.U else set(DCacheSetDivBits-1, 0)
  }

  def set_to_dcache_div_set(set: UInt) = {
    require(set.getWidth >= DCacheSetBits)
    set(DCacheSetBits - 1, DCacheSetDivBits)
  }

  def addr_to_dcache_bank(addr: UInt) = {
    require(addr.getWidth >= DCacheSetOffset)
    addr(DCacheSetOffset-1, DCacheBankOffset)
  }

  def addr_to_dcache_div(addr: UInt) = {
    require(addr.getWidth >= DCacheAboveIndexOffset)
    if(DCacheSetDivBits == 0) 0.U else addr(DCacheSetOffset + DCacheSetDivBits - 1, DCacheSetOffset)
  }

  def addr_to_dcache_div_set(addr: UInt) = {
    require(addr.getWidth >= DCacheAboveIndexOffset)
    addr(DCacheAboveIndexOffset - 1, DCacheSetOffset + DCacheSetDivBits)
  }

  def addr_to_dcache_set(addr: UInt) = {
    require(addr.getWidth >= DCacheAboveIndexOffset)
    addr(DCacheAboveIndexOffset-1, DCacheSetOffset)
  }

  def get_data_of_bank(bank: Int, data: UInt) = {
    require(data.getWidth >= (bank+1)*DCacheSRAMRowBits)
    data(DCacheSRAMRowBits * (bank + 1) - 1, DCacheSRAMRowBits * bank)
  }

  def get_mask_of_bank(bank: Int, data: UInt) = {
    require(data.getWidth >= (bank+1)*DCacheSRAMRowBytes)
    data(DCacheSRAMRowBytes * (bank + 1) - 1, DCacheSRAMRowBytes * bank)
  }

  def get_alias(vaddr: UInt): UInt ={
    // require(blockOffBits + idxBits > pgIdxBits)
    if(blockOffBits + idxBits > pgIdxBits){
      vaddr(blockOffBits + idxBits - 1, pgIdxBits)
    }else{
      0.U
    }
  }

  def is_alias_match(vaddr0: UInt, vaddr1: UInt): Bool = {
    require(vaddr0.getWidth == VAddrBits && vaddr1.getWidth == VAddrBits)
    if(blockOffBits + idxBits > pgIdxBits) {
      vaddr0(blockOffBits + idxBits - 1, pgIdxBits) === vaddr1(blockOffBits + idxBits - 1, pgIdxBits)
    }else {
      // no alias problem
      true.B
    }
  }

  def get_direct_map_way(addr:UInt): UInt = {
    addr(DCacheAboveIndexOffset + log2Up(DCacheWays) - 1, DCacheAboveIndexOffset)
  }

  def arbiter[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    name: Option[String] = None): Unit = {
    val arb = Module(new Arbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) {
      a <> req
    }
    out <> arb.io.out
  }

  def arbiter_with_pipereg[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    name: Option[String] = None): Unit = {
    val arb = Module(new Arbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) {
      a <> req
    }
    AddPipelineReg(arb.io.out, out, false.B)
  }

  def arbiter_with_pipereg_N_dup[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    dups: Seq[DecoupledIO[T]],
    name: Option[String] = None): Unit = {
    val arb = Module(new Arbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) {
      a <> req
    }
    for (dup <- dups) {
      AddPipelineReg(arb.io.out, dup, false.B)
    }
    AddPipelineReg(arb.io.out, out, false.B)
  }

  def rrArbiter[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    name: Option[String] = None): Unit = {
    val arb = Module(new RRArbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) {
      a <> req
    }
    out <> arb.io.out
  }

  def fastArbiter[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    name: Option[String] = None): Unit = {
    val arb = Module(new FastArbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) {
      a <> req
    }
    out <> arb.io.out
  }

  val numReplaceRespPorts = 2

  require(isPow2(nSets), s"nSets($nSets) must be pow2")
  require(isPow2(nWays), s"nWays($nWays) must be pow2")
  require(full_divide(rowBits, wordBits), s"rowBits($rowBits) must be multiple of wordBits($wordBits)")
  require(full_divide(beatBits, rowBits), s"beatBits($beatBits) must be multiple of rowBits($rowBits)")
}

abstract class DCacheModule(implicit p: Parameters) extends L1CacheModule
  with HasDCacheParameters

abstract class DCacheBundle(implicit p: Parameters) extends L1CacheBundle
  with HasDCacheParameters

class ReplacementAccessBundle(implicit p: Parameters) extends DCacheBundle {
  val set = UInt(log2Up(nSets).W)
  val way = UInt(log2Up(nWays).W)
}

class ReplacementWayReqIO(implicit p: Parameters) extends DCacheBundle {
  val set = ValidIO(UInt(log2Up(nSets).W))
  val dmWay = Output(UInt(log2Up(nWays).W))
  val way = Input(UInt(log2Up(nWays).W))
}

class DCacheExtraMeta(implicit p: Parameters) extends DCacheBundle
{
  val error = Bool() // cache line has been marked as corrupted by l2 / ecc error detected when store
  val prefetch = UInt(L1PfSourceBits.W) // cache line is first required by prefetch
  val access = Bool() // cache line has been accessed by load / store

  // val debug_access_timestamp = UInt(64.W) // last time a load / store / refill access that cacheline
}

// memory request in word granularity(load, mmio, lr/sc, atomics)
class DCacheWordReq(implicit p: Parameters) extends DCacheBundle
{
  val cmd    = UInt(M_SZ.W)
  val vaddr  = UInt(VAddrBits.W)
  val data   = UInt(VLEN.W)
  val mask   = UInt((VLEN/8).W)
  val id     = UInt(reqIdWidth.W)
  val instrtype   = UInt(sourceTypeWidth.W)
  val isFirstIssue = Bool()
  val replayCarry = new ReplayCarry(nWays)
  val lqIdx = new LqPtr

  val debug_robIdx = UInt(log2Ceil(RobSize).W)
  def dump() = {
    XSDebug("DCacheWordReq: cmd: %x vaddr: %x data: %x mask: %x id: %d\n",
      cmd, vaddr, data, mask, id)
  }
}

// memory request in word granularity(store)
class DCacheLineReq(implicit p: Parameters) extends DCacheBundle
{
  val cmd    = UInt(M_SZ.W)
  val vaddr  = UInt(VAddrBits.W)
  val addr   = UInt(PAddrBits.W)
  val data   = UInt((cfg.blockBytes * 8).W)
  val mask   = UInt(cfg.blockBytes.W)
  val id     = UInt(reqIdWidth.W)
  def dump() = {
    XSDebug("DCacheLineReq: cmd: %x addr: %x data: %x mask: %x id: %d\n",
      cmd, addr, data, mask, id)
  }
  def idx: UInt = get_idx(vaddr)
}

class DCacheWordReqWithVaddr(implicit p: Parameters) extends DCacheWordReq {
  val addr = UInt(PAddrBits.W)
  val wline = Bool()
}

class DCacheWordReqWithVaddrAndPfFlag(implicit p: Parameters) extends DCacheWordReqWithVaddr {
  val prefetch = Bool()
  val vecValid = Bool()

  def toDCacheWordReqWithVaddr() = {
    val res = Wire(new DCacheWordReqWithVaddr)
    res.vaddr := vaddr
    res.wline := wline
    res.cmd := cmd
    res.addr := addr
    res.data := data
    res.mask := mask
    res.id := id
    res.instrtype := instrtype
    res.replayCarry := replayCarry
    res.isFirstIssue := isFirstIssue
    res.debug_robIdx := debug_robIdx

    res
  }
}

class BaseDCacheWordResp(implicit p: Parameters) extends DCacheBundle
{
  // read in s2
  val data = UInt(VLEN.W)
  // select in s3
  val data_delayed = UInt(VLEN.W)
  val id     = UInt(reqIdWidth.W)
  // cache req missed, send it to miss queue
  val miss   = Bool()
  // cache miss, and failed to enter the missqueue, replay from RS is needed
  val replay = Bool()
  val replayCarry = new ReplayCarry(nWays)
  // data has been corrupted
  val tag_error = Bool() // tag error
  val mshr_id = UInt(log2Up(cfg.nMissEntries).W)

  val debug_robIdx = UInt(log2Ceil(RobSize).W)
  def dump() = {
    XSDebug("DCacheWordResp: data: %x id: %d miss: %b replay: %b\n",
      data, id, miss, replay)
  }
}

class DCacheWordResp(implicit p: Parameters) extends BaseDCacheWordResp
{
  val meta_prefetch = UInt(L1PfSourceBits.W)
  val meta_access = Bool()
  // s2
  val handled = Bool()
  val real_miss = Bool()
  // s3: 1 cycle after data resp
  val error_delayed = Bool() // all kinds of errors, include tag error
}

class BankedDCacheWordResp(implicit p: Parameters) extends DCacheWordResp
{
  val bank_data = Vec(DCacheBanks, Bits(DCacheSRAMRowBits.W))
  val bank_oh = UInt(DCacheBanks.W)
}

class DCacheWordRespWithError(implicit p: Parameters) extends BaseDCacheWordResp
{
  val error = Bool() // all kinds of errors, include tag error
  val nderr = Bool()
  val isNC = Bool()
  val isStore = Bool()
}

class DCacheLineResp(implicit p: Parameters) extends DCacheBundle
{
  val data   = UInt((cfg.blockBytes * 8).W)
  // cache req missed, send it to miss queue
  val miss   = Bool()
  // cache req nacked, replay it later
  val replay = Bool()
  val id     = UInt(reqIdWidth.W)
  def dump() = {
    XSDebug("DCacheLineResp: data: %x id: %d miss: %b replay: %b\n",
      data, id, miss, replay)
  }
}

class Refill(implicit p: Parameters) extends DCacheBundle
{
  val addr   = UInt(PAddrBits.W)
  val data   = UInt(l1BusDataWidth.W)
  val error  = Bool() // refilled data has been corrupted
  // for debug usage
  val data_raw = UInt((cfg.blockBytes * 8).W)
  val hasdata = Bool()
  val refill_done = Bool()
  def dump() = {
    XSDebug("Refill: addr: %x data: %x\n", addr, data)
  }
  val id     = UInt(log2Up(cfg.nMissEntries).W)
}

class Release(implicit p: Parameters) extends DCacheBundle
{
  val paddr  = UInt(PAddrBits.W)
  def dump() = {
    XSDebug("Release: paddr: %x\n", paddr(PAddrBits-1, DCacheTagOffset))
  }
}

class DCacheWordIO(implicit p: Parameters) extends DCacheBundle
{
  val req  = DecoupledIO(new DCacheWordReq)
  val resp = Flipped(DecoupledIO(new DCacheWordResp))
}


class UncacheWordReq(implicit p: Parameters) extends DCacheBundle
{
  val cmd  = UInt(M_SZ.W)
  val addr = UInt(PAddrBits.W)
  val data = UInt(XLEN.W)
  val mask = UInt((XLEN/8).W)
  val id   = UInt(uncacheIdxBits.W)
  val instrtype = UInt(sourceTypeWidth.W)
  val nc = Bool()
  val isFirstIssue = Bool()
  val replayCarry = new ReplayCarry(nWays)

  def dump() = {
    XSDebug("UncacheWordReq: cmd: %x addr: %x data: %x mask: %x id: %d\n",
      cmd, addr, data, mask, id)
  }
}

class UncacheWordResp(implicit p: Parameters) extends DCacheBundle
{
  val data      = UInt(XLEN.W)
  val data_delayed = UInt(XLEN.W)
  val id        = UInt(uncacheIdxBits.W)
  val miss      = Bool()
  val replay    = Bool()
  val tag_error = Bool()
  val error     = Bool()
  val nderr     = Bool()
  val isNC      = Bool()
  val isStore   = Bool()
  val replayCarry = new ReplayCarry(nWays)
  val mshr_id = UInt(log2Up(cfg.nMissEntries).W)  // FIXME: why uncacheWordResp is not merged to baseDcacheResp

  val debug_robIdx = UInt(log2Ceil(RobSize).W)
  def dump() = {
    XSDebug("UncacheWordResp: data: %x id: %d miss: %b replay: %b, tag_error: %b, error: %b\n",
      data, id, miss, replay, tag_error, error)
  }
}

class UncacheWordIO(implicit p: Parameters) extends DCacheBundle
{
  val req  = DecoupledIO(new UncacheWordReq)
  val resp = Flipped(DecoupledIO(new UncacheWordResp))
}

class MainPipeResp(implicit p: Parameters) extends DCacheBundle {
  //distinguish amo
  val source  = UInt(sourceTypeWidth.W)
  val data    = UInt(DataBits.W)
  val miss    = Bool()
  val miss_id = UInt(log2Up(cfg.nMissEntries).W)
  val replay  = Bool()
  val error   = Bool()

  val ack_miss_queue = Bool()

  val id     = UInt(reqIdWidth.W)

  def isAMO: Bool = source === AMO_SOURCE.U
  def isStore: Bool = source === STORE_SOURCE.U
}

class AtomicWordIO(implicit p: Parameters) extends DCacheBundle
{
  val req  = DecoupledIO(new MainPipeReq)
  val resp = Flipped(ValidIO(new MainPipeResp))
  val block_lr = Input(Bool())
}

// used by load unit
class DCacheLoadIO(implicit p: Parameters) extends DCacheWordIO
{
  // kill previous cycle's req
  val s1_kill_data_read = Output(Bool()) // only kill bandedDataRead at s1
  val s1_kill           = Output(Bool()) // kill loadpipe req at s1
  val s2_kill           = Output(Bool())
  val s0_pc             = Output(UInt(VAddrBits.W))
  val s1_pc             = Output(UInt(VAddrBits.W))
  val s2_pc             = Output(UInt(VAddrBits.W))
  val is128Req = Bool()
  // cycle 0: prefetch source bits
  val pf_source = Output(UInt(L1PfSourceBits.W))
  // cycle0: load microop
 // val s0_uop = Output(new MicroOp)
  // cycle 0: virtual address: req.addr
  // cycle 1: physical address: s1_paddr
  val s1_paddr_dup_lsu = Output(UInt(PAddrBits.W)) // lsu side paddr
  val s1_paddr_dup_dcache = Output(UInt(PAddrBits.W)) // dcache side paddr
  val s1_disable_fast_wakeup = Input(Bool())
  // cycle 2: hit signal
  val s2_hit = Input(Bool()) // hit signal for lsu,
  val s2_first_hit = Input(Bool())
  val s2_bank_conflict = Input(Bool())
  val s2_wpu_pred_fail = Input(Bool())
  val s2_mq_nack = Input(Bool())

  // debug
  val debug_s1_hit_way = Input(UInt(nWays.W))
  val debug_s2_pred_way_num = Input(UInt(XLEN.W))
  val debug_s2_dm_way_num = Input(UInt(XLEN.W))
  val debug_s2_real_way_num = Input(UInt(XLEN.W))
}

class DCacheLineIO(implicit p: Parameters) extends DCacheBundle
{
  val req  = DecoupledIO(new DCacheLineReq)
  val resp = Flipped(DecoupledIO(new DCacheLineResp))
}

class RefillToSbuffer(implicit p: Parameters) extends  DCacheBundle{
  val data = Output(UInt(l1BusDataWidth.W))
  val id = Output(UInt(reqIdWidth.W))
  val refill_count = Output(UInt((blockBytes/beatBytes).W))
  val isKeyword = Output(Bool())
}

class DCacheToSbufferIO(implicit p: Parameters) extends DCacheBundle {
  // sbuffer will directly send request to dcache main pipe
  val req = Flipped(Decoupled(new DCacheLineReq))

  val main_pipe_hit_resp = ValidIO(new DCacheLineResp)
  val amo_hit_resp = ValidIO(new DCacheLineResp)
  //val refill_hit_resp = ValidIO(new DCacheLineResp)

  val replay_resp = ValidIO(new DCacheLineResp)

  val refill_row_data = ValidIO(new RefillToSbuffer)
  val refill_to_mp_req = new MainPipeInfoToSbuffer
  val refill_to_mp_resp = Flipped(ValidIO(new DCacheLineReq))

  //def hit_resps: Seq[ValidIO[DCacheLineResp]] = Seq(main_pipe_hit_resp, refill_hit_resp)
  def hit_resps: Seq[ValidIO[DCacheLineResp]] = Seq(main_pipe_hit_resp, amo_hit_resp)
}

// forward tilelink channel D's data to ldu
class DcacheToLduForwardIO(implicit p: Parameters) extends DCacheBundle {
  val valid = Bool()
  val data = UInt(l1BusDataWidth.W)
  val mshrid = UInt(log2Up(cfg.nMissEntries).W)
  val last = Bool()

  def apply(req_valid : Bool, req_data : UInt, req_mshrid : UInt, req_last : Bool) = {
    valid := req_valid
    data := req_data
    mshrid := req_mshrid
    last := req_last
  }

  def dontCare() = {
    valid := false.B
    data := DontCare
    mshrid := DontCare
    last := DontCare
  }

  def forward(req_valid : Bool, req_mshr_id : UInt, req_paddr : UInt) = {
    val all_match = req_valid && valid &&
                req_mshr_id === mshrid &&
                req_paddr(log2Up(refillBytes)) === last
    val forward_D = RegInit(false.B)
    val forwardData = RegInit(VecInit(List.fill(VLEN/8)(0.U(8.W))))

    val block_idx = req_paddr(log2Up(refillBytes) - 1, 3)
    val block_data = Wire(Vec(l1BusDataWidth / 64, UInt(64.W)))
    (0 until l1BusDataWidth / 64).map(i => {
      block_data(i) := data(64 * i + 63, 64 * i)
    })
    val selected_data = Wire(UInt(128.W))
    selected_data := Mux(req_paddr(3), Fill(2, block_data(block_idx)), Cat(block_data(block_idx + 1.U), block_data(block_idx)))

    forward_D := all_match
    for (i <- 0 until VLEN/8) {
      when (all_match) {
        forwardData(i) := selected_data(8 * i + 7, 8 * i)
      }
    }

    (forward_D, forwardData)
  }
}

class MissEntryForwardIO(implicit p: Parameters) extends DCacheBundle {
  val inflight = Bool()
  val paddr = UInt(PAddrBits.W)
  val raw_data = Vec(blockRows, UInt(rowBits.W))
  val firstbeat_valid = Bool()
  val lastbeat_valid = Bool()

  def apply(mshr_valid : Bool, mshr_paddr : UInt, mshr_rawdata : Vec[UInt], mshr_first_valid : Bool, mshr_last_valid : Bool) = {
    inflight := mshr_valid
    paddr := mshr_paddr
    raw_data := mshr_rawdata
    firstbeat_valid := mshr_first_valid
    lastbeat_valid := mshr_last_valid
  }

  // check if we can forward from mshr or D channel
  def check(req_valid : Bool, req_paddr : UInt) = {
    RegNext(req_valid && inflight && req_paddr(PAddrBits - 1, blockOffBits) === paddr(PAddrBits - 1, blockOffBits)) // TODO: clock gate(1-bit)
  }

  def forward(req_valid : Bool, req_paddr : UInt, mshr_raw_data: Vec[UInt]) = {
    val all_match = (req_paddr(log2Up(refillBytes)) === 0.U && firstbeat_valid) ||
                    (req_paddr(log2Up(refillBytes)) === 1.U && lastbeat_valid)

    val forward_mshr = RegInit(false.B)
    val forwardData = RegInit(VecInit(List.fill(VLEN/8)(0.U(8.W))))

    val block_idx = req_paddr(log2Up(refillBytes), 3)
    val block_data = mshr_raw_data

    val selected_data = Wire(UInt(128.W))
    selected_data := Mux(req_paddr(3), Fill(2, block_data(block_idx)), Cat(block_data(block_idx + 1.U), block_data(block_idx)))

    forward_mshr := all_match
    for (i <- 0 until VLEN/8) {
      forwardData(i) := selected_data(8 * i + 7, 8 * i)
    }

    (forward_mshr, forwardData)
  }
}

// forward mshr's data to ldu
class LduToMissqueueForwardIO(implicit p: Parameters) extends DCacheBundle {
  // req
  val valid = Input(Bool())
  val mshrid = Input(UInt(log2Up(cfg.nMissEntries).W))
  val paddr = Input(UInt(PAddrBits.W))
  // resp
  val forward_mshr = Output(Bool())
  val forwardData = Output(Vec(VLEN/8, UInt(8.W)))
  val forward_result_valid = Output(Bool())

  def connect(sink: LduToMissqueueForwardIO) = {
    sink.valid := valid
    sink.mshrid := mshrid
    sink.paddr := paddr
    forward_mshr := sink.forward_mshr
    forwardData := sink.forwardData
    forward_result_valid := sink.forward_result_valid
  }

  def forward() = {
    (forward_result_valid, forward_mshr, forwardData)
  }
}

class StorePrefetchReq(implicit p: Parameters) extends DCacheBundle {
  val paddr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
}

class DCacheToLsuIO(implicit p: Parameters) extends DCacheBundle {
  val load  = Vec(LoadPipelineWidth, Flipped(new DCacheLoadIO)) // for speculative load
  val sta   = Vec(StorePipelineWidth, Flipped(new DCacheStoreIO)) // for non-blocking store
  //val lsq = ValidIO(new Refill)  // refill to load queue, wake up load misses
  val tl_d_channel = Output(new DcacheToLduForwardIO)
  val store = new DCacheToSbufferIO // for sbuffer
  val atomics  = Flipped(new AtomicWordIO)  // atomics reqs
  val release = ValidIO(new Release) // cacheline release hint for ld-ld violation check
  val forward_D = Output(Vec(LoadPipelineWidth, new DcacheToLduForwardIO))
  val forward_mshr = Vec(LoadPipelineWidth, new LduToMissqueueForwardIO)
}

class DCacheTopDownIO(implicit p: Parameters) extends DCacheBundle {
  val robHeadVaddr = Flipped(Valid(UInt(VAddrBits.W)))
  val robHeadMissInDCache = Output(Bool())
  val robHeadOtherReplay = Input(Bool())
}

class DCacheIO(implicit p: Parameters) extends DCacheBundle {
  val hartId = Input(UInt(hartIdLen.W))
  val l2_pf_store_only = Input(Bool())
  val lsu = new DCacheToLsuIO
  val csr = new L1CacheToCsrIO
  val error = ValidIO(new L1CacheErrorInfo)
  val mshrFull = Output(Bool())
  val memSetPattenDetected = Output(Bool())
  val lqEmpty = Input(Bool())
  val pf_ctrl = Output(new PrefetchControlBundle)
  val force_write = Input(Bool())
  val sms_agt_evict_req = DecoupledIO(new AGTEvictReq)
  val debugTopDown = new DCacheTopDownIO
  val debugRolling = Flipped(new RobDebugRollingIO)
  val l2_hint = Input(Valid(new L2ToL1Hint()))
  val cmoOpReq = Flipped(DecoupledIO(new MissReq))
  // val cmoOpResp = DecoupledIO(new CMOResp)
  val cmofinish = Bool()
}

private object ArbiterCtrl {
  def apply(request: Seq[Bool]): Seq[Bool] = request.length match {
    case 0 => Seq()
    case 1 => Seq(true.B)
    case _ => true.B +: request.tail.init.scanLeft(request.head)(_ || _).map(!_)
  }
}

class TreeArbiter[T <: MissReqWoStoreData](val gen: T, val n: Int) extends Module{
  val io = IO(new ArbiterIO(gen, n))

  def selectTree(in: Vec[Valid[T]], sIdx: UInt): Tuple2[UInt, T] = {
    if (in.length == 1) {
      (sIdx, in(0).bits)
    } else if (in.length == 2) {
      (
        Mux(in(0).valid, sIdx, sIdx + 1.U),
        Mux(in(0).valid, in(0).bits, in(1).bits)
      )
    } else {
      val half = in.length / 2
      val leftValid = in.slice(0, half).map(_.valid).reduce(_ || _)
      val (leftIdx, leftSel) = selectTree(VecInit(in.slice(0, half)), sIdx)
      val (rightIdx, rightSel) = selectTree(VecInit(in.slice(half, in.length)), sIdx + half.U)
      (
        Mux(leftValid, leftIdx, rightIdx),
        Mux(leftValid, leftSel, rightSel)
      )
    }
  }
  val ins = Wire(Vec(n, Valid(gen)))
  for (i <- 0 until n) {
    ins(i).valid := io.in(i).valid
    ins(i).bits  := io.in(i).bits
  }
  val (idx, sel) = selectTree(ins, 0.U)
  // NOTE: io.chosen is very slow, dont use it
  io.chosen := idx
  io.out.bits := sel

  val grant = ArbiterCtrl(io.in.map(_.valid))
  for ((in, g) <- io.in.zip(grant))
    in.ready := g && io.out.ready
  io.out.valid := !grant.last || io.in.last.valid
}

class DCacheMEQueryIOBundle(implicit p: Parameters) extends DCacheBundle
{
  val req              = ValidIO(new MissReqWoStoreData)
  val primary_ready    = Input(Bool())
  val secondary_ready  = Input(Bool())
  val secondary_reject = Input(Bool())
}

class DCacheMQQueryIOBundle(implicit p: Parameters) extends DCacheBundle
{
  val req    = ValidIO(new MissReq)
  val ready  = Input(Bool())
}

class MissReadyGen(val n: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val in = Vec(n, Flipped(DecoupledIO(new MissReq)))
    val queryMQ = Vec(n, new DCacheMQQueryIOBundle)
  })

  val mqReadyVec = io.queryMQ.map(_.ready)

  io.queryMQ.zipWithIndex.foreach{
    case (q, idx) => {
      q.req.valid := io.in(idx).valid
      q.req.bits  := io.in(idx).bits
    }
  }
  io.in.zipWithIndex.map {
    case (r, idx) => {
      if (idx == 0) {
        r.ready := mqReadyVec(idx)
      } else {
        r.ready := mqReadyVec(idx) && !Cat(io.in.slice(0, idx).map(_.valid)).orR
      }
    }
  }

}

class DCache()(implicit p: Parameters) extends LazyModule with HasDCacheParameters {
  override def shouldBeInlined: Boolean = false

  val reqFields: Seq[BundleFieldBase] = Seq(
    PrefetchField(),
    ReqSourceField(),
    VaddrField(VAddrBits - blockOffBits),
  //  IsKeywordField()
  ) ++ cacheParams.aliasBitsOpt.map(AliasField)
  val echoFields: Seq[BundleFieldBase] = Seq(
    IsKeywordField()
  )

  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "dcache",
      sourceId = IdRange(0, nEntries + 1),
      supportsProbe = TransferSizes(cfg.blockBytes)
    )),
    requestFields = reqFields :+ TLNanhuBusField(),
    echoFields = echoFields
  )

  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new DCacheImp(this)
}


class DCacheImp(outer: DCache) extends LazyModuleImp(outer) with HasDCacheParameters with HasPerfEvents with HasL1PrefetchSourceParameter {

  val io = IO(new DCacheIO)

  val (bus, edge) = outer.clientNode.out.head
  require(bus.d.bits.data.getWidth == l1BusDataWidth, "DCache: tilelink width does not match")

  println("DCache:")
  println("  DCacheSets: " + DCacheSets)
  println("  DCacheSetDiv: " + DCacheSetDiv)
  println("  DCacheWays: " + DCacheWays)
  println("  DCacheBanks: " + DCacheBanks)
  println("  DCacheSRAMRowBits: " + DCacheSRAMRowBits)
  println("  DCacheWordOffset: " + DCacheWordOffset)
  println("  DCacheBankOffset: " + DCacheBankOffset)
  println("  DCacheSetOffset: " + DCacheSetOffset)
  println("  DCacheTagOffset: " + DCacheTagOffset)
  println("  DCacheAboveIndexOffset: " + DCacheAboveIndexOffset)
  println("  DcacheMaxPrefetchEntry: " + MaxPrefetchEntry)
  println("  WPUEnable: " + dwpuParam.enWPU)
  println("  WPUEnableCfPred: " + dwpuParam.enCfPred)
  println("  WPUAlgorithm: " + dwpuParam.algoName)
  println("  HasCMO: " + HasCMO)

  // Enable L1 Store prefetch
  val StorePrefetchL1Enabled = EnableStorePrefetchAtCommit || EnableStorePrefetchAtIssue || EnableStorePrefetchSPB
  val MetaReadPort =
        if (StorePrefetchL1Enabled)
          1 + backendParams.LduCnt + backendParams.StaCnt 
        else
          1 + backendParams.LduCnt 
  val TagReadPort =
        if (StorePrefetchL1Enabled)
          1 + backendParams.LduCnt + backendParams.StaCnt 
        else
          1 + backendParams.LduCnt 

  // Enable L1 Load prefetch
  val LoadPrefetchL1Enabled = true
  val AccessArrayReadPort = if(LoadPrefetchL1Enabled) LoadPipelineWidth + 1 + 1 else LoadPipelineWidth + 1
  val PrefetchArrayReadPort = if(LoadPrefetchL1Enabled) LoadPipelineWidth + 1 + 1 else LoadPipelineWidth + 1

  //----------------------------------------
  // core data structures
  val bankedDataArray = if(dwpuParam.enWPU) Module(new SramedDataArray) else Module(new BankedDataArray)
  // val metaArray = Module(new L1CohMetaArray(readPorts = LoadPipelineWidth + 1, writePorts = 1))
  val errorArray = Module(new L1FlagMetaArray(readPorts = LoadPipelineWidth + 1, writePorts = 1))
  val prefetchArray = Module(new L1PrefetchSourceArray(readPorts = PrefetchArrayReadPort, writePorts = 1 + LoadPipelineWidth)) // prefetch flag array
  val accessArray = Module(new L1FlagMetaArray(readPorts = AccessArrayReadPort, writePorts = LoadPipelineWidth + 1))
  val tagArray = Module(new DuplicatedTagArray(readPorts = TagReadPort))
  val prefetcherMonitor = Module(new PrefetcherMonitor)
  val fdpMonitor =  Module(new FDPrefetcherMonitor)
  val bloomFilter =  Module(new BloomFilter(BLOOM_FILTER_ENTRY_NUM, true))
  val counterFilter = Module(new CounterFilter)
  bankedDataArray.dump()

  //----------------------------------------
  // miss queue
  // missReqArb port:
  // enableStorePrefetch: main pipe * 1 + load pipe * 2 + store pipe * 1 + storequeue-cmoreq * 1
  // hybrid * 1; disable: main pipe * 1 + load pipe * 2 + hybrid * 1
  // higher priority is given to lower indices
  val MissReqPortCount = if(StorePrefetchL1Enabled) 1 + backendParams.LduCnt + backendParams.StaCnt + 1 else 1 + backendParams.LduCnt + 1
  val MainPipeMissReqPort = 0
  // val HybridMissReqBase = MissReqPortCount 

  //----------------------------------------
  // core modules
  val ldu = Seq.tabulate(LoadPipelineWidth)({ i => Module(new LoadPipe(i))})
  val stu = Seq.tabulate(StorePipelineWidth)({ i => Module(new StorePipe(i))})
  val mainPipe     = Module(new MainPipe)
  // val refillPipe   = Module(new RefillPipe)
  val missQueue    = Module(new MissQueue(edge, MissReqPortCount))
  val probeQueue   = Module(new ProbeQueue(edge))
  val wb           = Module(new WritebackQueue(edge))

  // missQueue.io.cmoOpReq <> io.cmoOpReq
  // missQueue.io.cmoOpResp <> io.cmoOpResp
  io.cmofinish := missQueue.io.cmofinish
  missQueue.io.lqEmpty := io.lqEmpty
  missQueue.io.hartId := io.hartId
  missQueue.io.l2_pf_store_only := RegNext(io.l2_pf_store_only, false.B)
  missQueue.io.debugTopDown <> io.debugTopDown
  missQueue.io.l2_hint <> RegNext(io.l2_hint)
  missQueue.io.mainpipe_info := mainPipe.io.mainpipe_info
  mainPipe.io.refill_info := missQueue.io.refill_info
  mainPipe.io.replace <> missQueue.io.replace_block_query
  mainPipe.io.sms_agt_evict_req <> io.sms_agt_evict_req
  io.memSetPattenDetected := missQueue.io.memSetPattenDetected
  io.csr := DontCare

  val errors = ldu.map(_.io.error) ++ // load error
    Seq(mainPipe.io.error) // store / misc error
  val error_valid = errors.map(e => e.valid).reduce(_|_)
  io.error.bits <> RegEnable(
    Mux1H(errors.map(e => RegNext(e.valid) -> RegEnable(e.bits, e.valid))),
    RegNext(error_valid))
  io.error.valid := RegNext(RegNext(error_valid, init = false.B), init = false.B)

  //----------------------------------------
  // read extra meta (exclude stu)
  // (meta_read_ports.take(HybridLoadReadBase + 1) ++
  //  meta_read_ports.takeRight(backendParams.HyuCnt)).zip(errorArray.io.read).foreach { case (p, r) => r <> p }
  // (meta_read_ports.take(HybridLoadReadBase + 1) ++
  //  meta_read_ports.takeRight(backendParams.HyuCnt)).zip(prefetchArray.io.read).foreach { case (p, r) => r <> p }
  // (meta_read_ports.take(HybridLoadReadBase + 1) ++
  //  meta_read_ports.takeRight(backendParams.HyuCnt)).zip(accessArray.io.read).foreach { case (p, r) => r <> p }
  val tag_read_ports = ldu.map(_.io.tag_read) ++ Seq(mainPipe.io.tag_read)
  tag_read_ports.zip(errorArray.io.read).foreach { case (p, r) => r <> p}
  tag_read_ports.zip(prefetchArray.io.read).foreach { case (p, r) => r <> p}
  tag_read_ports.zip(accessArray.io.read).foreach { case (p, r) => r <> p}
  
  // val extra_meta_resp_ports = ldu.map(_.io.extra_meta_resp).take(HybridLoadReadBase) ++
  //   Seq(mainPipe.io.extra_meta_resp) ++
  //   ldu.map(_.io.extra_meta_resp).takeRight(backendParams.HyuCnt)
  val extra_meta_resp_ports = ldu.map(_.io.extra_meta_resp) ++ Seq(mainPipe.io.extra_meta_resp)
  extra_meta_resp_ports.zip(errorArray.io.resp).foreach { case (p, r) => {
    (0 until nWays).map(i => { p(i).error := r(i) })
  }}
  extra_meta_resp_ports.zip(prefetchArray.io.resp).foreach { case (p, r) => {
    (0 until nWays).map(i => { p(i).prefetch := r(i) })
  }}
  extra_meta_resp_ports.zip(accessArray.io.resp).foreach { case (p, r) => {
    (0 until nWays).map(i => { p(i).access := r(i) })
  }}

  if(LoadPrefetchL1Enabled) {
    // use last port to read prefetch and access flag
//    prefetchArray.io.read.last.valid := refillPipe.io.prefetch_flag_write.valid
//    prefetchArray.io.read.last.bits.idx := refillPipe.io.prefetch_flag_write.bits.idx
//    prefetchArray.io.read.last.bits.way_en := refillPipe.io.prefetch_flag_write.bits.way_en
//
//    accessArray.io.read.last.valid := refillPipe.io.prefetch_flag_write.valid
//    accessArray.io.read.last.bits.idx := refillPipe.io.prefetch_flag_write.bits.idx
//    accessArray.io.read.last.bits.way_en := refillPipe.io.prefetch_flag_write.bits.way_en
    prefetchArray.io.read.last.valid := mainPipe.io.prefetch_flag_write.valid
    prefetchArray.io.read.last.bits.idx := mainPipe.io.prefetch_flag_write.bits.idx
    prefetchArray.io.read.last.bits.way_en := mainPipe.io.prefetch_flag_write.bits.way_en

    accessArray.io.read.last.valid := mainPipe.io.prefetch_flag_write.valid
    accessArray.io.read.last.bits.idx := mainPipe.io.prefetch_flag_write.bits.idx
    accessArray.io.read.last.bits.way_en := mainPipe.io.prefetch_flag_write.bits.way_en

    val extra_flag_valid = RegNext(mainPipe.io.prefetch_flag_write.valid)
    val extra_flag_way_en = RegEnable(mainPipe.io.prefetch_flag_write.bits.way_en, mainPipe.io.prefetch_flag_write.valid)
    val extra_flag_prefetch = Mux1H(extra_flag_way_en, prefetchArray.io.resp.last)
    val extra_flag_access = Mux1H(extra_flag_way_en, accessArray.io.resp.last)

    prefetcherMonitor.io.validity.good_prefetch := extra_flag_valid && isPrefetchRelated(extra_flag_prefetch) && extra_flag_access
    prefetcherMonitor.io.validity.bad_prefetch := extra_flag_valid && isPrefetchRelated(extra_flag_prefetch) && !extra_flag_access
  }

  // write extra meta
  val error_flag_write_ports = Seq(
    mainPipe.io.error_flag_write // error flag generated by corrupted store
    // refillPipe.io.error_flag_write // corrupted signal from l2
  )
  error_flag_write_ports.zip(errorArray.io.write).foreach { case (p, w) => w <> p }

  val prefetch_flag_write_ports = ldu.map(_.io.prefetch_flag_write) ++ Seq(
    mainPipe.io.prefetch_flag_write // set prefetch_flag to false if coh is set to Nothing
    // refillPipe.io.prefetch_flag_write // refill required by prefetch will set prefetch_flag
  )
  prefetch_flag_write_ports.zip(prefetchArray.io.write).foreach { case (p, w) => w <> p }

  // FIXME: add hybrid unit?
  val same_cycle_update_pf_flag = ldu(0).io.prefetch_flag_write.valid && ldu(1).io.prefetch_flag_write.valid && (ldu(0).io.prefetch_flag_write.bits.idx === ldu(1).io.prefetch_flag_write.bits.idx) && (ldu(0).io.prefetch_flag_write.bits.way_en === ldu(1).io.prefetch_flag_write.bits.way_en)
  XSPerfAccumulate("same_cycle_update_pf_flag", same_cycle_update_pf_flag)

  val access_flag_write_ports = ldu.map(_.io.access_flag_write) ++ Seq(
    mainPipe.io.access_flag_write
    // refillPipe.io.access_flag_write
  )
  access_flag_write_ports.zip(accessArray.io.write).foreach { case (p, w) => w <> p }

  //----------------------------------------
  // tag array
  if(StorePrefetchL1Enabled) {
    require(tagArray.io.read.size == (LoadPipelineWidth + StorePipelineWidth + 1))
  }else {
    require(tagArray.io.read.size == (LoadPipelineWidth + 1))
  }
  // val tag_write_intend = missQueue.io.refill_pipe_req.valid || mainPipe.io.tag_write_intend
  val tag_write_intend = mainPipe.io.tag_write_intend
  assert(!RegNext(!tag_write_intend && tagArray.io.write.valid))
  ldu.zipWithIndex.foreach {
    case (ld, i) =>
      tagArray.io.read(i) <> ld.io.tag_read
      ld.io.tag_resp := tagArray.io.resp(i)
      ld.io.tag_read.ready := !tag_write_intend
  }
  if(StorePrefetchL1Enabled) {
    stu.zipWithIndex.foreach {
      case (st, i) =>
        tagArray.io.read(LoadPipelineWidth + i) <> st.io.tag_read
        st.io.tag_resp := tagArray.io.resp(LoadPipelineWidth + i)
        st.io.tag_read.ready := !tag_write_intend
    }
  }else {
    stu.foreach {
      case st =>
        st.io.tag_read.ready := false.B
        st.io.tag_resp := 0.U.asTypeOf(st.io.tag_resp)
    }
  }
  // for (i <- 0 until backendParams.HyuCnt) {
  //   val HybridLoadTagReadPort = HybridLoadReadBase + i
  //   val HybridStoreTagReadPort = HybridStoreReadBase + i
  //   val TagReadPort =
  //     if (EnableStorePrefetchSPB)
  //       HybridLoadReadBase + HybridStoreReadBase + i
  //     else
  //       HybridLoadReadBase + i

  //   // read tag
  //   ldu(HybridLoadTagReadPort).io.tag_read.ready := false.B
  //   stu(HybridStoreTagReadPort).io.tag_read.ready := false.B

  //   if (StorePrefetchL1Enabled) {
  //     when (ldu(HybridLoadTagReadPort).io.tag_read.valid) {
  //       tagArray.io.read(TagReadPort) <> ldu(HybridLoadTagReadPort).io.tag_read
  //       ldu(HybridLoadTagReadPort).io.tag_read.ready := !tag_write_intend
  //     } .otherwise {
  //       tagArray.io.read(TagReadPort) <> stu(HybridStoreTagReadPort).io.tag_read
  //       stu(HybridStoreTagReadPort).io.tag_read.ready := !tag_write_intend
  //     }
  //   } else {
  //     tagArray.io.read(TagReadPort) <> ldu(HybridLoadTagReadPort).io.tag_read
  //     ldu(HybridLoadTagReadPort).io.tag_read.ready := !tag_write_intend
  //   }

  //   // tag resp
  //   ldu(HybridLoadTagReadPort).io.tag_resp := tagArray.io.resp(TagReadPort)
  //   stu(HybridStoreTagReadPort).io.tag_resp := tagArray.io.resp(TagReadPort)
  // }
  tagArray.io.read.last <> mainPipe.io.tag_read
  mainPipe.io.tag_resp := tagArray.io.resp.last

  val fake_tag_read_conflict_this_cycle = PopCount(ldu.map(ld=> ld.io.tag_read.valid))
  XSPerfAccumulate("fake_tag_read_conflict", fake_tag_read_conflict_this_cycle)

  val tag_write_arb = Module(new Arbiter(new TagWriteReq, 1))
  // tag_write_arb.io.in(0) <> refillPipe.io.tag_write
  tag_write_arb.io.in(0) <> mainPipe.io.tag_write
  tagArray.io.write <> tag_write_arb.io.out

  ldu.map(m => {
    m.io.vtag_update.valid := tagArray.io.write.valid
    m.io.vtag_update.bits := tagArray.io.write.bits
  })

  //----------------------------------------
  // data array
  mainPipe.io.data_read.zip(ldu).map(x => x._1 := x._2.io.lsu.req.valid)

  val dataWriteArb = Module(new Arbiter(new L1BankedDataWriteReq, 1))
  // dataWriteArb.io.in(0) <> refillPipe.io.data_write
  dataWriteArb.io.in(0) <> mainPipe.io.data_write

  bankedDataArray.io.write <> dataWriteArb.io.out

  for (bank <- 0 until DCacheBanks) {
    val dataWriteArb_dup = Module(new Arbiter(new L1BankedDataWriteReqCtrl, 1))
    // dataWriteArb_dup.io.in(0).valid := refillPipe.io.data_write_dup(bank).valid
    // dataWriteArb_dup.io.in(0).bits := refillPipe.io.data_write_dup(bank).bits
    dataWriteArb_dup.io.in(0).valid := mainPipe.io.data_write.valid
    dataWriteArb_dup.io.in(0).bits := mainPipe.io.data_write.bits

    bankedDataArray.io.write_dup(bank) <> dataWriteArb_dup.io.out
  }

  bankedDataArray.io.readline <> mainPipe.io.data_readline
  bankedDataArray.io.readline_can_go := mainPipe.io.data_readline_can_go
  bankedDataArray.io.readline_stall := mainPipe.io.data_readline_stall
  bankedDataArray.io.readline_can_resp := mainPipe.io.data_readline_can_resp
  bankedDataArray.io.readline_intend := mainPipe.io.data_read_intend
  mainPipe.io.readline_error_delayed := bankedDataArray.io.readline_error_delayed
  mainPipe.io.data_resp := bankedDataArray.io.readline_resp

  (0 until LoadPipelineWidth).map(i => {
    bankedDataArray.io.read(i) <> ldu(i).io.banked_data_read
    bankedDataArray.io.is128Req(i) <> ldu(i).io.is128Req
    bankedDataArray.io.read_error_delayed(i) <> ldu(i).io.read_error_delayed

    ldu(i).io.banked_data_resp := bankedDataArray.io.read_resp(i)

    ldu(i).io.bank_conflict_slow := bankedDataArray.io.bank_conflict_slow(i)
  })

  // connect bus d
  missQueue.io.mem_grant.valid := false.B
  missQueue.io.mem_grant.bits := DontCare
  wb.io.mem_grant.valid := false.B
  wb.io.mem_grant.bits := DontCare


  // for grantDataQueue
  class GrantDataQueueEntry extends Bundle {
    val dWidth = bus.d.bits.getWidth

    val tlDBundle = new TLBundleD(edge.bundle)
    val tmp = Option.when(isPrime(dWidth) & (dWidth > 256)) (Bool()) //useless, due to mbist

    private def isPrime(n: Int): Boolean = {
      if (n <= 1) false
      else if (n == 2) true
      else !(2 to (math.sqrt(n).toInt + 1)).exists(x => n % x == 0)
    }
  }


  val grantDataQueue = Module(new SRAMQueue(new GrantDataQueueEntry, entries = (scala.math.pow(2,log2Up(nGrantDataEntries)).toInt + 1), flow = true, pipe = false, singlePort = true,
    hasMbist = hasMbist , suffix = "_grnt_q"))
  grantDataQueue.io.enq.valid := false.B
  grantDataQueue.io.enq.bits := DontCare

  val gDQDeqBitsWire = Wire(DecoupledIO(new TLBundleD(edge.bundle)))
  gDQDeqBitsWire.valid := grantDataQueue.io.deq.valid
  gDQDeqBitsWire.bits := grantDataQueue.io.deq.bits.tlDBundle
  grantDataQueue.io.deq.ready := gDQDeqBitsWire.ready

  // in L1DCache, we ony expect Grant[Data] and ReleaseAck
  bus.d.ready := false.B
  when(bus.d.bits.opcode === TLMessages.Grant || bus.d.bits.opcode === TLMessages.GrantData || bus.d.bits.opcode === TLMessages.CBOAck) {
    grantDataQueue.io.enq.valid := bus.d.valid
    grantDataQueue.io.enq.bits.tlDBundle := bus.d.bits
    bus.d.ready := grantDataQueue.io.enq.ready
  }.elsewhen(bus.d.bits.opcode === TLMessages.ReleaseAck) {
    wb.io.mem_grant <> bus.d
  }.otherwise {
    assert(!bus.d.fire)
  }

  val grantDataQueueConnectPipe = Module(new PipelineConnectPipe(new TLBundleD(edge.bundle)))
  grantDataQueueConnectPipe.io.in <> gDQDeqBitsWire
  grantDataQueueConnectPipe.io.out <> missQueue.io.mem_grant
  grantDataQueueConnectPipe.io.rightOutFire := missQueue.io.mem_grant.fire
  grantDataQueueConnectPipe.io.isFlush := false.B

//  missQueue.io.mem_grant <> gDQDeqBitsWire
 val isKeyword = missQueue.io.mem_grant.bits.echo.lift(IsKeywordKey).getOrElse(false.B)
  (0 until LoadPipelineWidth).map(i => {
    val (_, _, done, _) = edge.count(missQueue.io.mem_grant)
    when(missQueue.io.mem_grant.bits.opcode === TLMessages.GrantData) {
      io.lsu.forward_D(i).apply(missQueue.io.mem_grant.valid, missQueue.io.mem_grant.bits.data, missQueue.io.mem_grant.bits.source, isKeyword ^ done)
   //   io.lsu.forward_D(i).apply(bus.d.valid, bus.d.bits.data, bus.d.bits.source,done)
    }.otherwise {
      io.lsu.forward_D(i).dontCare()
    }
  })
  // tl D channel wakeup
  val (_, _, done, _) = edge.count(bus.d)
  when (missQueue.io.mem_grant.bits.opcode === TLMessages.GrantData || missQueue.io.mem_grant.bits.opcode === TLMessages.Grant) {
    io.lsu.tl_d_channel.apply(missQueue.io.mem_grant.valid, missQueue.io.mem_grant.bits.data, missQueue.io.mem_grant.bits.source, done)
  } .otherwise {
    io.lsu.tl_d_channel.dontCare()
  }
  mainPipe.io.force_write <> io.force_write

  /** dwpu */
  if (dwpuParam.enWPU) {
    val dwpu = Module(new DCacheWpuWrapper(LoadPipelineWidth))
    for(i <- 0 until LoadPipelineWidth){
      dwpu.io.req(i) <> ldu(i).io.dwpu.req(0)
      dwpu.io.resp(i) <> ldu(i).io.dwpu.resp(0)
      dwpu.io.lookup_upd(i) <> ldu(i).io.dwpu.lookup_upd(0)
      dwpu.io.cfpred(i) <> ldu(i).io.dwpu.cfpred(0)
    }
    dwpu.io.tagwrite_upd.valid := tagArray.io.write.valid
    dwpu.io.tagwrite_upd.bits.vaddr := tagArray.io.write.bits.vaddr
    dwpu.io.tagwrite_upd.bits.s1_real_way_en := tagArray.io.write.bits.way_en
  } else {
    for(i <- 0 until LoadPipelineWidth){
      ldu(i).io.dwpu.req(0).ready := true.B
      ldu(i).io.dwpu.resp(0).valid := false.B
      ldu(i).io.dwpu.resp(0).bits := DontCare
    }
  }

  //----------------------------------------
  // load pipe
  // the s1 kill signal
  // only lsu uses this, replay never kills
  for (w <- 0 until LoadPipelineWidth) {
    ldu(w).io.lsu <> io.lsu.load(w)

    // TODO:when have load128Req
    ldu(w).io.load128Req := io.lsu.load(w).is128Req

    // replay and nack not needed anymore
    // TODO: remove replay and nack
    ldu(w).io.nack := false.B

    ldu(w).io.disable_ld_fast_wakeup :=
      bankedDataArray.io.disable_ld_fast_wakeup(w) // load pipe fast wake up should be disabled when bank conflict
  }

  prefetcherMonitor.io.timely.total_prefetch := ldu.map(_.io.prefetch_info.naive.total_prefetch).reduce(_ || _)
  prefetcherMonitor.io.timely.late_hit_prefetch := ldu.map(_.io.prefetch_info.naive.late_hit_prefetch).reduce(_ || _)
  prefetcherMonitor.io.timely.late_miss_prefetch := missQueue.io.prefetch_info.naive.late_miss_prefetch
  prefetcherMonitor.io.timely.prefetch_hit := PopCount(ldu.map(_.io.prefetch_info.naive.prefetch_hit))
  io.pf_ctrl <> prefetcherMonitor.io.pf_ctrl
  XSPerfAccumulate("useless_prefetch", ldu.map(_.io.prefetch_info.naive.total_prefetch).reduce(_ || _) && !(ldu.map(_.io.prefetch_info.naive.useful_prefetch).reduce(_ || _)))
  XSPerfAccumulate("useful_prefetch", ldu.map(_.io.prefetch_info.naive.useful_prefetch).reduce(_ || _))
  XSPerfAccumulate("late_prefetch_hit", ldu.map(_.io.prefetch_info.naive.late_prefetch_hit).reduce(_ || _))
  XSPerfAccumulate("late_load_hit", ldu.map(_.io.prefetch_info.naive.late_load_hit).reduce(_ || _))

  /** LoadMissDB: record load miss state */
  val hartId = p(XSCoreParamsKey).HartId
  val isWriteLoadMissTable = Constantin.createRecord(s"isWriteLoadMissTable$hartId")
  val isFirstHitWrite = Constantin.createRecord(s"isFirstHitWrite$hartId")
  val tableName = s"LoadMissDB$hartId"
  val siteName = s"DcacheWrapper$hartId"
  val loadMissTable = ChiselDB.createTable(tableName, new LoadMissEntry)
  for( i <- 0 until LoadPipelineWidth){
    val loadMissEntry = Wire(new LoadMissEntry)
    val loadMissWriteEn =
      (!ldu(i).io.lsu.resp.bits.replay && ldu(i).io.miss_req.fire) ||
      (ldu(i).io.lsu.s2_first_hit && ldu(i).io.lsu.resp.valid && isFirstHitWrite.orR)
    loadMissEntry.timeCnt := GTimer()
    loadMissEntry.robIdx := ldu(i).io.lsu.resp.bits.debug_robIdx
    loadMissEntry.paddr := ldu(i).io.miss_req.bits.addr
    loadMissEntry.vaddr := ldu(i).io.miss_req.bits.vaddr
    loadMissEntry.missState := OHToUInt(Cat(Seq(
      ldu(i).io.miss_req.fire & ldu(i).io.miss_resp.merged,
      ldu(i).io.miss_req.fire & !ldu(i).io.miss_resp.merged,
      ldu(i).io.lsu.s2_first_hit && ldu(i).io.lsu.resp.valid
    )))
    loadMissTable.log(
      data = loadMissEntry,
      en = isWriteLoadMissTable.orR && loadMissWriteEn,
      site = siteName,
      clock = clock,
      reset = reset
    )
  }

  val isWriteLoadAccessTable = Constantin.createRecord(s"isWriteLoadAccessTable$hartId")
  val loadAccessTable = ChiselDB.createTable(s"LoadAccessDB$hartId", new LoadAccessEntry)
  for (i <- 0 until LoadPipelineWidth) {
    val loadAccessEntry = Wire(new LoadAccessEntry)
    loadAccessEntry.timeCnt := GTimer()
    loadAccessEntry.robIdx := ldu(i).io.lsu.resp.bits.debug_robIdx
    loadAccessEntry.paddr := ldu(i).io.miss_req.bits.addr
    loadAccessEntry.vaddr := ldu(i).io.miss_req.bits.vaddr
    loadAccessEntry.missState := OHToUInt(Cat(Seq(
      ldu(i).io.miss_req.fire & ldu(i).io.miss_resp.merged,
      ldu(i).io.miss_req.fire & !ldu(i).io.miss_resp.merged,
      ldu(i).io.lsu.s2_first_hit && ldu(i).io.lsu.resp.valid
    )))
    loadAccessEntry.pred_way_num := ldu(i).io.lsu.debug_s2_pred_way_num
    loadAccessEntry.real_way_num := ldu(i).io.lsu.debug_s2_real_way_num
    loadAccessEntry.dm_way_num := ldu(i).io.lsu.debug_s2_dm_way_num
    loadAccessTable.log(
      data = loadAccessEntry,
      en = isWriteLoadAccessTable.orR && ldu(i).io.lsu.resp.valid,
      site = siteName + "_loadpipe" + i.toString,
      clock = clock,
      reset = reset
    )
  }

  //----------------------------------------
  // Sta pipe
  for (w <- 0 until StorePipelineWidth) {
    stu(w).io.lsu <> io.lsu.sta(w)
  }

  //----------------------------------------
  // atomics
  // atomics not finished yet
  val atomic_resp_valid = mainPipe.io.atomic_resp.valid && mainPipe.io.atomic_resp.bits.isAMO
  io.lsu.atomics.resp.valid := RegNext(atomic_resp_valid)
  io.lsu.atomics.resp.bits := RegEnable(mainPipe.io.atomic_resp.bits, atomic_resp_valid)
  io.lsu.atomics.block_lr := mainPipe.io.block_lr
  // atomicsReplayUnit.io.pipe_resp := RegNext(mainPipe.io.atomic_resp)
  // atomicsReplayUnit.io.block_lr <> mainPipe.io.block_lr

  // Request
  val missReqArb = Module(new TreeArbiter(new MissReq, MissReqPortCount))
  // seperately generating miss queue enq ready for better timeing
//  val missReadyGen = Module(new MissReadyGen(MissReqPortCount))

  missReqArb.io.in(MainPipeMissReqPort) <> mainPipe.io.miss_req
//  missReadyGen.io.in(MainPipeMissReqPort) <> mainPipe.io.miss_req
  for (w <- 0 until backendParams.LduCnt) {
    missReqArb.io.in(w + 1) <> ldu(w).io.miss_req
//    missReadyGen.io.in(w + 1) <> ldu(w).io.miss_req
  }
  missReqArb.io.in(MissReqPortCount-1) <> io.cmoOpReq

  for (w <- 0 until LoadPipelineWidth) { ldu(w).io.miss_resp := missQueue.io.resp }
  mainPipe.io.miss_resp := missQueue.io.resp

  if(StorePrefetchL1Enabled) {
    for (w <- 0 until backendParams.StaCnt) {
      missReqArb.io.in(1 + backendParams.LduCnt + w) <> stu(w).io.miss_req
//      missReadyGen.io.in(1 + backendParams.LduCnt + w) <> stu(w).io.miss_req
    }
  }else {
    for (w <- 0 until backendParams.StaCnt) { stu(w).io.miss_req.ready := false.B }
  }


  for(w <- 0 until LoadPipelineWidth) {
    wb.io.miss_req_conflict_check(w) := ldu(w).io.wbq_conflict_check
    ldu(w).io.wbq_block_miss_req     := wb.io.block_miss_req(w)
  }

  wb.io.miss_req_conflict_check(2) := mainPipe.io.wbq_conflict_check
  mainPipe.io.wbq_block_miss_req   := wb.io.block_miss_req(2)
  
  wb.io.miss_req_conflict_check(3).valid := missReqArb.io.out.valid
  wb.io.miss_req_conflict_check(3).bits  := missReqArb.io.out.bits.addr
  missQueue.io.wbq_block_miss_req := wb.io.block_miss_req(3)

  missReqArb.io.out <> missQueue.io.req
//  missReadyGen.io.queryMQ <> missQueue.io.queryMQ

  for (w <- 0 until LoadPipelineWidth) { ldu(w).io.mq_enq_cancel := missQueue.io.mq_enq_cancel }

  XSPerfAccumulate("miss_queue_fire", PopCount(VecInit(missReqArb.io.in.map(_.fire))) >= 1.U)
  XSPerfAccumulate("miss_queue_muti_fire", PopCount(VecInit(missReqArb.io.in.map(_.fire))) > 1.U)

  XSPerfAccumulate("miss_queue_has_enq_req", PopCount(VecInit(missReqArb.io.in.map(_.valid))) >= 1.U)
  XSPerfAccumulate("miss_queue_has_muti_enq_req", PopCount(VecInit(missReqArb.io.in.map(_.valid))) > 1.U)
  XSPerfAccumulate("miss_queue_has_muti_enq_but_not_fire", PopCount(VecInit(missReqArb.io.in.map(_.valid))) > 1.U && PopCount(VecInit(missReqArb.io.in.map(_.fire))) === 0.U)

  // forward missqueue
  (0 until LoadPipelineWidth).map(i => io.lsu.forward_mshr(i).connect(missQueue.io.forward(i)))

  // refill to load queue
 // io.lsu.lsq <> missQueue.io.refill_to_ldq

  // tilelink stuff
  bus.a <> missQueue.io.mem_acquire
  bus.e <> missQueue.io.mem_finish
  missQueue.io.probe_addr := bus.b.bits.address

  missQueue.io.main_pipe_resp.valid := RegNext(mainPipe.io.atomic_resp.valid)
  missQueue.io.main_pipe_resp.bits := RegEnable(mainPipe.io.atomic_resp.bits, mainPipe.io.atomic_resp.valid)

  //----------------------------------------
  // probe
  // probeQueue.io.mem_probe <> bus.b
  block_decoupled(bus.b, probeQueue.io.mem_probe, missQueue.io.probe_block)
  probeQueue.io.lrsc_locked_block <> mainPipe.io.lrsc_locked_block
  probeQueue.io.update_resv_set <> mainPipe.io.update_resv_set

  val refill_req = RegNext(missQueue.io.main_pipe_req.valid && ((missQueue.io.main_pipe_req.bits.isLoad) | (missQueue.io.main_pipe_req.bits.isStore)))
  val refill_req_data_from_sb = missQueue.io.main_pipe_req.bits.isStore | missQueue.io.main_pipe_req.bits.isAMO
  val refill_req_data_from_sb_s1 = RegEnable(refill_req_data_from_sb, missQueue.io.main_pipe_req.fire)
  val refill_req_data_from_sb_s2 = RegNext(refill_req_data_from_sb_s1)
  //----------------------------------------
  // mainPipe
  // when a req enters main pipe, if it is set-conflict with replace pipe or refill pipe,
  // block the req in main pipe
  probeQueue.io.pipe_req <> mainPipe.io.probe_req
  probeQueue.io.pipe_resp <> mainPipe.io.probe_resp
  io.lsu.store.req <> mainPipe.io.store_req


  //sbuffer
  io.lsu.store.refill_row_data <> missQueue.io.refill_to_sbuffer
  io.lsu.store.refill_to_mp_req <> mainPipe.io.sbuffer_info
  when(io.lsu.store.refill_to_mp_resp.valid){
    mainPipe.io.refill_info.bits.store_data := io.lsu.store.refill_to_mp_resp.bits.data
    mainPipe.io.refill_info.bits.store_mask := io.lsu.store.refill_to_mp_resp.bits.mask
  }


  io.lsu.store.replay_resp.valid := RegNext(mainPipe.io.store_replay_resp.valid)
  io.lsu.store.replay_resp.bits := RegEnable(mainPipe.io.store_replay_resp.bits, mainPipe.io.store_replay_resp.valid)
  io.lsu.store.main_pipe_hit_resp := mainPipe.io.store_hit_resp
  io.lsu.store.amo_hit_resp := mainPipe.io.amo_hit_resp

  mainPipe.io.atomic_req <> io.lsu.atomics.req

  mainPipe.io.invalid_resv_set := RegNext(
    wb.io.req.fire &&
    wb.io.req.bits.addr === mainPipe.io.lrsc_locked_block.bits &&
    mainPipe.io.lrsc_locked_block.valid
  )

  //----------------------------------------
  // replace (main pipe)
  val mpStatus = mainPipe.io.status
  mainPipe.io.refill_req <> missQueue.io.main_pipe_req

  mainPipe.io.data_write.ready := true.B
  mainPipe.io.tag_write.ready := true.B 
  mainPipe.io.wb.ready := wb.io.req_ready_dup(0)

  //----------------------------------------
  // wb
  // add a queue between MainPipe and WritebackUnit to reduce MainPipe stalls due to WritebackUnit busy

  wb.io.req <> mainPipe.io.wb
  bus.c     <> wb.io.mem_release
  // wb.io.release_wakeup := refillPipe.io.release_wakeup
  // wb.io.release_update := mainPipe.io.release_update
  //wb.io.probe_ttob_check_req <> mainPipe.io.probe_ttob_check_req
  //wb.io.probe_ttob_check_resp <> mainPipe.io.probe_ttob_check_resp

  io.lsu.release.valid := RegNext(wb.io.req.fire)
  io.lsu.release.bits.paddr := RegEnable(wb.io.req.bits.addr, wb.io.req.fire)
  // Note: RegNext() is required by:
  // * load queue released flag update logic
  // * load / load violation check logic
  // * and timing requirements
  // CHANGE IT WITH CARE


  //----------------------------------------
  // Feedback Direct Prefetch Monitor
  fdpMonitor.io.refill := missQueue.io.prefetch_info.fdp.prefetch_monitor_cnt
  fdpMonitor.io.timely.late_prefetch := missQueue.io.prefetch_info.fdp.late_miss_prefetch
  fdpMonitor.io.accuracy.total_prefetch := missQueue.io.prefetch_info.fdp.total_prefetch
  for (w <- 0 until LoadPipelineWidth)  {
    if(w == 0) {
      fdpMonitor.io.accuracy.useful_prefetch(w) := ldu(w).io.prefetch_info.fdp.useful_prefetch
    }else {
      fdpMonitor.io.accuracy.useful_prefetch(w) := Mux(same_cycle_update_pf_flag, false.B, ldu(w).io.prefetch_info.fdp.useful_prefetch)
    }
  }
  for (w <- 0 until LoadPipelineWidth)  { fdpMonitor.io.pollution.cache_pollution(w) :=  ldu(w).io.prefetch_info.fdp.pollution }
  for (w <- 0 until LoadPipelineWidth)  { fdpMonitor.io.pollution.demand_miss(w) :=  ldu(w).io.prefetch_info.fdp.demand_miss }
  fdpMonitor.io.debugRolling := io.debugRolling

  //----------------------------------------
  // Bloom Filter
  // bloomFilter.io.set <> missQueue.io.bloom_filter_query.set
  // bloomFilter.io.clr <> missQueue.io.bloom_filter_query.clr
  bloomFilter.io.set <> mainPipe.io.bloom_filter_query.set
  bloomFilter.io.clr <> mainPipe.io.bloom_filter_query.clr

  for (w <- 0 until LoadPipelineWidth)  { bloomFilter.io.query(w) <> ldu(w).io.bloom_filter_query.query }
  for (w <- 0 until LoadPipelineWidth)  { bloomFilter.io.resp(w) <> ldu(w).io.bloom_filter_query.resp }

  for (w <- 0 until LoadPipelineWidth)  { counterFilter.io.ld_in(w) <> ldu(w).io.counter_filter_enq }
  for (w <- 0 until LoadPipelineWidth)  { counterFilter.io.query(w) <> ldu(w).io.counter_filter_query }

  //----------------------------------------
  // replacement algorithm
  val replacer = ReplacementPolicy.fromString(cacheParams.replacer, nWays, nSets)
  val replWayReqs = ldu.map(_.io.replace_way) ++ Seq(mainPipe.io.replace_way) ++ stu.map(_.io.replace_way)

  if (dwpuParam.enCfPred) {
    val victimList = VictimList(nSets)
    replWayReqs.foreach {
      case req =>
        req.way := DontCare
        when(req.set.valid) {
          when(victimList.whether_sa(req.set.bits)) {
            req.way := replacer.way(req.set.bits)
          }.otherwise {
            req.way := req.dmWay
          }
        }
    }
  } else {
    replWayReqs.foreach {
      case req =>
        req.way := DontCare
        when(req.set.valid) {
          req.way := replacer.way(req.set.bits)
        }
    }
  }

  val replAccessReqs = ldu.map(_.io.replace_access) ++ Seq(
    mainPipe.io.replace_access
  ) ++ stu.map(_.io.replace_access)
  val touchWays = Seq.fill(replAccessReqs.size)(Wire(ValidIO(UInt(log2Up(nWays).W))))
  touchWays.zip(replAccessReqs).foreach {
    case (w, req) =>
      w.valid := req.valid
      w.bits := req.bits.way
  }
  val touchSets = replAccessReqs.map(_.bits.set)
  replacer.access(touchSets, touchWays)

  //----------------------------------------
  // assertions
  // dcache should only deal with DRAM addresses
  // import freechips.rocketchip.util._
  // when (bus.a.fire) {
  //   assert(PmemRanges.map(_.cover(bus.a.bits.address)).reduce(_ || _))
  // }
  // when (bus.b.fire) {
  //   assert(PmemRanges.map(_.cover(bus.b.bits.address)).reduce(_ || _))
  // }
  // when (bus.c.fire) {
  //   assert(PmemRanges.map(_.cover(bus.c.bits.address)).reduce(_ || _))
  // }

  //----------------------------------------
  // utility functions
  def block_decoupled[T <: Data](source: DecoupledIO[T], sink: DecoupledIO[T], block_signal: Bool) = {
    sink.valid   := source.valid && !block_signal
    source.ready := sink.ready   && !block_signal
    sink.bits    := source.bits
  }

  //----------------------------------------
  // performance counters
  val num_loads = PopCount(ldu.map(e => e.io.lsu.req.fire))
  XSPerfAccumulate("num_loads", num_loads)

  io.mshrFull := missQueue.io.full

  val perfEvents = (Seq(wb, mainPipe, missQueue, probeQueue) ++ ldu).flatMap(_.getPerfEvents)
  generatePerfEvent()
}

class AMOHelper() extends ExtModule {
  val clock  = IO(Input(Clock()))
  val enable = IO(Input(Bool()))
  val cmd    = IO(Input(UInt(5.W)))
  val addr   = IO(Input(UInt(64.W)))
  val wdata  = IO(Input(UInt(64.W)))
  val mask   = IO(Input(UInt(8.W)))
  val rdata  = IO(Output(UInt(64.W)))
}

class DCacheWrapper()(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false

  val useDcache = coreParams.dcacheParametersOpt.nonEmpty
  val clientNode = if (useDcache) TLIdentityNode() else null
  val dcache = if (useDcache) LazyModule(new DCache()) else null
  if (useDcache) {
    clientNode := dcache.clientNode
  }

  class DCacheWrapperImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) with HasPerfEvents {
    val io = IO(new DCacheIO)
    val perfEvents = if (!useDcache) {
      // a fake dcache which uses dpi-c to access memory, only for debug usage!
      val fake_dcache = Module(new FakeDCache())
      io <> fake_dcache.io
      Seq()
    }
    else {
      io <> dcache.module.io
      dcache.module.getPerfEvents
    }
    generatePerfEvent()
  }

  lazy val module = new DCacheWrapperImp(this)
}