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

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xs.utils.{Constantin, GatedValidRegNext, GetEvenBits, GetOddBits, PriorityEncoderWithFlag, ValidPseudoLRU, ZeroExt, MaskExpand, ParallelOR}
import xs.utils.perf._
import xiangshan.cache._
import xiangshan.mem._
import xiangshan.backend.Bundles.DynInst
import difftest._
import freechips.rocketchip.util._
import xiangshan.backend.fu.FuType._

class SbufferFlushBundle extends Bundle {
  val valid = Output(Bool())
  val empty = Input(Bool())
}

trait HasSbufferConst extends HasXSParameter {
  val EvictCycles = 1 << 10
  val SbufferReplayDelayCycles = 16
  require(isPow2(EvictCycles))
  val EvictCountBits = log2Up(EvictCycles+1)
  val MissqReplayCountBits = log2Up(SbufferReplayDelayCycles)

  // dcache write hit resp has 2 sources
  // refill pipe resp and main pipe resp (fixed:only main pipe resp)
  // val NumDcacheWriteResp = 2 // hardcoded
  val NumDcacheWriteResp = 2 // hardcoded
  val NumDcacheRefillWritePort = l1BusDataWidth/VLEN

  val SbufferIndexWidth: Int = log2Up(StoreBufferSize)
  // paddr = ptag + offset
  val CacheLineBytes: Int = CacheLineSize / 8
  val CacheLineWords: Int = CacheLineBytes / DataBytes
  val OffsetWidth: Int = log2Up(CacheLineBytes)
  val WordsWidth: Int = log2Up(CacheLineWords)
  val PTagWidth: Int = PAddrBits - OffsetWidth
  val VTagWidth: Int = VAddrBits - OffsetWidth
  val WordOffsetWidth: Int = PAddrBits - WordsWidth

  val CacheLineVWords: Int = CacheLineBytes / VDataBytes
  val VWordsWidth: Int = log2Up(CacheLineVWords)
  val VWordWidth: Int = log2Up(VDataBytes)
  val VWordOffsetWidth: Int = PAddrBits - VWordWidth
}

class SbufferEntryState (implicit p: Parameters) extends SbufferBundle {
  val state_valid    = Bool() // this entry is active
  val state_inflight = Bool() // sbuffer is trying to write this entry to dcache
  val w_timeout = Bool() // with timeout resp, waiting for resend store pipeline req timeout
  val w_sameblock_inflight = Bool() // same cache block dcache req is inflight

  def isInvalid(): Bool = !state_valid
  def isValid(): Bool = state_valid
  def isActive(): Bool = state_valid && !state_inflight
  def isInflight(): Bool = state_inflight
  def isDcacheReqCandidate(): Bool = state_valid && !state_inflight && !w_sameblock_inflight
}

class SbufferBundle(implicit p: Parameters) extends XSBundle with HasSbufferConst

class DataWriteReq(implicit p: Parameters) extends SbufferBundle {
  // univerisal writemask
  val wvec = UInt(StoreBufferSize.W)
  // 2 cycle update
  val mask = UInt((VLEN/8).W)
  val data = UInt(VLEN.W)
  val vwordOffset = UInt(VWordOffsetWidth.W)
  val wline = Bool() // write full cacheline
}

class MaskFlushReq(implicit p: Parameters) extends SbufferBundle {
  // univerisal writemask
  val wvec = UInt(StoreBufferSize.W)
}

class SbufferData(implicit p: Parameters) extends DCacheModule with HasSbufferConst {
  val io = IO(new Bundle(){
    // update data and mask when alloc or merge
    val writeReq = Vec(EnsbufferWidth + NumDcacheRefillWritePort, Flipped(ValidIO(new DataWriteReq)))
    // clean mask when deq
    val maskFlushReq = Vec(NumDcacheWriteResp, Flipped(ValidIO(new MaskFlushReq)))
    val dataOut = Output(Vec(StoreBufferSize, Vec(CacheLineVWords, Vec(VDataBytes, UInt(8.W)))))
    val maskOut = Output(Vec(StoreBufferSize, Vec(CacheLineVWords, Vec(VDataBytes, Bool()))))
  })

  val data = Reg(Vec(StoreBufferSize, Vec(CacheLineVWords, Vec(VDataBytes, UInt(8.W)))))
  // val mask = Reg(Vec(StoreBufferSize, Vec(CacheLineWords, Vec(DataBytes, Bool()))))
  val mask = RegInit(
    VecInit(Seq.fill(StoreBufferSize)(
      VecInit(Seq.fill(CacheLineVWords)(
        VecInit(Seq.fill(VDataBytes)(false.B))
      ))
    ))
  )

  // 2 cycle line mask clean
  for(line <- 0 until StoreBufferSize){
    val line_mask_clean_flag = GatedValidRegNext(
      io.maskFlushReq.map(a => a.valid && a.bits.wvec(line)).reduce(_ || _)
    )
    line_mask_clean_flag.suggestName("line_mask_clean_flag_"+line)
    when(line_mask_clean_flag){
      for(word <- 0 until CacheLineVWords){
        for(byte <- 0 until VDataBytes){
          mask(line)(word)(byte) := false.B
        }
      }
    }
  }

  val req = io.writeReq
  //s1
  val w_valid_s1 = req.map(a => RegNext(a.valid))
  val w_data_s1 = req.map(a => RegEnable(a.bits.data, a.valid))
  val w_wline_s1 = req.map(a => RegEnable(a.bits.wline, a.valid))
  val w_mask_s1 = req.map(a => RegEnable(a.bits.mask, a.valid))
  val w_addr_s1 = req.map(a => RegEnable(OHToUInt(a.bits.wvec), a.valid))
  val w_word_offset_s1 = req.map(a => RegEnable(a.bits.vwordOffset(VWordsWidth - 1, 0), a.valid))

  val w_valid_s1_dup = req.map(a => RegNext(a.valid))
  val w_data_s1_dup  = req.map(a => RegEnable(a.bits.data, a.valid))
  val w_wline_s1_dup = req.map(a => RegEnable(a.bits.wline, a.valid))
  val w_mask_s1_dup  = req.map(a => RegEnable(a.bits.mask, a.valid))
  val w_addr_s1_dup  = req.map(a => RegEnable(OHToUInt(a.bits.wvec), a.valid))
  val w_word_offset_s1_dup = req.map(a => RegEnable(a.bits.vwordOffset(VWordsWidth - 1, 0), a.valid))

  require(StoreBufferSize % 2 == 0)

  for(entry <- 0 until StoreBufferSize / 2){
    w_valid_s1_dup.zipWithIndex.foreach({ case (valid, i) =>
      for (word <- 0 until CacheLineVWords) {
        for (byte <- 0 until VDataBytes) {
          val wen = valid && (
            (w_mask_s1_dup(i)(byte) && (w_word_offset_s1_dup(i) === word.U)) ||
              w_wline_s1_dup(i)
            )
          when(wen){
            when(w_addr_s1_dup(i) === entry.U){
              data(entry)(word)(byte) := w_data_s1_dup(i)(byte * 8 + 7, byte * 8)
              mask(entry)(word)(byte) := true.B
            }
          }
        }
      }
    })
  }

  for (entry <- StoreBufferSize / 2 until StoreBufferSize) {
    w_valid_s1.zipWithIndex.foreach({ case (valid, i) =>
      for (word <- 0 until CacheLineVWords) {
        for (byte <- 0 until VDataBytes) {
          val wen = valid && (
            (w_mask_s1(i)(byte) && (w_word_offset_s1(i) === word.U)) ||
              w_wline_s1(i)
            )
          when(wen) {
            when(w_addr_s1(i) === entry.U) {
              data(entry)(word)(byte) := w_data_s1(i)(byte * 8 + 7, byte * 8)
              mask(entry)(word)(byte) := true.B
            }
          }
        }
      }
    })
  }



  io.dataOut := data
  io.maskOut := mask
}

class Sbuffer(implicit p: Parameters)
  extends DCacheModule
    with HasSbufferConst
    with HasPerfEvents {
  val io = IO(new Bundle() {
    val hartId = Input(UInt(hartIdLen.W))
    val in = Vec(EnsbufferWidth, Flipped(Decoupled(new DCacheWordReqWithVaddrAndPfFlag)))  //Todo: store logic only support Width == 2 now
    val vecDifftestInfo = Vec(EnsbufferWidth, Flipped(Decoupled(new DynInst)))
    val dcache = Flipped(new DCacheToSbufferIO)
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
    val sqempty = Input(Bool())
    val sbempty = Output(Bool())
    val flush = Flipped(new SbufferFlushBundle)
    val csrCtrl = Flipped(new CustomCSRCtrlIO)
    val store_prefetch = Vec(StorePipelineWidth, DecoupledIO(new StorePrefetchReq)) // to dcache
    val memSetPattenDetected = Input(Bool())
    val force_write = Input(Bool())
  })

  val dataModule = Module(new SbufferData)
  dataModule.io.writeReq <> DontCare
//  val prefetcher = Module(new StorePfWrapper())
  val writeReq = dataModule.io.writeReq

  val ptag = Reg(Vec(StoreBufferSize, UInt(PTagWidth.W)))
  val vtag = Reg(Vec(StoreBufferSize, UInt(VTagWidth.W)))
  val debug_mask = Reg(Vec(StoreBufferSize, Vec(CacheLineWords, Vec(DataBytes, Bool()))))
  val waitInflightMask = Reg(Vec(StoreBufferSize, UInt(StoreBufferSize.W)))
  val data = dataModule.io.dataOut
  val mask = dataModule.io.maskOut
  val stateVec = RegInit(VecInit(Seq.fill(StoreBufferSize)(0.U.asTypeOf(new SbufferEntryState))))
  val cohCount = RegInit(VecInit(Seq.fill(StoreBufferSize)(0.U(EvictCountBits.W))))
  val missqReplayCount = RegInit(VecInit(Seq.fill(StoreBufferSize)(0.U(MissqReplayCountBits.W))))

  val sbuffer_out_s0_fire = Wire(Bool())

  /*
       idle --[flush]   --> drain   --[buf empty]--> idle
            --[buf full]--> replace --[dcache resp]--> idle
  */
  // x_drain_all: drain store queue and sbuffer
  // x_drain_sbuffer: drain sbuffer only, block store queue to sbuffer write
  val x_idle :: x_replace :: x_drain_all :: x_drain_sbuffer :: Nil = Enum(4)
  def needDrain(state: UInt): Bool =
    state(1)
  val sbuffer_state = RegInit(x_idle)

  // ---------------------- Store Enq Sbuffer ---------------------

  def getPTag(pa: UInt): UInt =
    pa(PAddrBits - 1, PAddrBits - PTagWidth)

  def getVTag(va: UInt): UInt =
    va(VAddrBits - 1, VAddrBits - VTagWidth)

  def getWord(pa: UInt): UInt =
    pa(PAddrBits-1, 3)

  def getVWord(pa: UInt): UInt =
    pa(PAddrBits-1, 4)

  def getWordOffset(pa: UInt): UInt =
    pa(OffsetWidth-1, 3)

  def getVWordOffset(pa: UInt): UInt =
    pa(OffsetWidth-1, 4)

  def getAddr(ptag: UInt): UInt =
    Cat(ptag, 0.U((PAddrBits - PTagWidth).W))

  def getByteOffset(offect: UInt): UInt =
    Cat(offect(OffsetWidth - 1, 3), 0.U(3.W))

  def isOneOf(key: UInt, seq: Seq[UInt]): Bool =
    if(seq.isEmpty) false.B else Cat(seq.map(_===key)).orR

  def widthMap[T <: Data](f: Int => T) = (0 until StoreBufferSize) map f

  // sbuffer entry count

  val plru = new ValidPseudoLRU(StoreBufferSize)
  val accessIdx = Wire(Vec(EnsbufferWidth + 1, Valid(UInt(SbufferIndexWidth.W))))

  val candidateVec = VecInit(stateVec.map(s => s.isDcacheReqCandidate()))

  val replaceAlgoIdx = plru.way(candidateVec.reverse)._2
  val replaceAlgoNotDcacheCandidate = !stateVec(replaceAlgoIdx).isDcacheReqCandidate()

  assert(!(candidateVec.asUInt.orR && replaceAlgoNotDcacheCandidate), "we have way to select, but replace algo selects invalid way")

  val replaceIdx = replaceAlgoIdx
  plru.access(accessIdx)

  //-------------------------cohCount-----------------------------
  // insert and merge: cohCount=0
  // every cycle cohCount+=1
  // if cohCount(EvictCountBits-1)==1, evict
  val cohTimeOutMask = VecInit(widthMap(i => cohCount(i)(EvictCountBits - 1) && stateVec(i).isActive()))
  val (cohTimeOutIdx, cohHasTimeOut) = PriorityEncoderWithFlag(cohTimeOutMask)
  val cohTimeOutOH = PriorityEncoderOH(cohTimeOutMask)
  val missqReplayTimeOutMask = VecInit(widthMap(i => missqReplayCount(i)(MissqReplayCountBits - 1) && stateVec(i).w_timeout))
  val (missqReplayTimeOutIdxGen, missqReplayHasTimeOutGen) = PriorityEncoderWithFlag(missqReplayTimeOutMask)
  val missqReplayHasTimeOut = GatedValidRegNext(missqReplayHasTimeOutGen) && !GatedValidRegNext(sbuffer_out_s0_fire)
  val missqReplayTimeOutIdx = RegEnable(missqReplayTimeOutIdxGen, missqReplayHasTimeOutGen)

  //-------------------------sbuffer enqueue-----------------------------

  // Now sbuffer enq logic is divided into 3 stages:

  // sbuffer_in_s0:
  // * read data and meta from store queue
  // * store them in 2 entry fifo queue

  // sbuffer_in_s1:
  // * read data and meta from fifo queue
  // * update sbuffer meta (vtag, ptag, flag)
  // * prevert that line from being sent to dcache (add a block condition)
  // * prepare cacheline level write enable signal, RegNext() data and mask

  // sbuffer_in_s2:
  // * use cacheline level buffer to update sbuffer data and mask
  // * remove dcache write block (if there is)

  val activeMask = VecInit(stateVec.map(s => s.isActive()))
  val validMask  = VecInit(stateVec.map(s => s.isValid()))
  val drainIdx = PriorityEncoder(activeMask)

  val inflightMask = VecInit(stateVec.map(s => s.isInflight()))

  val inptags = io.in.map(in => getPTag(in.bits.addr))
  val invtags = io.in.map(in => getVTag(in.bits.vaddr))
  val sameTag = inptags(0) === inptags(1) && io.in(0).valid && io.in(1).valid && io.in(0).bits.vecValid && io.in(1).bits.vecValid
  val firstWord = getVWord(io.in(0).bits.addr)
  val secondWord = getVWord(io.in(1).bits.addr)
  // merge condition
  val mergeMask = Wire(Vec(EnsbufferWidth, Vec(StoreBufferSize, Bool())))
  val mergeIdx = mergeMask.map(PriorityEncoder(_)) // avoid using mergeIdx for better timing
  val canMerge = mergeMask.map(ParallelOR(_))
  val mergeVec = mergeMask.map(_.asUInt)

  for(i <- 0 until EnsbufferWidth){
    mergeMask(i) := widthMap(j =>
      inptags(i) === ptag(j) && activeMask(j)
    )
    assert(!(PopCount(mergeMask(i).asUInt) > 1.U && io.in(i).fire && io.in(i).bits.vecValid))
  }

  // insert condition
  // firstInsert: the first invalid entry
  // if first entry canMerge or second entry has the same ptag with the first entry,
  // secondInsert equal the first invalid entry, otherwise, the second invalid entry
  val invalidMask = VecInit(stateVec.map(s => s.isInvalid()))
  val evenInvalidMask = GetEvenBits(invalidMask.asUInt)
  val oddInvalidMask = GetOddBits(invalidMask.asUInt)

  def getFirstOneOH(input: UInt): UInt = {
    assert(input.getWidth > 1)
    val output = WireInit(VecInit(input.asBools))
    (1 until input.getWidth).map(i => {
      output(i) := !input(i - 1, 0).orR && input(i)
    })
    output.asUInt
  }

  val evenRawInsertVec = getFirstOneOH(evenInvalidMask)
  val oddRawInsertVec = getFirstOneOH(oddInvalidMask)
  val (evenRawInsertIdx, evenCanInsert) = PriorityEncoderWithFlag(evenInvalidMask)
  val (oddRawInsertIdx, oddCanInsert) = PriorityEncoderWithFlag(oddInvalidMask)
  val evenInsertIdx = Cat(evenRawInsertIdx, 0.U(1.W)) // slow to generate, for debug only
  val oddInsertIdx = Cat(oddRawInsertIdx, 1.U(1.W)) // slow to generate, for debug only
  val evenInsertVec = GetEvenBits.reverse(evenRawInsertVec)
  val oddInsertVec = GetOddBits.reverse(oddRawInsertVec)

  val enbufferSelReg = RegInit(false.B)
  when(io.in(0).valid && io.in(0).bits.vecValid) {
    enbufferSelReg := ~enbufferSelReg
  }

  val firstInsertIdx = Mux(enbufferSelReg, evenInsertIdx, oddInsertIdx) // slow to generate, for debug only
  val secondInsertIdx = Mux(sameTag,
    firstInsertIdx,
    Mux(~enbufferSelReg, evenInsertIdx, oddInsertIdx)
  ) // slow to generate, for debug only
  val firstInsertVec = Mux(enbufferSelReg, evenInsertVec, oddInsertVec)
  val secondInsertVec = Mux(sameTag,
    firstInsertVec,
    Mux(~enbufferSelReg, evenInsertVec, oddInsertVec)
  ) // slow to generate, for debug only
  val firstCanInsert = sbuffer_state =/= x_drain_sbuffer && Mux(enbufferSelReg, evenCanInsert, oddCanInsert)
  val secondCanInsert = sbuffer_state =/= x_drain_sbuffer && Mux(sameTag,
    firstCanInsert,
    Mux(~enbufferSelReg, evenCanInsert, oddCanInsert)
  ) && (EnsbufferWidth >= 1).B
  val forward_need_uarch_drain = WireInit(false.B)
  val merge_need_uarch_drain = WireInit(false.B)
  val do_uarch_drain = GatedValidRegNext(forward_need_uarch_drain) || GatedValidRegNext(GatedValidRegNext(merge_need_uarch_drain))
  XSPerfAccumulate("do_uarch_drain", do_uarch_drain)

  io.in(0).ready := firstCanInsert
  io.in(1).ready := secondCanInsert && io.in(0).ready

  io.store_prefetch := DontCare

  def wordReqToBufLine( // allocate a new line in sbuffer
    req: DCacheWordReq,
    reqptag: UInt,
    reqvtag: UInt,
    insertIdx: UInt,
    insertVec: UInt,
    wordOffset: UInt
  ): Unit = {
    assert(UIntToOH(insertIdx) === insertVec)
    val sameBlockInflightMask = genSameBlockInflightMask(reqptag)
    (0 until StoreBufferSize).map(entryIdx => {
      when(insertVec(entryIdx)){
        stateVec(entryIdx).state_valid := true.B
        stateVec(entryIdx).w_sameblock_inflight := sameBlockInflightMask.orR // set w_sameblock_inflight when a line is first allocated
        when(sameBlockInflightMask.orR){
          waitInflightMask(entryIdx) := sameBlockInflightMask
        }
        cohCount(entryIdx) := 0.U
        // missqReplayCount(insertIdx) := 0.U
        ptag(entryIdx) := reqptag
        vtag(entryIdx) := reqvtag // update vtag if a new sbuffer line is allocated
      }
    })
  }

  def mergeWordReq( // merge write req into an existing line
    req: DCacheWordReq,
    reqptag: UInt,
    reqvtag: UInt,
    mergeIdx: UInt,
    mergeVec: UInt,
    wordOffset: UInt
  ): Unit = {
    assert(UIntToOH(mergeIdx) === mergeVec)
    (0 until StoreBufferSize).map(entryIdx => {
      when(mergeVec(entryIdx)) {
        cohCount(entryIdx) := 0.U
        // missqReplayCount(entryIdx) := 0.U
        // check if vtag is the same, if not, trigger sbuffer flush
        when(reqvtag =/= vtag(entryIdx)) {
          XSDebug("reqvtag =/= sbufvtag req(vtag %x ptag %x) sbuffer(vtag %x ptag %x)\n",
            reqvtag << OffsetWidth,
            reqptag << OffsetWidth,
            vtag(entryIdx) << OffsetWidth,
            ptag(entryIdx) << OffsetWidth
          )
          merge_need_uarch_drain := true.B
        }
      }
    })
  }

  for(((in, vwordOffset), i) <- io.in.zip(Seq(firstWord, secondWord)).zipWithIndex){
    writeReq(i).valid := in.fire && in.bits.vecValid
    writeReq(i).bits.vwordOffset := vwordOffset
    writeReq(i).bits.mask := in.bits.mask
    writeReq(i).bits.data := in.bits.data
    writeReq(i).bits.wline := in.bits.wline
    val debug_insertIdx = if(i == 0) firstInsertIdx else secondInsertIdx
    val insertVec = if(i == 0) firstInsertVec else secondInsertVec
    assert(!((PopCount(insertVec) > 1.U) && in.fire && in.bits.vecValid))
    val insertIdx = OHToUInt(insertVec)
    val accessValid = in.fire && in.bits.vecValid
    accessIdx(i).valid := RegNext(accessValid)
    accessIdx(i).bits := RegEnable(Mux(canMerge(i), mergeIdx(i), insertIdx), accessValid)
    when(accessValid){
      when(canMerge(i)){
        writeReq(i).bits.wvec := mergeVec(i)
        mergeWordReq(in.bits, inptags(i), invtags(i), mergeIdx(i), mergeVec(i), vwordOffset)
        XSDebug(p"merge req $i to line [${mergeIdx(i)}]\n")
      }.otherwise({
        writeReq(i).bits.wvec := insertVec
        wordReqToBufLine(in.bits, inptags(i), invtags(i), insertIdx, insertVec, vwordOffset)
        XSDebug(p"insert req $i to line[$insertIdx]\n")
        assert(debug_insertIdx === insertIdx)
      })
    }
  }


  for(i <- 0 until StoreBufferSize){
    XSDebug(stateVec(i).isValid(),
      p"[$i] timeout:${cohCount(i)(EvictCountBits-1)} state:${stateVec(i)}\n"
    )
  }

  for((req, i) <- io.in.zipWithIndex){
    XSDebug(req.fire && req.bits.vecValid,
      p"accept req [$i]: " +
        p"addr:${Hexadecimal(req.bits.addr)} " +
        p"mask:${Binary(shiftMaskToLow(req.bits.addr,req.bits.mask))} " +
        p"data:${Hexadecimal(shiftDataToLow(req.bits.addr,req.bits.data))}\n"
    )
    XSDebug(req.valid && !req.ready,
      p"req [$i] blocked by sbuffer\n"
    )
  }

  // for now, when enq, trigger a prefetch (if EnableAtCommitMissTrigger)
//  require(EnsbufferWidth <= StorePipelineWidth)

  // ---------------------- Send Dcache Req ---------------------

  val sbuffer_empty = Cat(invalidMask).andR
  val sq_empty = !Cat(io.in.map(_.valid)).orR
  val empty = sbuffer_empty && sq_empty
  val threshold = Wire(UInt(5.W)) // RegNext(io.csrCtrl.sbuffer_threshold +& 1.U)
  threshold := Constantin.createRecord(s"StoreBufferThreshold_${p(XSCoreParamsKey).HartId}", initValue = 7)
  val base = Wire(UInt(5.W))
  base := Constantin.createRecord(s"StoreBufferBase_${p(XSCoreParamsKey).HartId}", initValue = 4)
  val ActiveCount = PopCount(activeMask)
  val ValidCount = PopCount(validMask)
  val forceThreshold = Mux(io.force_write, threshold - base, threshold)
  val do_eviction = GatedValidRegNext(ActiveCount >= forceThreshold || ActiveCount === (StoreBufferSize-1).U || ValidCount === (StoreBufferSize).U, init = false.B)
  require((StoreBufferThreshold + 1) <= StoreBufferSize)

  XSDebug(p"ActiveCount[$ActiveCount]\n")

  io.sbempty := GatedValidRegNext(empty)
  io.flush.empty := GatedValidRegNext(empty && io.sqempty)
  // lru.io.flush := sbuffer_state === x_drain_all && empty
  switch(sbuffer_state){
    is(x_idle){
      when(io.flush.valid){
        sbuffer_state := x_drain_all
      }.elsewhen(do_uarch_drain){
        sbuffer_state := x_drain_sbuffer
      }.elsewhen(do_eviction){
        sbuffer_state := x_replace
      }
    }
    is(x_drain_all){
      when(empty){
        sbuffer_state := x_idle
      }
    }
    is(x_drain_sbuffer){
      when(io.flush.valid){
        sbuffer_state := x_drain_all
      }.elsewhen(sbuffer_empty){
        sbuffer_state := x_idle
      }
    }
    is(x_replace){
      when(io.flush.valid){
        sbuffer_state := x_drain_all
      }.elsewhen(do_uarch_drain){
        sbuffer_state := x_drain_sbuffer
      }.elsewhen(!do_eviction){
        sbuffer_state := x_idle
      }
    }
  }
  XSDebug(p"sbuffer state:${sbuffer_state} do eviction:${do_eviction} empty:${empty}\n")

  def noSameBlockInflight(idx: UInt): Bool = {
    // stateVec(idx) itself must not be s_inflight
    !Cat(widthMap(i => inflightMask(i) && ptag(idx) === ptag(i))).orR
  }

  def genSameBlockInflightMask(ptag_in: UInt): UInt = {
    val mask = VecInit(widthMap(i => inflightMask(i) && ptag_in === ptag(i))).asUInt // quite slow, use it with care
    assert(!(PopCount(mask) > 1.U))
    mask
  }

  def haveSameBlockInflight(ptag_in: UInt): Bool = {
    genSameBlockInflightMask(ptag_in).orR
  }

  // ---------------------------------------------------------------------------
  // sbuffer to dcache pipeline
  // ---------------------------------------------------------------------------

  // Now sbuffer deq logic is divided into 2 stages:

  // sbuffer_out_s0:
  // * read data and meta from sbuffer
  // * RegNext() them
  // * set line state to inflight

  // sbuffer_out_s1:
  // * send write req to dcache

  // sbuffer_out_extra:
  // * receive write result from dcache
  // * update line state

  val sbuffer_out_s1_ready = Wire(Bool())

  // ---------------------------------------------------------------------------
  // sbuffer_out_s0
  // ---------------------------------------------------------------------------

  val need_drain = needDrain(sbuffer_state)
  val need_replace = do_eviction || (sbuffer_state === x_replace)
  val sbuffer_out_s0_evictionIdx = Mux(missqReplayHasTimeOut,
    missqReplayTimeOutIdx,
    Mux(need_drain,
      drainIdx,
      Mux(cohHasTimeOut, cohTimeOutIdx, replaceIdx)
    )
  )

  // If there is a inflight dcache req which has same ptag with sbuffer_out_s0_evictionIdx's ptag,
  // current eviction should be blocked.
  val sbuffer_out_s0_valid = missqReplayHasTimeOut ||
    stateVec(sbuffer_out_s0_evictionIdx).isDcacheReqCandidate() &&
    (need_drain || cohHasTimeOut || need_replace)
  assert(!(
    stateVec(sbuffer_out_s0_evictionIdx).isDcacheReqCandidate() &&
    !noSameBlockInflight(sbuffer_out_s0_evictionIdx)
  ))
  val sbuffer_out_s0_cango = sbuffer_out_s1_ready
  sbuffer_out_s0_fire := sbuffer_out_s0_valid && sbuffer_out_s0_cango

  // ---------------------------------------------------------------------------
  // sbuffer_out_s1
  // ---------------------------------------------------------------------------

  // TODO: use EnsbufferWidth
  val shouldWaitWriteFinish = GatedValidRegNext(VecInit((0 until EnsbufferWidth).map{i =>
    (writeReq(i).bits.wvec.asUInt & UIntToOH(sbuffer_out_s0_evictionIdx).asUInt).orR &&
    writeReq(i).valid
  }).asUInt.orR)
  // block dcache write if read / write hazard
  val blockDcacheWrite = shouldWaitWriteFinish

  val sbuffer_out_s1_valid = RegInit(false.B)
  sbuffer_out_s1_ready := io.dcache.req.ready && !blockDcacheWrite || !sbuffer_out_s1_valid
  val sbuffer_out_s1_fire = io.dcache.req.fire

  // when sbuffer_out_s1_fire, send dcache req stored in pipeline reg to dcache
  when(sbuffer_out_s1_fire){
    sbuffer_out_s1_valid := false.B
  }
  // when sbuffer_out_s0_fire, read dcache req data and store them in a pipeline reg
  when(sbuffer_out_s0_cango){
    sbuffer_out_s1_valid := sbuffer_out_s0_valid
  }
  when(sbuffer_out_s0_fire){
    stateVec(sbuffer_out_s0_evictionIdx).state_inflight := true.B
    stateVec(sbuffer_out_s0_evictionIdx).w_timeout := false.B
    // stateVec(sbuffer_out_s0_evictionIdx).s_pipe_req := true.B
    XSDebug(p"$sbuffer_out_s0_evictionIdx will be sent to Dcache\n")
  }

  XSDebug(p"need drain:$need_drain cohHasTimeOut: $cohHasTimeOut need replace:$need_replace\n")
  XSDebug(p"drainIdx:$drainIdx tIdx:$cohTimeOutIdx replIdx:$replaceIdx " +
    p"blocked:${!noSameBlockInflight(sbuffer_out_s0_evictionIdx)} v:${activeMask(sbuffer_out_s0_evictionIdx)}\n")
  XSDebug(p"sbuffer_out_s0_valid:$sbuffer_out_s0_valid evictIdx:$sbuffer_out_s0_evictionIdx dcache ready:${io.dcache.req.ready}\n")
  // Note: if other dcache req in the same block are inflight,
  // the lru update may not accurate
  accessIdx(EnsbufferWidth).valid := invalidMask(replaceIdx) || (
    need_replace && !need_drain && !cohHasTimeOut && !missqReplayHasTimeOut && sbuffer_out_s0_cango && activeMask(replaceIdx))
  accessIdx(EnsbufferWidth).bits := replaceIdx
  val sbuffer_out_s1_evictionIdx = RegEnable(sbuffer_out_s0_evictionIdx, sbuffer_out_s0_fire)
  val sbuffer_out_s1_evictionPTag = RegEnable(ptag(sbuffer_out_s0_evictionIdx), sbuffer_out_s0_fire)
  val sbuffer_out_s1_evictionVTag = RegEnable(vtag(sbuffer_out_s0_evictionIdx), sbuffer_out_s0_fire)

  io.dcache.req.valid := sbuffer_out_s1_valid && !blockDcacheWrite
  io.dcache.req.bits := DontCare
  io.dcache.req.bits.cmd   := MemoryOpConstants.M_XWR
  io.dcache.req.bits.addr  := getAddr(sbuffer_out_s1_evictionPTag)
  io.dcache.req.bits.vaddr := getAddr(sbuffer_out_s1_evictionVTag)
  io.dcache.req.bits.data  := data(sbuffer_out_s1_evictionIdx).asUInt
  io.dcache.req.bits.mask  := mask(sbuffer_out_s1_evictionIdx).asUInt
  io.dcache.req.bits.id := sbuffer_out_s1_evictionIdx

  // when (sbuffer_out_s1_fire) {
  //   assert(!(io.dcache.req.bits.vaddr === 0.U))
  //   assert(!(io.dcache.req.bits.addr === 0.U))
  // }

  XSDebug(sbuffer_out_s1_fire,
    p"send buf [$sbuffer_out_s1_evictionIdx] to Dcache, req fire\n"
  )

  // update sbuffer status according to dcache resp source

  def id_to_sbuffer_id(id: UInt): UInt = {
    require(id.getWidth >= log2Up(StoreBufferSize))
    id(log2Up(StoreBufferSize)-1, 0)
  }

  // hit resp
  io.dcache.hit_resps.map(resp => {
    val dcache_resp_id = resp.bits.id
    when (resp.fire) {
      stateVec(dcache_resp_id).state_inflight := false.B
      stateVec(dcache_resp_id).state_valid := false.B
      assert(!resp.bits.replay)
      assert(!resp.bits.miss) // not need to resp if miss, to be opted
      assert(stateVec(dcache_resp_id).state_inflight === true.B || stateVec(dcache_resp_id).isInvalid())
    }

    // Update w_sameblock_inflight flag is delayed for 1 cycle
    //
    // When a new req allocate a new line in sbuffer, sameblock_inflight check will ignore
    // current dcache.hit_resps. Then, in the next cycle, we have plenty of time to check
    // if the same block is still inflight
    (0 until StoreBufferSize).map(i => {
      when(
        stateVec(i).w_sameblock_inflight &&
        stateVec(i).state_valid &&
        GatedValidRegNext(resp.fire) &&
        waitInflightMask(i) === UIntToOH(RegEnable(id_to_sbuffer_id(dcache_resp_id), resp.fire))
      ){
        stateVec(i).w_sameblock_inflight := false.B
      }
    })
  })

  io.dcache.hit_resps.zip(dataModule.io.maskFlushReq).map{case (resp, maskFlush) => {
    maskFlush.valid := resp.fire
    maskFlush.bits.wvec := UIntToOH(resp.bits.id)
  }}

  // replay resp
  val replay_resp_id = io.dcache.replay_resp.bits.id
  when (io.dcache.replay_resp.fire) {
    missqReplayCount(replay_resp_id) := 0.U
    stateVec(replay_resp_id).w_timeout := true.B
    // waiting for timeout
    assert(io.dcache.replay_resp.bits.replay)
    assert(stateVec(replay_resp_id).state_inflight === true.B)
  }

  //L2 refill row data
  val refill_data = io.dcache.refill_row_data.bits.data
  val refill_id = UIntToOH(io.dcache.refill_row_data.bits.id)
  val store_mask = Mux1H(refill_id, mask)
  (EnsbufferWidth until EnsbufferWidth +  NumDcacheRefillWritePort) map { i =>
    writeReq(i).valid := false.B
    writeReq(i).bits := DontCare
  }


  when(io.dcache.refill_row_data.valid) {
    for (i <- 0 until  NumDcacheRefillWritePort) {
      val wordOffsetFake = ((io.dcache.refill_row_data.bits.refill_count << log2Floor(NumDcacheRefillWritePort)).asUInt + i.U)
      val wordOffset = Mux(io.dcache.refill_row_data.bits.isKeyword, wordOffsetFake ^ NumDcacheRefillWritePort.U, wordOffsetFake)
      val mask = Mux1H(UIntToOH(wordOffset), store_mask).asUInt
      writeReq(EnsbufferWidth + i).valid := !(mask.andR)
      writeReq(EnsbufferWidth + i).bits.wvec := refill_id
      writeReq(EnsbufferWidth + i).bits.mask := ~mask
      writeReq(EnsbufferWidth + i).bits.data := refill_data(VLEN * (i + 1) - 1, VLEN * i)
      writeReq(EnsbufferWidth + i).bits.vwordOffset := wordOffset
      writeReq(EnsbufferWidth + i).bits.wline := false.B
    }
    assert(stateVec(io.dcache.refill_row_data.bits.id).state_inflight === true.B || stateVec(io.dcache.refill_row_data.bits.id).isInvalid())
  }

  // refill data to dcache
  val refill_to_mp_req = io.dcache.refill_to_mp_req.s2_valid
  val refill_to_mp_id = io.dcache.refill_to_mp_req.sbuffer_id

  io.dcache.refill_to_mp_resp.valid := refill_to_mp_req
  io.dcache.refill_to_mp_resp.bits := DontCare
  io.dcache.refill_to_mp_resp.bits.data := data(refill_to_mp_id).asUInt
  io.dcache.refill_to_mp_resp.bits.mask := mask(refill_to_mp_id).asUInt

  // TODO: reuse cohCount
  (0 until StoreBufferSize).map(i => {
    when(stateVec(i).w_timeout && stateVec(i).state_inflight && !missqReplayCount(i)(MissqReplayCountBits-1)) {
      missqReplayCount(i) := missqReplayCount(i) + 1.U
    }
    when(activeMask(i) && !cohTimeOutMask(i)){
      cohCount(i) := cohCount(i)+1.U
    }
  })

  if (env.EnableDifftest) {
    // hit resp
    io.dcache.hit_resps.zipWithIndex.map{case (resp, index) => {
      val difftest = DifftestModule(new DiffSbufferEvent, delay = 1)
      val dcache_resp_id = resp.bits.id
      difftest.coreid := io.hartId
      difftest.index  := index.U
      difftest.valid  := resp.fire && stateVec(dcache_resp_id).isValid()
      difftest.addr   := getAddr(ptag(dcache_resp_id))
      difftest.data   := data(dcache_resp_id).asTypeOf(Vec(CacheLineBytes, UInt(8.W)))
      difftest.mask   := mask(dcache_resp_id).asUInt
    }}
  }

  // ---------------------- Load Data Forward ---------------------
  val mismatch = Wire(Vec(LoadPipelineWidth, Bool()))
  XSPerfAccumulate("vaddr_match_failed", mismatch(0) || mismatch(1))
  for ((forward, i) <- io.forward.zipWithIndex) {
    val vtag_matches = VecInit(widthMap(w => vtag(w) === getVTag(forward.vaddr)))
    // ptag_matches uses paddr from dtlb, which is far from sbuffer
    val forward_ptag =  RegEnable(getPTag(forward.paddr), forward.valid)
    val ptag_matches = VecInit(widthMap(w => ptag(w) === forward_ptag))
    // val ptag_matches = VecInit(widthMap(w => RegEnable(ptag(w), forward.valid) === RegEnable(getPTag(forward.paddr), forward.valid)))
    val tag_matches = vtag_matches
    val tag_mismatch = GatedValidRegNext(forward.valid) && VecInit(widthMap(w =>
      GatedValidRegNext(vtag_matches(w)) =/= ptag_matches(w) && GatedValidRegNext((activeMask(w) || inflightMask(w)))
    )).asUInt.orR
    mismatch(i) := tag_mismatch
    when (tag_mismatch) {
      XSDebug("forward tag mismatch: pmatch %x vmatch %x vaddr %x paddr %x\n",
        RegNext(ptag_matches.asUInt),
        RegNext(vtag_matches.asUInt),
        RegNext(forward.vaddr),
        RegNext(forward.paddr)
      )
      forward_need_uarch_drain := true.B
    }
    val valid_tag_matches = widthMap(w => tag_matches(w) && activeMask(w))
    val inflight_tag_matches = widthMap(w => tag_matches(w) && inflightMask(w))
    val line_offset_mask = UIntToOH(getVWordOffset(forward.paddr))

    val candidate_mask = VecInit(mask.map(entry => entry(getVWordOffset(forward.paddr))))
    val candidate_data = VecInit(data.map(entry => entry(getVWordOffset(forward.paddr))))

    val selectedValidMask = RegEnable(
      Mux1H(valid_tag_matches, candidate_mask), forward.valid
    )
    val selectedValidData = RegEnable(
      Mux1H(valid_tag_matches, candidate_data), forward.valid
    )

    val selectedInflightMask = RegEnable(
      Mux1H(inflight_tag_matches, candidate_mask), forward.valid
    )
    val selectedInflightData = RegEnable(
      Mux1H(inflight_tag_matches, candidate_data), forward.valid
    )

    // currently not being used
    val selectedInflightMaskFast = Mux1H(line_offset_mask, Mux1H(inflight_tag_matches, mask).asTypeOf(Vec(CacheLineVWords, Vec(VDataBytes, Bool()))))
    val selectedValidMaskFast = Mux1H(line_offset_mask, Mux1H(valid_tag_matches, mask).asTypeOf(Vec(CacheLineVWords, Vec(VDataBytes, Bool()))))

    forward.dataInvalid := false.B // data in store line merge buffer is always ready
    forward.matchInvalid := tag_mismatch // paddr / vaddr cam result does not match
    for (j <- 0 until VDataBytes) {
      forward.forwardMask(j) := false.B
      forward.forwardData(j) := DontCare

      // valid entries have higher priority than inflight entries
      when(selectedInflightMask(j)) {
        forward.forwardMask(j) := true.B
        forward.forwardData(j) := selectedInflightData(j)
      }
      when(selectedValidMask(j)) {
        forward.forwardMask(j) := true.B
        forward.forwardData(j) := selectedValidData(j)
      }

      forward.forwardMaskFast(j) := selectedInflightMaskFast(j) || selectedValidMaskFast(j)
    }
    forward.addrInvalid := DontCare
  }

  for (i <- 0 until StoreBufferSize) {
    XSDebug("sbf entry " + i + " : ptag %x vtag %x valid %x active %x inflight %x w_timeout %x\n",
      ptag(i) << OffsetWidth,
      vtag(i) << OffsetWidth,
      stateVec(i).isValid(),
      activeMask(i),
      inflightMask(i),
      stateVec(i).w_timeout
    )
  }
  
  /*
  *
  **********************************************************
  *      -------------                   -------------     *
  *      | XiangShan |                   |    NEMU   |     *
  *      -------------                   -------------     *
  *            |                               |           *
  *            V                               V           *
  *          -----                           -----         *
  *          | Q |                           | Q |         *
  *          | U |                           | U |         *
  *          | E |                           | E |         *
  *          | U |                           | U |         *
  *          | E |                           | E |         *
  *          |   |                           |   |         *
  *          -----                           -----         *
  *            |                               |           *
  *            |        --------------         |           *
  *            |>>>>>>>>|  DIFFTEST  |<<<<<<<<<|           *
  *                     --------------                     *
  **********************************************************
  */
  // Initialize when unenabled difftest.
  for (i <- 0 until EnsbufferWidth) {
    io.vecDifftestInfo(i) := DontCare
  }
  if (env.EnableDifftest) {
    val VecMemFLOWMaxNumber = 16
    val WlineMaxNumber = blockWords

    def UIntSlice(in: UInt, High: UInt, Low: UInt): UInt = {
      val maxNum = in.getWidth
      val result = Wire(Vec(maxNum, Bool()))

      for (i <- 0 until maxNum) {
        when (Low + i.U <= High) {
          result(i) := in(Low + i.U)
        }.otherwise{
          result(i) := 0.U
        }
      }

      result.asUInt
    }

    // To align with 'nemu', we need:
    //  For 'unit-store' and 'whole' vector store instr, we re-split here,
    //  and for the res, we do nothing.
    for (i <- 0 until EnsbufferWidth) {
      io.vecDifftestInfo(i).ready := io.in(i).ready

      val uop             = io.vecDifftestInfo(i).bits

      val isVse           = isVStore(uop.fuType) && LSUOpType.isUStride(uop.fuOpType)
      val isVsm           = isVStore(uop.fuType) && VstuType.isMasked(uop.fuOpType)
      val isVsr           = isVStore(uop.fuType) && VstuType.isWhole(uop.fuOpType)

      val vpu             = uop.vpu
      val veew            = uop.vpu.veew
      val eew             = EewLog2(veew)
      val EEB             = (1.U << eew).asUInt //Only when VLEN=128 effective element byte
      val EEWBits         = (EEB << 3.U).asUInt
      val nf              = Mux(isVsr, 0.U, vpu.nf)

      val isSegment       = nf =/= 0.U && !isVsm
      val isVSLine        = (isVse || isVsm || isVsr) && !isSegment
      val isWline         = io.in(i).bits.wline

      // The number of stores generated by a uop theroy.
      // No other vector instructions need to be considered.
      val flow            = Mux(
                              isVSLine,
                              (16.U >> eew).asUInt,
                              0.U
                            )

      val rawData         = io.in(i).bits.data
      val rawMask         = io.in(i).bits.mask
      val rawAddr         = io.in(i).bits.addr

      // A common difftest interface for scalar and vector instr
      val difftestCommon = DifftestModule(new DiffStoreEvent, delay = 2, dontCare = true)
      when (isVSLine) {
        val splitMask         = UIntSlice(rawMask, EEB - 1.U, 0.U)(7,0)  // Byte
        val splitData         = UIntSlice(rawData, EEWBits - 1.U, 0.U)(63,0) // Double word
        val storeCommit       = io.in(i).fire && splitMask.orR && io.in(i).bits.vecValid
        val waddr             = rawAddr
        val wmask             = splitMask
        val wdata             = splitData & MaskExpand(splitMask)

        difftestCommon.coreid := io.hartId
        difftestCommon.index  := (i*VecMemFLOWMaxNumber).U
        difftestCommon.valid  := storeCommit
        difftestCommon.addr   := waddr
        difftestCommon.data   := wdata
        difftestCommon.mask   := wmask

      } .elsewhen (!isWline) {
        val storeCommit       = io.in(i).fire
        val waddr             = ZeroExt(Cat(io.in(i).bits.addr(PAddrBits - 1, 3), 0.U(3.W)), 64)
        val sbufferMask       = shiftMaskToLow(io.in(i).bits.addr, io.in(i).bits.mask)
        val sbufferData       = shiftDataToLow(io.in(i).bits.addr, io.in(i).bits.data)
        val wmask             = sbufferMask
        val wdata             = sbufferData & MaskExpand(sbufferMask)

        difftestCommon.coreid := io.hartId
        difftestCommon.index  := (i*VecMemFLOWMaxNumber).U
        difftestCommon.valid  := storeCommit && io.in(i).bits.vecValid
        difftestCommon.addr   := waddr
        difftestCommon.data   := wdata
        difftestCommon.mask   := wmask

      }

      for (index <- 0 until WlineMaxNumber) {
        val difftest = DifftestModule(new DiffStoreEvent, delay = 2, dontCare = true)

        val storeCommit = io.in(i).fire && io.in(i).bits.vecValid
        val blockAddr = get_block_addr(io.in(i).bits.addr)

        when (isWline) {
          difftest.coreid := io.hartId
          difftest.index  := (i*VecMemFLOWMaxNumber + index).U
          difftest.valid  := storeCommit
          difftest.addr   := blockAddr + (index.U << wordOffBits)
          difftest.data   := io.in(i).bits.data
          difftest.mask   := ((1 << wordBytes) - 1).U
          
          assert(!storeCommit || (io.in(i).bits.data === 0.U), "wline only supports whole zero write now")
        }
      }

      // Only the interface used by the 'unit-store' and 'whole' vector store instr
      for (index <- 1 until VecMemFLOWMaxNumber) {
        val difftest = DifftestModule(new DiffStoreEvent, delay = 2, dontCare = true)

        // I've already done something process with 'mask' outside:
        //  Different cases of 'vm' have been considered:
        //    Any valid store will definitely not have all 0 masks,
        //    and the extra part due to unaligned access must have a mask of 0
        when (index.U < flow && isVSLine) {
          // Make NEMU-difftest happy
          val shiftIndex  = EEB*index.U
          val shiftFlag   = shiftIndex(2,0).orR // Double word Flag
          val shiftBytes  = Mux(shiftFlag, shiftIndex(2,0), 0.U)
          val shiftBits   = shiftBytes << 3.U
          val splitMask   = UIntSlice(rawMask, (EEB*(index+1).U - 1.U), EEB*index.U)(7,0)  // Byte
          val splitData   = UIntSlice(rawData, (EEWBits*(index+1).U - 1.U), EEWBits*index.U)(63,0) // Double word
          val storeCommit = io.in(i).fire && splitMask.orR  && io.in(i).bits.vecValid
          val waddr       = Cat(rawAddr(PAddrBits - 1, 4), Cat(shiftIndex(3), 0.U(3.W)))
          val wmask       = splitMask << shiftBytes
          val wdata       = (splitData & MaskExpand(splitMask)) << shiftBits

          difftest.coreid := io.hartId
          difftest.index  := (i*VecMemFLOWMaxNumber+index).U
          difftest.valid  := storeCommit
          difftest.addr   := waddr
          difftest.data   := wdata
          difftest.mask   := wmask

        }
      }
    }
  }

  val perf_valid_entry_count = RegNext(PopCount(VecInit(stateVec.map(s => !s.isInvalid())).asUInt))
  XSPerfHistogram("util", perf_valid_entry_count, true.B, 0, StoreBufferSize, 1)
  XSPerfAccumulate("sbuffer_req_valid", PopCount(VecInit(io.in.map(_.valid)).asUInt))
  XSPerfAccumulate("sbuffer_req_fire", PopCount(VecInit(io.in.map(_.fire)).asUInt))
  XSPerfAccumulate("sbuffer_req_fire_vecinvalid", PopCount(VecInit(io.in.map(data => data.fire && !data.bits.vecValid)).asUInt))
  XSPerfAccumulate("sbuffer_merge", PopCount(VecInit(io.in.zipWithIndex.map({case (in, i) => in.fire && canMerge(i)})).asUInt))
  XSPerfAccumulate("sbuffer_newline", PopCount(VecInit(io.in.zipWithIndex.map({case (in, i) => in.fire && !canMerge(i)})).asUInt))
  XSPerfAccumulate("dcache_req_valid", io.dcache.req.valid)
  XSPerfAccumulate("dcache_req_fire", io.dcache.req.fire)
  XSPerfAccumulate("sbuffer_idle", sbuffer_state === x_idle)
  XSPerfAccumulate("sbuffer_flush", sbuffer_state === x_drain_sbuffer)
  XSPerfAccumulate("sbuffer_replace", sbuffer_state === x_replace)
  XSPerfAccumulate("evenCanInsert", evenCanInsert)
  XSPerfAccumulate("oddCanInsert", oddCanInsert)
  XSPerfAccumulate("mainpipe_resp_valid", io.dcache.main_pipe_hit_resp.fire)
  //XSPerfAccumulate("refill_resp_valid", io.dcache.refill_hit_resp.fire)
  XSPerfAccumulate("replay_resp_valid", io.dcache.replay_resp.fire)
  XSPerfAccumulate("coh_timeout", cohHasTimeOut)

  // val (store_latency_sample, store_latency) = TransactionLatencyCounter(io.lsu.req.fire, io.lsu.resp.fire)
  // XSPerfHistogram("store_latency", store_latency, store_latency_sample, 0, 100, 10)
  // XSPerfAccumulate("store_req", io.lsu.req.fire)

  val perfEvents = Seq(
    ("sbuffer_req_valid ", PopCount(VecInit(io.in.map(_.valid)).asUInt)                                                                ),
    ("sbuffer_req_fire  ", PopCount(VecInit(io.in.map(_.fire)).asUInt)                                                               ),
    ("sbuffer_merge     ", PopCount(VecInit(io.in.zipWithIndex.map({case (in, i) => in.fire && canMerge(i)})).asUInt)                ),
    ("sbuffer_newline   ", PopCount(VecInit(io.in.zipWithIndex.map({case (in, i) => in.fire && !canMerge(i)})).asUInt)               ),
    ("dcache_req_valid  ", io.dcache.req.valid                                                                                         ),
    ("dcache_req_fire   ", io.dcache.req.fire                                                                                        ),
    ("sbuffer_idle      ", sbuffer_state === x_idle                                                                                    ),
    ("sbuffer_flush     ", sbuffer_state === x_drain_sbuffer                                                                           ),
    ("sbuffer_replace   ", sbuffer_state === x_replace                                                                                 ),
    ("mpipe_resp_valid  ", io.dcache.main_pipe_hit_resp.fire                                                                         ),
    //("refill_resp_valid ", io.dcache.refill_hit_resp.fire                                                                            ),
    ("replay_resp_valid ", io.dcache.replay_resp.fire                                                                                ),
    ("coh_timeout       ", cohHasTimeOut                                                                                               ),
    ("sbuffer_1_4_valid ", (perf_valid_entry_count < (StoreBufferSize.U/4.U))                                                          ),
    ("sbuffer_2_4_valid ", (perf_valid_entry_count > (StoreBufferSize.U/4.U)) & (perf_valid_entry_count <= (StoreBufferSize.U/2.U))    ),
    ("sbuffer_3_4_valid ", (perf_valid_entry_count > (StoreBufferSize.U/2.U)) & (perf_valid_entry_count <= (StoreBufferSize.U*3.U/4.U))),
    ("sbuffer_full_valid", (perf_valid_entry_count > (StoreBufferSize.U*3.U/4.U)))
  )
  generatePerfEvent()

}
