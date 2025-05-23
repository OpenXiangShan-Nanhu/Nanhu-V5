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

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import xs.utils._
import xs.utils.perf._
import xiangshan._
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.Bundles._
import xiangshan.mem._
import xiangshan.backend.fu.{FuType, PMPRespBundle}
import freechips.rocketchip.diplomacy.BufferParams
import xiangshan.cache.mmu._
import xiangshan.cache._
import xiangshan.cache.wpu.ReplayCarry
import xiangshan.backend.fu.util.SdtrigExt
import xiangshan.ExceptionNO._
import xiangshan.backend.fu.vector.Bundles.{VConfig, VType}
import xiangshan.backend.datapath.NewPipelineConnect
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.fu.vector.Utils.VecDataToMaskDataVec

class VSegmentBundle(implicit p: Parameters) extends VLSUBundle
{
  val baseVaddr        = UInt(XLEN.W)
  val uop              = new DynInst
  val paddr            = UInt(PAddrBits.W)
  val mask             = UInt(VLEN.W)
  val alignedType      = UInt(alignTypeBits.W)
  val vl               = UInt(elemIdxBits.W)
  val uopFlowNum       = UInt(elemIdxBits.W)
  val uopFlowNumMask   = UInt(elemIdxBits.W)
  // for exception
  val vstart           = UInt(elemIdxBits.W)
  val exceptionVaddr   = UInt(XLEN.W)
  val exceptionGpaddr  = UInt(XLEN.W)
  val exceptionIsForVSnonLeafPTE = Bool()
  val exception_va     = Bool()
  val exception_gpa    = Bool()
  val exception_pa     = Bool()
  val exceptionVstart  = UInt(elemIdxBits.W)
  // valid: have fof exception but can not trigger, need update all writebacked uop.vl with exceptionVl
  val exceptionVl      = ValidIO(UInt(elemIdxBits.W))
  val isFof            = Bool()
}

// latch each uop's VecWen, pdest, v0Wen, uopIdx
class VSegmentUop(implicit p: Parameters) extends VLSUBundle{
  val uop              = new DynInst
}

class VSegmentUnit (implicit p: Parameters) extends VLSUModule
  with HasDCacheParameters
  with MemoryOpConstants
  with SdtrigExt
  with HasLoadHelper
{
  val io               = IO(new VSegmentUnitIO)

//  val maxSize          = VSegmentBufferSize
//
//  class VSegUPtr(implicit p: Parameters) extends CircularQueuePtr[VSegUPtr](maxSize){
//  }
//
//  object VSegUPtr {
//    def apply(f: Bool, v: UInt)(implicit p: Parameters): VSegUPtr = {
//      val ptr           = Wire(new VSegUPtr)
//      ptr.flag         := f
//      ptr.value        := v
//      ptr
//    }
//  }
//
//
//  /**
//  ********************************************************************************************************
//  *  Use an example to illustrate the working logic of a segmentunit:                                    *
//  *    For:                                                                                              *
//  *      lmul=2 sew=32 emul=2 eew=32  vl=16                                                              *
//  *    Then:                                                                                             *
//  *      Access memory in the order:                                                                     *
//  *        (V2,S0),(V4,S0),(V6,S0),(V8,S0),                                                              *
//  *        (V2,S1),(V4,S1),(V6,S1),(V8,S1),                                                              *
//  *        (V2,S2),(V4,S2),(V6,S2),(V8,S2),                                                              *
//  *        (V2,S3),(V4,S3),(V6,S3),(V8,S3),                                                              *
//  *        (V3,S4),(V5,S4),(V7,S4),(V9,S4),                                                              *
//  *        (V3,S5),(V5,S5),(V7,S5),(V9,S5),                                                              *
//  *        (V3,S6),(V5,S6),(V7,S6),(V9,S6),                                                              *
//  *        (V3,S7),(V5,S7),(V7,S7),(V9,S7),                                                              *
//  *                                                                                                      *
//  *                                                                                                      *
//  *    [[data]] saves the data generated by the access and corresponds to the register.                  *
//  *    [[splitPtr]] controls the destination register written to.                                        *
//  *                                                                                                      *
//  *    splitptr offset can be seen in [[splitPtrNext]] is assignment logic,                              *
//  *    which is mainly calculated in terms of [[fieldIdx]] and [[segmentIdx]]                            *
//  *    First access different fields of the same segment, and then visit different segments.             *
//  *    For the case of 'emul' greater than 1, such as the following example,                             *
//  *    although 'v2' and 'v3' are different vd and the same field, they are still different segments,    *
//  *    so they should be accessed sequentially.Just like the 'Access memory in the order' above.         *
//  *                                                                                                      *
//  *                         [[segmentIdx]]                                                               *
//  *                               |                                                                      *
//  *                               |                                                                      *
//  *                               V                                                                      *
//  *                                                                                                      *
//  *                               S0               S1                S2                 S3               *
//  *                      ----------------------------------------------------------------------------    *
//  *  [[splitPtr]]--> v2  |     field0     |      field0     |      field0     |      field0         |    *
//  *                      ----------------------------------------------------------------------------    *
//  *                               S4               S5                S6                 S7               *
//  *                      ----------------------------------------------------------------------------    *
//  *                  v3  |     field0     |      field0     |      field0     |      field0         |    *
//  *                      ----------------------------------------------------------------------------    *
//  *                               S0               S1                S2                 S3               *
//  *                      ----------------------------------------------------------------------------    *
//  *                  v4  |     field1     |      field1     |      field1     |      field1         |    *
//  *                      ----------------------------------------------------------------------------    *
//  *                               S4               S5                S6                 S7               *
//  *                      ----------------------------------------------------------------------------    *
//  *                  v5  |     field1     |      field1     |      field1     |      field1         |    *
//  *                      ----------------------------------------------------------------------------    *
//  *                               S0               S1                S2                 S3               *
//  *                      ----------------------------------------------------------------------------    *
//  *                  v6  |     field2     |      field2     |      field2     |      field2         |    *
//  *                      ----------------------------------------------------------------------------    *
//  *                               S4               S5                S6                 S7               *
//  *                      ----------------------------------------------------------------------------    *
//  *                  v7  |     field2     |      field2     |      field2     |      field2         |    *
//  *                      ----------------------------------------------------------------------------    *
//  *                               S0               S1                S2                 S3               *
//  *                      ----------------------------------------------------------------------------    *
//  *                  v8  |     field3     |      field3     |      field3     |      field3         |    *
//  *                      ----------------------------------------------------------------------------    *
//  *                               S4               S5                S6                 S7               *
//  *                      ----------------------------------------------------------------------------    *
//  *                  v9  |     field3     |      field3     |      field3     |      field3         |    *
//  *                      ----------------------------------------------------------------------------    *                                                                                    *
//  *                                                                                                      *                                                                                    *
//  *                                                                                                      *                                                                                    *
//  ********************************************************************************************************
//  **/
//
//
//  // buffer uop
//  val instMicroOp       = Reg(new VSegmentBundle)
//  val instMicroOpValid  = RegInit(false.B)
//  val data              = Reg(Vec(maxSize, UInt(VLEN.W)))
//  val uopq              = Reg(Vec(maxSize, new VSegmentUop))
//  val stride            = Reg(Vec(maxSize, UInt(VLEN.W)))
//  val allocated         = RegInit(VecInit(Seq.fill(maxSize)(false.B)))
//  val enqPtr            = RegInit(0.U.asTypeOf(new VSegUPtr))
//  val deqPtr            = RegInit(0.U.asTypeOf(new VSegUPtr))
//  val stridePtr         = WireInit(0.U.asTypeOf(new VSegUPtr)) // for select stride/index
//
//  val segmentIdx        = RegInit(0.U(elemIdxBits.W))
//  val fieldIdx          = RegInit(0.U(fieldBits.W))
//  val segmentOffset     = RegInit(0.U(XLEN.W))
//  val splitPtr          = RegInit(0.U.asTypeOf(new VSegUPtr)) // for select load/store data
//  val splitPtrNext      = WireInit(0.U.asTypeOf(new VSegUPtr))
//
//  val exception_va      = WireInit(false.B)
//  val exception_gpa     = WireInit(false.B)
//  val exception_pa      = WireInit(false.B)
//
//  val maxSegIdx         = instMicroOp.vl - 1.U
//  val maxNfields        = instMicroOp.uop.vpu.nf
//  val latchVaddr        = RegInit(0.U(VAddrBits.W))
//
//  XSError((segmentIdx > maxSegIdx) && instMicroOpValid, s"segmentIdx > vl, something error!\n")
//  XSError((fieldIdx > maxNfields) &&  instMicroOpValid, s"fieldIdx > nfields, something error!\n")
//
//  // MicroOp
//  val baseVaddr                       = instMicroOp.baseVaddr
//  val alignedType                     = instMicroOp.alignedType
//  val fuType                          = instMicroOp.uop.fuType
//  val mask                            = instMicroOp.mask
//  val exceptionVec                    = instMicroOp.uop.exceptionVec
//  val issueEew                        = instMicroOp.uop.vpu.veew
//  val issueLmul                       = instMicroOp.uop.vpu.vtype.vlmul
//  val issueSew                        = instMicroOp.uop.vpu.vtype.vsew
//  val issueEmul                       = EewLog2(issueEew) - issueSew + issueLmul
//  val elemIdxInVd                     = segmentIdx & instMicroOp.uopFlowNumMask
//  val issueInstType                   = Cat(true.B, instMicroOp.uop.fuOpType(6, 5)) // always segment instruction
//  val issueUopFlowNumLog2             = GenRealFlowLog2(issueInstType, issueEmul, issueLmul, issueEew, issueSew, true) // max element number log2 in vd
//  val issueVlMax                      = instMicroOp.uopFlowNum // max elementIdx in vd
//  val issueMaxIdxInIndex              = GenVLMAX(Mux(issueEmul.asSInt > 0.S, 0.U, issueEmul), issueEew(1, 0)) // index element index in index register
//  val issueMaxIdxInIndexMask          = GenVlMaxMask(issueMaxIdxInIndex, elemIdxBits)
//  val issueMaxIdxInIndexLog2          = GenVLMAXLog2(Mux(issueEmul.asSInt > 0.S, 0.U, issueEmul), issueEew(1, 0))
//  val issueIndexIdx                   = segmentIdx & issueMaxIdxInIndexMask
//  val segmentActive                   = (mask & UIntToOH(segmentIdx)).orR
//
//  // sbuffer write interface
//  val sbufferOut                      = Wire(Decoupled(new DCacheWordReqWithVaddrAndPfFlag))
//
//
//  // segment fof instrction buffer
//  val fofBuffer                       = RegInit(0.U.asTypeOf(new DynInst))
//  val fofBufferValid                  = RegInit(false.B)
//
//
//  // Segment instruction's FSM
//  /*
//  * s_idle: wait request
//  * s_flush_sbuffer_req: flush sbuffer
//  * s_wait_flush_sbuffer_resp: wait sbuffer empty
//  * s_tlb_req: request tlb
//  * s_wait_tlb_resp: wait tlb resp
//  * s_pm: check pmp
//  * s_cache_req: request cache
//  * s_cache_resp: wait cache resp
//  * s_latch_and_merge_data: for read data
//  * s_send_data: for send write data
//  * s_wait_to_sbuffer: Wait for data from the sbufferOut pipelayer to be sent to the sbuffer
//  * s_finish:
//  * s_fof_fix_vl: Writeback the uop of the fof instruction to modify vl.
//  * */
//  val s_idle :: s_flush_sbuffer_req :: s_wait_flush_sbuffer_resp :: s_tlb_req :: s_wait_tlb_resp :: s_pm ::s_cache_req :: s_cache_resp :: s_latch_and_merge_data :: s_send_data :: s_wait_to_sbuffer :: s_finish :: s_fof_fix_vl :: Nil = Enum(13)
//  val state             = RegInit(s_idle)
//  val stateNext         = WireInit(s_idle)
//  val sbufferEmpty      = io.flush_sbuffer.empty
//  val isVSegLoad        = FuType.isVSegLoad(instMicroOp.uop.fuType)
//  val isEnqfof          = io.in.bits.uop.fuOpType === VlduType.vleff && io.in.valid
//  val isEnqFixVlUop     = isEnqfof && io.in.bits.uop.vpu.lastUop
//
//  /**
//   * state update
//   */
//  state  := stateNext
//
//  /**
//   * state transfer
//   */
//  when(state === s_idle){
//    stateNext := Mux(isAfter(enqPtr, deqPtr), s_flush_sbuffer_req, s_idle)
//  }.elsewhen(state === s_flush_sbuffer_req){
//    stateNext := Mux(sbufferEmpty, s_tlb_req, s_wait_flush_sbuffer_resp) // if sbuffer is empty, go to query tlb
//
//  }.elsewhen(state === s_wait_flush_sbuffer_resp){
//    stateNext := Mux(sbufferEmpty, s_tlb_req, s_wait_flush_sbuffer_resp)
//
//  }.elsewhen(state === s_tlb_req){
//    stateNext := Mux(segmentActive, s_wait_tlb_resp, Mux(isVSegLoad, s_latch_and_merge_data, s_send_data))
//
//  }.elsewhen(state === s_wait_tlb_resp){
//    stateNext := Mux(io.dtlb.resp.fire,
//                      Mux(!io.dtlb.resp.bits.miss,
//                          s_pm,
//                          s_tlb_req),
//                      s_wait_tlb_resp)
//
//  }.elsewhen(state === s_pm){
//    /* if is vStore, send data to sbuffer, so don't need query dcache */
//    stateNext := Mux(exception_pa || exception_va || exception_gpa,
//                     s_finish,
//                     Mux(isVSegLoad, s_cache_req, s_send_data))
//
//  }.elsewhen(state === s_cache_req){
//    stateNext := Mux(io.rdcache.req.fire, s_cache_resp, s_cache_req)
//
//  }.elsewhen(state === s_cache_resp){
//    when(io.rdcache.resp.fire) {
//      when(io.rdcache.resp.bits.miss || io.rdcache.s2_bank_conflict) {
//        stateNext := s_cache_req
//      }.otherwise {
//        stateNext := Mux(isVSegLoad, s_latch_and_merge_data, s_send_data)
//      }
//    }.otherwise{
//      stateNext := s_cache_resp
//    }
//    /* if segment is inactive, don't need to wait access all of the field */
//  }.elsewhen(state === s_latch_and_merge_data) {
//    when((segmentIdx === maxSegIdx) && (fieldIdx === maxNfields) ||
//      ((segmentIdx === maxSegIdx) && !segmentActive)) {
//
//      stateNext := s_finish // segment instruction finish
//    }.otherwise {
//      stateNext := s_tlb_req // need continue
//    }
//    /* if segment is inactive, don't need to wait access all of the field */
//  }.elsewhen(state === s_send_data) { // when sbuffer accept data
//    when(!sbufferOut.fire && segmentActive) {
//      stateNext := s_send_data
//    }.elsewhen(segmentIdx === maxSegIdx && (fieldIdx === maxNfields && sbufferOut.fire || !segmentActive && io.sbuffer.valid && !io.sbuffer.ready)) {
//      stateNext := s_wait_to_sbuffer
//    }.elsewhen(segmentIdx === maxSegIdx && !segmentActive){
//      stateNext := s_finish // segment instruction finish
//    }.otherwise {
//      stateNext := s_tlb_req // need continue
//    }
//
//  }.elsewhen(state === s_wait_to_sbuffer){
//    stateNext := Mux(io.sbuffer.fire, s_finish, s_wait_to_sbuffer)
//
//  }.elsewhen(state === s_finish){ // writeback uop
//    stateNext := Mux(
//      distanceBetween(enqPtr, deqPtr) === 0.U,
//      Mux(fofBufferValid, s_fof_fix_vl, s_idle),
//      s_finish
//    )
//  }.elsewhen(state === s_fof_fix_vl){ // writeback uop
//    stateNext := Mux(!fofBufferValid, s_idle, s_fof_fix_vl)
//
//  }.otherwise{
//    stateNext := s_idle
//    XSError(true.B, s"Unknown state!\n")
//  }
//
//  /*************************************************************************
//   *                            enqueue logic
//   *************************************************************************/
//  io.in.ready                         := true.B
//  val fuOpType                         = io.in.bits.uop.fuOpType
//  val vtype                            = io.in.bits.uop.vpu.vtype
//  val mop                              = fuOpType(6, 5)
//  val instType                         = Cat(true.B, mop)
//  val eew                              = io.in.bits.uop.vpu.veew
//  val sew                              = vtype.vsew
//  val lmul                             = vtype.vlmul
//  val emul                             = EewLog2(eew) - sew + lmul
//  val vl                               = instMicroOp.vl
//  val vm                               = instMicroOp.uop.vpu.vm
//  val vstart                           = instMicroOp.uop.vpu.vstart
//  val srcMask                          = GenFlowMask(Mux(vm, Fill(VLEN, 1.U(1.W)), io.in.bits.src_mask), vstart, vl, true)
//  // first uop enqueue, we need to latch microOp of segment instruction
//  when(io.in.fire && !instMicroOpValid && !isEnqFixVlUop){
//    // element number in a vd
//    // TODO Rewrite it in a more elegant way.
//    val uopFlowNum                    = ZeroExt(GenRealFlowNum(instType, emul, lmul, eew, sew, true), elemIdxBits)
//    instMicroOp.baseVaddr             := io.in.bits.src_rs1
//    instMicroOpValid                  := true.B // if is first uop
//    instMicroOp.alignedType           := Mux(isIndexed(instType), sew(1, 0), eew)
//    instMicroOp.uop                   := io.in.bits.uop
//    instMicroOp.mask                  := srcMask
//    instMicroOp.vstart                := 0.U
//    instMicroOp.uopFlowNum            := uopFlowNum
//    instMicroOp.uopFlowNumMask        := GenVlMaxMask(uopFlowNum, elemIdxBits) // for merge data
//    instMicroOp.vl                    := io.in.bits.src_vl.asTypeOf(VConfig()).vl
//    instMicroOp.exceptionVl.valid     := false.B
//    instMicroOp.exceptionVl.bits      := io.in.bits.src_vl.asTypeOf(VConfig()).vl
//    segmentOffset                     := 0.U
//    instMicroOp.isFof                 := (fuOpType === VlduType.vleff) && FuType.isVLoad(fuType)
//  }
//  // latch data
//  when(io.in.fire && !isEnqFixVlUop){
//    data(enqPtr.value)                := io.in.bits.src_vs3
//    stride(enqPtr.value)              := io.in.bits.src_stride
//    uopq(enqPtr.value).uop            := io.in.bits.uop
//  }
//
//  // update enqptr, only 1 port
//  when(io.in.fire && !isEnqFixVlUop){
//    enqPtr                            := enqPtr + 1.U
//  }
//
//  /*************************************************************************
//   *                            output logic
//   *************************************************************************/
//
//  val indexStride                     = IndexAddr( // index for indexed instruction
//                                                    index = stride(stridePtr.value),
//                                                    flow_inner_idx = issueIndexIdx,
//                                                    eew = issueEew
//                                                  )
//  val realSegmentOffset               = Mux(isIndexed(issueInstType),
//                                            indexStride,
//                                            segmentOffset)
//  val vaddr                           = baseVaddr + (fieldIdx << alignedType).asUInt + realSegmentOffset
//
//  //latch vaddr
//  when(state === s_tlb_req){
//    latchVaddr := vaddr(VAddrBits - 1, 0)
//  }
//  /**
//   * tlb req and tlb resq
//   */
//
//  // query DTLB IO Assign
//  io.dtlb.req                         := DontCare
//  io.dtlb.resp.ready                  := true.B
//  io.dtlb.req.valid                   := state === s_tlb_req && segmentActive
//  io.dtlb.req.bits.cmd                := Mux(FuType.isVLoad(fuType), TlbCmd.read, TlbCmd.write)
//  io.dtlb.req.bits.vaddr              := vaddr(VAddrBits - 1, 0)
//  io.dtlb.req.bits.fullva             := vaddr
//  io.dtlb.req.bits.checkfullva        := true.B
//  io.dtlb.req.bits.size               := instMicroOp.alignedType(2,0)
//  io.dtlb.req.bits.memidx.is_ld       := FuType.isVLoad(fuType)
//  io.dtlb.req.bits.memidx.is_st       := FuType.isVStore(fuType)
//  io.dtlb.req.bits.debug.robIdx       := instMicroOp.uop.robIdx
//  io.dtlb.req.bits.no_translate       := false.B
//  io.dtlb.req.bits.debug.pc           := instMicroOp.uop.pc
//  io.dtlb.req.bits.debug.isFirstIssue := DontCare
//  io.dtlb.req_kill                    := false.B
//
//  val canTriggerException              = segmentIdx === 0.U || !instMicroOp.isFof // only elementIdx = 0 or is not fof can trigger
//
//  val segmentTrigger = Module(new VSegmentTrigger)
//  segmentTrigger.io.fromCsrTrigger.tdataVec             := io.fromCsrTrigger.tdataVec
//  segmentTrigger.io.fromCsrTrigger.tEnableVec           := io.fromCsrTrigger.tEnableVec
//  segmentTrigger.io.fromCsrTrigger.triggerCanRaiseBpExp := io.fromCsrTrigger.triggerCanRaiseBpExp
//  segmentTrigger.io.fromCsrTrigger.debugMode            := io.fromCsrTrigger.debugMode
//  segmentTrigger.io.memType                             := isVSegLoad
//  segmentTrigger.io.fromLoadStore.vaddr                 := latchVaddr
//  segmentTrigger.io.fromLoadStore.isVectorUnitStride    := false.B
//  segmentTrigger.io.fromLoadStore.mask                  := 0.U
//
//  val triggerAction = segmentTrigger.io.toLoadStore.triggerAction
//  val triggerDebugMode = TriggerAction.isDmode(triggerAction)
//  val triggerBreakpoint = TriggerAction.isExp(triggerAction)
//
//  // tlb resp
//  when(io.dtlb.resp.fire && state === s_wait_tlb_resp){
//      exceptionVec(storePageFault)      := io.dtlb.resp.bits.excp(0).pf.st
//      exceptionVec(loadPageFault)       := io.dtlb.resp.bits.excp(0).pf.ld
//      exceptionVec(storeGuestPageFault) := io.dtlb.resp.bits.excp(0).gpf.st
//      exceptionVec(loadGuestPageFault)  := io.dtlb.resp.bits.excp(0).gpf.ld
//      exceptionVec(storeAccessFault)    := io.dtlb.resp.bits.excp(0).af.st
//      exceptionVec(loadAccessFault)     := io.dtlb.resp.bits.excp(0).af.ld
//      when(!io.dtlb.resp.bits.miss){
//        instMicroOp.paddr             := io.dtlb.resp.bits.paddr(0)
//        instMicroOp.exceptionGpaddr   := io.dtlb.resp.bits.gpaddr(0)
//        instMicroOp.exceptionIsForVSnonLeafPTE  := io.dtlb.resp.bits.isForVSnonLeafPTE
//      }
//  }
//  // pmp
//  // NOTE: only handle load/store exception here, if other exception happens, don't send here
//  val exceptionWithPf = exceptionVec(storePageFault) || exceptionVec(loadPageFault) || exceptionVec(storeGuestPageFault) || exceptionVec(loadGuestPageFault)
//  val pmp = (io.pmpResp.asUInt & Fill(io.pmpResp.asUInt.getWidth, !exceptionWithPf)).asTypeOf(new PMPRespBundle())
//  when(state === s_pm) {
//    val addr_aligned = LookupTree(Mux(isIndexed(issueInstType), issueSew(1, 0), issueEew(1, 0)), List(
//      "b00".U   -> true.B,                   //b
//      "b01".U   -> (vaddr(0)    === 0.U), //h
//      "b10".U   -> (vaddr(1, 0) === 0.U), //w
//      "b11".U   -> (vaddr(2, 0) === 0.U)  //d
//    ))
//    val missAligned = !addr_aligned
//    exceptionVec(loadAddrMisaligned)  := missAligned && FuType.isVSegLoad(fuType)  && canTriggerException
//    exceptionVec(storeAddrMisaligned) := missAligned && FuType.isVSegStore(fuType) && canTriggerException
//
//    exception_va  := exceptionVec(storePageFault) || exceptionVec(loadPageFault) ||
//                     exceptionVec(storeAccessFault) || exceptionVec(loadAccessFault) ||
//                     exceptionVec(breakPoint) || triggerDebugMode || missAligned
//    exception_gpa := exceptionVec(storeGuestPageFault) || exceptionVec(loadGuestPageFault)
//    exception_pa  := pmp.st || pmp.ld || pmp.mmio
//
//    instMicroOp.exception_pa  := exception_pa
//    instMicroOp.exception_va  := exception_va
//    instMicroOp.exception_gpa := exception_gpa
//    // update storeAccessFault bit. Currently, we don't support vector MMIO
//    exceptionVec(loadAccessFault)  := (exceptionVec(loadAccessFault) || pmp.ld || pmp.mmio)   && FuType.isVSegLoad(fuType)  && canTriggerException
//    exceptionVec(storeAccessFault) := (exceptionVec(storeAccessFault) || pmp.st || pmp.mmio)  && FuType.isVSegStore(fuType) && canTriggerException
//    exceptionVec(breakPoint)       := triggerBreakpoint && canTriggerException
//
//    exceptionVec(storePageFault)      := exceptionVec(storePageFault)      && FuType.isVSegStore(fuType) && canTriggerException
//    exceptionVec(loadPageFault)       := exceptionVec(loadPageFault)       && FuType.isVSegLoad(fuType)  && canTriggerException
//    exceptionVec(storeGuestPageFault) := exceptionVec(storeGuestPageFault) && FuType.isVSegStore(fuType) && canTriggerException
//    exceptionVec(loadGuestPageFault)  := exceptionVec(loadGuestPageFault)  && FuType.isVSegLoad(fuType)  && canTriggerException
//
//    when(exception_va || exception_gpa || exception_pa) {
//      when(canTriggerException) {
//        instMicroOp.exceptionVaddr  := vaddr
//        instMicroOp.exceptionVstart := segmentIdx // for exception
//      }.otherwise {
//        instMicroOp.exceptionVl.valid := true.B
//        instMicroOp.exceptionVl.bits := segmentIdx
//      }
//    }
//
//    when(exceptionVec(breakPoint) || triggerDebugMode) {
//      instMicroOp.uop.trigger := triggerAction
//    }
//  }
//
//  /**
//   * flush sbuffer IO Assign
//   */
//  io.flush_sbuffer.valid           := !sbufferEmpty && (state === s_flush_sbuffer_req)
//
//
//  /**
//   * merge data for load
//   */
//  val cacheData = LookupTree(latchVaddr(3,0), List(
//    "b0000".U -> io.rdcache.resp.bits.data_delayed(63,    0),
//    "b0001".U -> io.rdcache.resp.bits.data_delayed(63,    8),
//    "b0010".U -> io.rdcache.resp.bits.data_delayed(63,   16),
//    "b0011".U -> io.rdcache.resp.bits.data_delayed(63,   24),
//    "b0100".U -> io.rdcache.resp.bits.data_delayed(63,   32),
//    "b0101".U -> io.rdcache.resp.bits.data_delayed(63,   40),
//    "b0110".U -> io.rdcache.resp.bits.data_delayed(63,   48),
//    "b0111".U -> io.rdcache.resp.bits.data_delayed(63,   56),
//    "b1000".U -> io.rdcache.resp.bits.data_delayed(127,  64),
//    "b1001".U -> io.rdcache.resp.bits.data_delayed(127,  72),
//    "b1010".U -> io.rdcache.resp.bits.data_delayed(127,  80),
//    "b1011".U -> io.rdcache.resp.bits.data_delayed(127,  88),
//    "b1100".U -> io.rdcache.resp.bits.data_delayed(127,  96),
//    "b1101".U -> io.rdcache.resp.bits.data_delayed(127, 104),
//    "b1110".U -> io.rdcache.resp.bits.data_delayed(127, 112),
//    "b1111".U -> io.rdcache.resp.bits.data_delayed(127, 120)
//  ))
//  val pickData  = rdataVecHelper(alignedType(1,0), cacheData)
//  val mergedData = mergeDataWithElemIdx(
//    oldData = data(splitPtr.value),
//    newData = Seq(pickData),
//    alignedType = alignedType(1,0),
//    elemIdx = Seq(elemIdxInVd),
//    valids = Seq(true.B)
//  )
//  when(state === s_latch_and_merge_data && segmentActive){
//    data(splitPtr.value) := mergedData
//  }
//  /**
//   * split data for store
//   * */
//  val splitData = genVSData(
//    data = data(splitPtr.value),
//    elemIdx = elemIdxInVd,
//    alignedType = alignedType
//  )
//  val flowData  = genVWdata(splitData, alignedType) // TODO: connect vstd, pass vector data
//  val wmask     = genVWmask(latchVaddr, alignedType(1, 0)) & Fill(VLENB, segmentActive)
//
//  /**
//   * rdcache req, write request don't need to query dcache, because we write element to sbuffer
//   */
//  io.rdcache.req                    := DontCare
//  io.rdcache.req.valid              := state === s_cache_req && FuType.isVLoad(fuType)
//  io.rdcache.req.bits.cmd           := MemoryOpConstants.M_XRD
//  io.rdcache.req.bits.vaddr         := latchVaddr
//  io.rdcache.req.bits.mask          := mask
//  io.rdcache.req.bits.data          := flowData
//  io.rdcache.pf_source              := LOAD_SOURCE.U
//  io.rdcache.req.bits.id            := DontCare
//  io.rdcache.resp.ready             := true.B
//  io.rdcache.s1_paddr_dup_lsu       := instMicroOp.paddr
//  io.rdcache.s1_paddr_dup_dcache    := instMicroOp.paddr
//  io.rdcache.s1_kill                := false.B
//  io.rdcache.s1_kill_data_read      := false.B
//  io.rdcache.s2_kill                := false.B
//  if (env.FPGAPlatform){
//    io.rdcache.s0_pc                := DontCare
//    io.rdcache.s1_pc                := DontCare
//    io.rdcache.s2_pc                := DontCare
//  }else{
//    io.rdcache.s0_pc                := instMicroOp.uop.pc
//    io.rdcache.s1_pc                := instMicroOp.uop.pc
//    io.rdcache.s2_pc                := instMicroOp.uop.pc
//  }
//  io.rdcache.replacementUpdated     := false.B
//  io.rdcache.is128Req               := false.B
//
//
//  /**
//   * write data to sbuffer
//   * */
//  sbufferOut.bits                  := DontCare
//  sbufferOut.valid                 := state === s_send_data && segmentActive
//  sbufferOut.bits.vecValid         := state === s_send_data && segmentActive
//  sbufferOut.bits.mask             := wmask
//  sbufferOut.bits.data             := flowData
//  sbufferOut.bits.vaddr            := latchVaddr
//  sbufferOut.bits.cmd              := MemoryOpConstants.M_XWR
//  sbufferOut.bits.id               := DontCare
//  sbufferOut.bits.addr             := instMicroOp.paddr
//
//  NewPipelineConnect(
//    sbufferOut, io.sbuffer, io.sbuffer.fire,
//    false.B,
//    Option(s"VSegmentUnitPipelineConnect")
//  )
//
//  io.vecDifftestInfo.valid         := io.sbuffer.valid
//  io.vecDifftestInfo.bits          := uopq(deqPtr.value).uop
//
//  /**
//   * update ptr
//   * */
//  private val fieldActiveWirteFinish = sbufferOut.fire && segmentActive // writedata finish and is a active segment
//  XSError(sbufferOut.fire && !segmentActive, "Attempt write inactive segment to sbuffer, something wrong!\n")
//
//  private val segmentInactiveFinish = ((state === s_latch_and_merge_data) || (state === s_send_data)) && !segmentActive
//
//  val splitPtrOffset = Mux(
//    isIndexed(instType),
//    Mux(lmul.asSInt < 0.S, 1.U, (1.U << lmul).asUInt),
//    Mux(emul.asSInt < 0.S, 1.U, (1.U << emul).asUInt)
//  )
//  splitPtrNext :=
//    Mux(fieldIdx === maxNfields || !segmentActive, // if segment is active, need to complete this segment, otherwise jump to next segment
//      // segment finish, By shifting 'issueUopFlowNumLog2' to the right to ensure that emul != 1 can correctly generate lateral offset.
//     (deqPtr + ((segmentIdx +& 1.U) >> issueUopFlowNumLog2).asUInt),
//      // next field.
//     (splitPtr + splitPtrOffset)
//    )
//
//  dontTouch(issueUopFlowNumLog2)
//  dontTouch(issueEmul)
//  dontTouch(splitPtrNext)
//  dontTouch(stridePtr)
//  dontTouch(segmentActive)
//
//  // update splitPtr
//  when(state === s_latch_and_merge_data || (state === s_send_data && (fieldActiveWirteFinish || !segmentActive))){
//    splitPtr := splitPtrNext
//  }.elsewhen(io.in.fire && !instMicroOpValid){
//    splitPtr := deqPtr // initial splitPtr
//  }
//
//  // update stridePtr, only use in index
//  val strideOffset = Mux(isIndexed(issueInstType), segmentIdx >> issueMaxIdxInIndexLog2, 0.U)
//  stridePtr       := deqPtr + strideOffset
//
//  // update fieldIdx
//  when(io.in.fire && !instMicroOpValid){ // init
//    fieldIdx := 0.U
//  }.elsewhen(state === s_latch_and_merge_data && segmentActive ||
//            (state === s_send_data && fieldActiveWirteFinish)){ // only if segment is active
//
//    /* next segment, only if segment complete */
//    fieldIdx := Mux(fieldIdx === maxNfields, 0.U, fieldIdx + 1.U)
//  }.elsewhen(segmentInactiveFinish){ // segment is inactive, go to next segment
//    fieldIdx := 0.U
//  }
//  //update segmentIdx
//  when(io.in.fire && !instMicroOpValid){
//    segmentIdx := 0.U
//  }.elsewhen(fieldIdx === maxNfields && (state === s_latch_and_merge_data || (state === s_send_data && fieldActiveWirteFinish)) &&
//             segmentIdx =/= maxSegIdx){ // next segment, only if segment is active
//
//    segmentIdx := segmentIdx + 1.U
//  }.elsewhen(segmentInactiveFinish && segmentIdx =/= maxSegIdx){ // if segment is inactive, go to next segment
//    segmentIdx := segmentIdx + 1.U
//  }
//
//  //update segmentOffset
//  /* when segment is active or segment is inactive, increase segmentOffset */
//  when((fieldIdx === maxNfields && (state === s_latch_and_merge_data || (state === s_send_data && fieldActiveWirteFinish))) ||
//       segmentInactiveFinish){
//
//    segmentOffset := segmentOffset + Mux(isUnitStride(issueInstType), (maxNfields +& 1.U) << issueEew(1, 0), stride(stridePtr.value))
//  }
//
//  //update deqPtr
//  when((state === s_finish) && !isEmpty(enqPtr, deqPtr)){
//    deqPtr := deqPtr + 1.U
//  }
//
//
//  /*************************************************************************
//   *                            fof logic
//   *************************************************************************/
//
//  //Enq
//  when(isEnqFixVlUop && !fofBufferValid) { fofBuffer := io.in.bits.uop }
//  when(isEnqFixVlUop && !fofBufferValid) { fofBufferValid := true.B }
//
//  //Deq
//  val fofFixVlValid                    = state === s_fof_fix_vl && fofBufferValid
//
//  when(fofFixVlValid) { fofBuffer      := 0.U.asTypeOf(new DynInst) }
//  when(fofFixVlValid) { fofBufferValid := false.B }
//
//
//  /*************************************************************************
//   *                            dequeue logic
//   *************************************************************************/
//  val vdIdxInField = GenUopIdxInField(Mux(isIndexed(instType), issueLmul, issueEmul), uopq(deqPtr.value).uop.vpu.vuopIdx)
//  /*select mask of vd, maybe remove in feature*/
//  val realEw        = Mux(isIndexed(issueInstType), issueSew(1, 0), issueEew(1, 0))
//  val maskDataVec: Vec[UInt] = VecDataToMaskDataVec(instMicroOp.mask, realEw)
//  val maskUsed      = maskDataVec(vdIdxInField)
//
//  when(stateNext === s_idle){
//    instMicroOpValid := false.B
//  }
//  // writeback to backend
//  val writebackOut                     = WireInit(io.uopwriteback.bits)
//  val writebackValid                   = (state === s_finish) && !isEmpty(enqPtr, deqPtr) || fofFixVlValid
//
//  when(fofFixVlValid) {
//    writebackOut.uop                    := fofBuffer
//    writebackOut.uop.vpu.vl             := instMicroOp.exceptionVl.bits
//    writebackOut.data                   := instMicroOp.exceptionVl.bits
//    writebackOut.mask.get               := Fill(VLEN, 1.U)
//    writebackOut.uop.vpu.vmask          := Fill(VLEN, 1.U)
//  }.otherwise{
//    writebackOut.uop                    := uopq(deqPtr.value).uop
//    writebackOut.uop.vpu                := instMicroOp.uop.vpu
//    writebackOut.uop.exceptionVec       := instMicroOp.uop.exceptionVec
//    writebackOut.mask.get               := instMicroOp.mask
//    writebackOut.data                   := data(deqPtr.value)
//    writebackOut.vdIdx.get              := vdIdxInField
//    writebackOut.uop.vpu.vl             := Mux(instMicroOp.exceptionVl.valid, instMicroOp.exceptionVl.bits, instMicroOp.vl)
//    writebackOut.uop.vpu.vstart         := Mux(instMicroOp.uop.exceptionVec.asUInt.orR, instMicroOp.exceptionVstart, instMicroOp.vstart)
//    writebackOut.uop.vpu.vmask          := maskUsed
//    writebackOut.uop.vpu.vuopIdx        := uopq(deqPtr.value).uop.vpu.vuopIdx
//    // when exception updates vl, should use vtu strategy.
//    writebackOut.uop.vpu.vta            := Mux(instMicroOp.exceptionVl.valid, VType.tu, instMicroOp.uop.vpu.vta)
//    writebackOut.debug                  := DontCare
//    writebackOut.vdIdxInField.get       := vdIdxInField
//    writebackOut.uop.robIdx             := instMicroOp.uop.robIdx
//    writebackOut.uop.fuOpType           := instMicroOp.uop.fuOpType
//  }
//
//  io.uopwriteback.valid               := RegNext(writebackValid)
//  io.uopwriteback.bits                := RegEnable(writebackOut, writebackValid)
//
//  dontTouch(writebackValid)
//
//  //to RS
//  val feedbackOut                      = WireInit(0.U.asTypeOf(io.feedback.bits))
//  val feedbackValid                    = state === s_finish && !isEmpty(enqPtr, deqPtr)
//  feedbackOut.hit                     := true.B
//  feedbackOut.robIdx                  := instMicroOp.uop.robIdx
//  feedbackOut.sourceType              := DontCare
//  feedbackOut.flushState              := DontCare
//  feedbackOut.dataInvalidSqIdx        := DontCare
//  feedbackOut.sqIdx                   := uopq(deqPtr.value).uop.sqIdx
//  feedbackOut.lqIdx                   := uopq(deqPtr.value).uop.lqIdx
//
//  io.feedback.valid                   := RegNext(feedbackValid)
//  io.feedback.bits                    := RegEnable(feedbackOut, feedbackValid)
//
//  dontTouch(feedbackValid)
//
//  // exception
//  io.exceptionInfo                    := DontCare
//  io.exceptionInfo.bits.robidx        := instMicroOp.uop.robIdx
//  io.exceptionInfo.bits.uopidx        := uopq(deqPtr.value).uop.vpu.vuopIdx
//  io.exceptionInfo.bits.vstart        := instMicroOp.exceptionVstart
//  io.exceptionInfo.bits.vaddr         := instMicroOp.exceptionVaddr
//  io.exceptionInfo.bits.gpaddr        := instMicroOp.exceptionGpaddr
//  io.exceptionInfo.bits.isForVSnonLeafPTE := instMicroOp.exceptionIsForVSnonLeafPTE
//  io.exceptionInfo.bits.vl            := instMicroOp.exceptionVl.bits
//  io.exceptionInfo.valid              := (state === s_finish) && instMicroOp.uop.exceptionVec.asUInt.orR && !isEmpty(enqPtr, deqPtr)

  io <> DontCare
}

