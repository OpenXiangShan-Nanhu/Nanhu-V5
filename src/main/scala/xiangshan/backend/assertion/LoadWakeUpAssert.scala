package xiangshan.backend.assertion

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.issue.IssueBlockParams
import chisel3.ltl.{Sequence, AssertProperty, CoverProperty}
import chisel3.ltl.Sequence.BoolSequence
import chisel3.ltl.Delay
import xs.utils.DelayN

object AssertLoadWakeUp {
  def apply(
    assertEn: Boolean,
    coverEn: Boolean,
    cvlLongSequence: Boolean,
    clock: Clock,
    reset: Reset,
    redirect: Valid[Redirect],
    wakeup: Valid[DynInst],
    cancel: LoadCancelIO,
    wb: DecoupledIO[MemExuOutput]
  )(implicit p: Parameters): Unit = {
    if(cvlLongSequence) {
      val wakeUpBeFlush = Wire(Vec(3, Bool()))
      for(i <- 0 until 3) {
        wakeUpBeFlush(i) := DelayN(wakeup.valid, i) && DelayN(wakeup.bits.robIdx, i).needFlush(redirect)
      }
      dontTouch(wakeUpBeFlush)

      val redirectDelay = RegNext(redirect)
      val wakeUpNotFlushLast = BoolSequence(!(wakeup.valid && wakeup.bits.robIdx.needFlush(redirectDelay)))

      val wakeUpPdestDelay3 = DelayN(wakeup.bits.pdest, 3)
      val wakeUp = BoolSequence(wakeup.valid)
      val wakeUpWb = BoolSequence(wb.bits.uop.pdest === wakeUpPdestDelay3)
      val wakeUpCancel = BoolSequence(cancel.ld2Cancel)

      val noflush_0 = BoolSequence(!wakeUpBeFlush(0))
      val noflush_1 = BoolSequence(!wakeUpBeFlush(1))
      val noflush_2 = BoolSequence(!wakeUpBeFlush(2))
      val flush_2 = BoolSequence(wakeUpBeFlush(2))

      val flushOrCancelOrWb = flush_2 or wakeUpCancel or (noflush_2 ### wakeUpWb)
      val wakeUpPass = (wakeUpNotFlushLast and wakeUp and noflush_0) ### noflush_1 |=> flushOrCancelOrWb
      
      if(assertEn) {
        AssertProperty(wakeUpPass, clock = Some(clock), disable = Some(reset.asDisable), label = Some("sva_assert_wakeUpPass"))
      }
      if(coverEn) {
        CoverProperty(wakeUpPass, clock = Some(clock), disable = Some(reset.asDisable), label = Some("sva_cover_wakeUpPass"))
      }
    } else {
      val wakeUpPdestDelay3 = DelayN(wakeup.bits.pdest, 3)
      val redirectDelay = RegNext(redirect)
      val wakeUpNotFlushLast = !(wakeup.valid && wakeup.bits.robIdx.needFlush(redirectDelay))
      val wkup = Wire(Valid(UInt(wakeup.bits.robIdx.value.getWidth.W)))
      wkup.valid := wakeup.valid && wakeUpNotFlushLast
      wkup.bits := wakeup.bits.robIdx.value
      /* 
        1. s0 s1 s2: redirect - 3 port
        2. s2: ldcancel       - 1 port
        3. s3: wb             - 1 port
      */
      val ackVec = Wire(Vec(5, Valid(UInt(wakeup.bits.robIdx.value.getWidth.W))))
      for(i <- 0 until 3) {
        ackVec(i).valid := DelayN(wakeup.valid, i) && DelayN(wakeup.bits.robIdx, i).needFlush(redirect)
        ackVec(i).bits := DelayN(wakeup.bits.robIdx.value, i)
      }
      ackVec(3).valid := cancel.ld2Cancel
      ackVec(3).bits := DelayN(wakeup.bits.robIdx.value, 3)
      ackVec(4).valid := wb.valid && (wb.bits.uop.pdest === wakeUpPdestDelay3)
      ackVec(4).bits := DelayN(wakeup.bits.robIdx.value, 3)

      import xs.utils.cvl.advanced.CVL_ASSERT_REQ_ACK_MULTI_ACK
      CVL_ASSERT_REQ_ACK_MULTI_ACK(
        assertEn = assertEn,
        coverEn = coverEn,
        cvlLongSequence = false,
        clock = clock,
        reset = reset,
        name = "LoadWakeUp",
        min_time = 0,
        max_time = 3,
        req = wkup,
        ackWidth = 4,
        ack = ackVec,
      )

      import xs.utils.cvl.advanced.CVL_ASSERT_MUTEX
      CVL_ASSERT_MUTEX(
        assertEn = assertEn,
        coverEn = coverEn,
        cvlLongSequence = false,
        clock = clock,
        reset = reset,
        name = "LoadWakeUpMutex",
        a = ackVec(3).valid && ackVec(4).valid,
        b = ackVec(3).bits === ackVec(4).bits
      )
    }
  }  
}