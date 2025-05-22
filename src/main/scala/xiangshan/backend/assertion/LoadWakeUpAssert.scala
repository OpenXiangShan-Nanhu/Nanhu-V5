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
    clock: Clock,
    disable: Disable,
    redirect: Valid[Redirect],
    wakeup: Valid[DynInst],
    cancel: LoadCancelIO,
    wb: DecoupledIO[MemExuOutput]
  )(implicit p: Parameters): Unit = {
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
      AssertProperty(wakeUpPass, clock = Some(clock), disable = Some(disable), label = Some("sva_assert_wakeUpPass"))
    }
    if(coverEn) {
      CoverProperty(wakeUpPass, clock = Some(clock), disable = Some(disable), label = Some("sva_cover_wakeUpPass"))
    }
  }
}