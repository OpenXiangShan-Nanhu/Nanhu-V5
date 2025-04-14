package xiangshan.backend.issue.assertion

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

object AssertVfSplit {
  def apply(
    params: IssueBlockParams,
    assertEn: Boolean,
    coverEn: Boolean,
    clock: Clock,
    disable: Disable,
    iss_lo_before: DecoupledIO[IssueQueueIssueBundle],
    iss_hi_before: DecoupledIO[IssueQueueIssueBundle],
    iss_lo: Valid[IssueQueueIssueBundle],
    iss_hi: Valid[IssueQueueIssueBundle],
  )(implicit p: Parameters): Unit = {
    val vfNeedSplit = BoolSequence(iss_lo_before.valid && !iss_lo_before.bits.common.vpu.get.fpu.isFpToVecInst)
    val issuePort_0_1_signalCopy = BoolSequence(iss_lo.valid && iss_hi.valid &&
                                                iss_lo.bits.common.robIdx === iss_hi.bits.common.robIdx &&
                                                iss_lo.bits.common.vpu.get.vuopIdx === iss_hi.bits.common.vpu.get.vuopIdx)
    val issuePort_0_1_wenDiff = BoolSequence(iss_lo.bits.common.vfWenL.get === true.B && iss_lo.bits.common.vfWenH.get === false.B &&
                                              iss_lo.bits.common.v0WenL.get === true.B && iss_lo.bits.common.v0WenH.get === false.B &&
                                              iss_hi.bits.common.vfWenL.get === false.B && iss_hi.bits.common.vfWenH.get === true.B && 
                                              iss_hi.bits.common.v0WenL.get === false.B && iss_hi.bits.common.v0WenH.get === true.B)
    if(assertEn) {
      AssertProperty(vfNeedSplit |=> issuePort_0_1_signalCopy, clock = Some(clock), disable = Some(disable), label = Some("sva_assert_vfSplitSignalCopy"))
      AssertProperty(vfNeedSplit |=> issuePort_0_1_wenDiff, clock = Some(clock), disable = Some(disable), label = Some("sva_assert_vfSplitWenDiff"))
    }
    if(coverEn) {
      CoverProperty(vfNeedSplit |=> issuePort_0_1_signalCopy, clock = Some(clock), disable = Some(disable), label = Some("sva_cover_vfSplitSignalCopy"))
      CoverProperty(vfNeedSplit |=> issuePort_0_1_wenDiff, clock = Some(clock), disable = Some(disable), label = Some("sva_cover_vfSplitWenDiff"))
    }
  }
}