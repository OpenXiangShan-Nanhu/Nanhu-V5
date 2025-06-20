package xiangshan.backend.issue.assertion.IssueQueue

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
import xs.utils.cvl.basic._

object SVA_AssertIqOldestSel {
  def apply(
    params: IssueBlockParams,
    clock: Clock,
    reset: Reset,
    canIssueVec: Seq[Bool],
    robPtrVec: Seq[RobPtr],
    validVec: Seq[Bool],
    issue_valid: Bool,
    selRobPtr: RobPtr
  )(implicit p: Parameters): Unit = {

    def oldestSelTree(robPtrVec: Seq[RobPtr], canIssueVec: Seq[Bool], validVec: Seq[Bool]): (RobPtr, Bool, Bool) = {
      require(robPtrVec.length == canIssueVec.length && robPtrVec.length == validVec.length)
      if(robPtrVec.length == 1) {
        (robPtrVec.head, canIssueVec.head, validVec.head)
      } else {
        val half = robPtrVec.length / 2
        val leftOldest = oldestSelTree(robPtrVec.take(half), canIssueVec.take(half), validVec.take(half))
        val rightOldest = oldestSelTree(robPtrVec.takeRight(half), canIssueVec.takeRight(half), validVec.takeRight(half))
        
        val (lRobPtr, lCanIssue, lValid) = leftOldest
        val (rRobPtr, rCanIssue, rValid) = rightOldest
        val robPtr = Wire(new RobPtr)
        val canIssue = Wire(Bool())
        val valid = Wire(Bool())
        when((lValid && lCanIssue) && !(rValid && rCanIssue)) {
          robPtr := lRobPtr
          canIssue := lCanIssue
          valid := lValid
        }.elsewhen(!(lValid && lCanIssue) && (rValid && rCanIssue)) {
          robPtr := rRobPtr
          canIssue := rCanIssue
          valid := rValid
        }.otherwise {
          robPtr := Mux(lRobPtr < rRobPtr, lRobPtr, rRobPtr)
          canIssue := Mux(lRobPtr < rRobPtr, lCanIssue, rCanIssue)
          valid := Mux(lRobPtr < rRobPtr, lValid, rValid)
        }
        (robPtr, canIssue, valid)
      }
    }

    val entriesNum = params.numEntries
    val enqEntryNum = params.numEnq
    val othersEntryNum = params.numEntries - params.numEnq

    val (oldestRobPtr, oldestCanIssue, oldestValid) = oldestSelTree(robPtrVec, canIssueVec, validVec)

    if(params.deqFuSame) {
      // 1. if othersEntry has entry that is (canIssue && valid), deqPort0 is oldest
      val othersEntryHasCanIssue = validVec.zip(canIssueVec).map{ case (v, iss) => v && iss }.takeRight(othersEntryNum).reduce(_ || _)
      CVL_ASSERT_IMPLICATION(
        assertEn = true,
        coverEn = false,
        cvlLongSequence = true,
        clock = clock,
        reset = reset,
        name = "deqFuSame_oldest_sel",
        antecedent_expr = othersEntryHasCanIssue,
        consequent_expr = selRobPtr === oldestRobPtr
      )
    } else {
      // 1. deqPort0 must be oldest
      CVL_ASSERT_IMPLICATION(
        assertEn = true,
        coverEn = false,
        cvlLongSequence = true,
        clock = clock,
        reset = reset,
        name = "oldest_sel",
        antecedent_expr = issue_valid,
        consequent_expr = selRobPtr === oldestRobPtr
      )
    }
  }
}
