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
import utils.OptionWrapper
import xs.utils.cvl.basic._
import xs.utils.cvl.advanced._

object SVA_AssertEntriesTrans {
  def apply(
    params: IssueBlockParams,
    clock: Clock,
    reset: Reset,
    enqCanTrans2Comp: Option[Bool],
    enqCanTrans2Simp: Option[Bool],
    simpCanTrans2Comp: Option[Bool],
    robPtrVec: Seq[RobPtr],
    validVec: Seq[Bool]
  )(implicit p: Parameters): Unit = {

    def oldestSelTree(robPtrVec: Seq[RobPtr], validVec: Seq[Bool]): (RobPtr, Bool) = {
      require(robPtrVec.length == validVec.length)
      if(robPtrVec.length == 1) {
        (robPtrVec.head, validVec.head)
      } else {
        val half = robPtrVec.length / 2
        val leftOldest = oldestSelTree(robPtrVec.take(half), validVec.take(half))
        val rightOldest = oldestSelTree(robPtrVec.takeRight(half), validVec.takeRight(half))
        
        val (lRobPtr, lValid) = leftOldest
        val (rRobPtr, rValid) = rightOldest
        val robPtr = Wire(new RobPtr)
        val valid = Wire(Bool())
        when(lValid && !rValid) {
          robPtr := lRobPtr
          valid := lValid
        }.elsewhen(!lValid && rValid) {
          robPtr := rRobPtr
          valid := rValid
        }.otherwise {
          robPtr := Mux(lRobPtr < rRobPtr, lRobPtr, rRobPtr)
          valid := Mux(lRobPtr < rRobPtr, lValid, rValid)
        }
        (robPtr, valid)
      }
    }

    def Compare(robPtrLeft: RobPtr, validLeft: Bool, robPtrRight: RobPtr, validRight: Bool): Bool = {
      !(validLeft && validRight && robPtrLeft < robPtrRight)
    }

    val entriesNum = params.numEntries
    val enqEntryNum = params.numEnq
    val othersEntryNum = params.numEntries - params.numEnq
    val simpEnrtyNum = othersEntryNum - params.numComp
    val compEntryNum = params.numComp

    if(params.hasCompAndSimp) {
      // trans strategy:
      // enq->comp
      // enq->simp and simp->comp
      CVL_ASSERT_MUTEX(
        assertEn = true,
        coverEn = false,
        cvlLongSequence = true,
        clock = clock,
        reset = reset,
        name = "Entries_trans_strategy",
        a = enqCanTrans2Comp.get,
        b = enqCanTrans2Simp.get || simpCanTrans2Comp.get
      )
    } else {
      // trans strategy
      // only enq -> comp
    }

    if(params.hasCompAndSimp) {
      val (enqOldest, enqValid) = oldestSelTree(robPtrVec.take(enqEntryNum), validVec.take(enqEntryNum))
      val (simpOldest, simpValid) = oldestSelTree(robPtrVec.drop(enqEntryNum).take(simpEnrtyNum), validVec.drop(enqEntryNum).take(simpEnrtyNum))

      val simpOlderThanEnqVec = Wire(Vec(simpEnrtyNum, Bool()))
      val compOlderThanSimpVec = Wire(Vec(compEntryNum, Bool()))
      val compOlderThanEnqVec = Wire(Vec(compEntryNum, Bool()))

      compOlderThanSimpVec.zip(compOlderThanEnqVec).zipWithIndex.foreach {
        case ((compOlderThanSimp, compOlderThanEnq), idx) => {
          val compRobPtr = robPtrVec.takeRight(compEntryNum)(idx)
          val compValid = validVec.takeRight(compEntryNum)(idx)
          compOlderThanSimp := Compare(compRobPtr, compValid, simpOldest, simpValid)
          compOlderThanEnq := Compare(compRobPtr, compValid, enqOldest, enqValid)
        }
      }

      simpOlderThanEnqVec.zipWithIndex.foreach {
        case (simpOlderThanEnq, idx) => {
          val simpRobPtr = robPtrVec.drop(enqEntryNum).take(simpEnrtyNum)(idx)
          val simpValid = validVec.drop(enqEntryNum).take(simpEnrtyNum)(idx)
          simpOlderThanEnq := Compare(simpRobPtr, simpValid, enqOldest, enqValid)
        }
      }

      CVL_ASSERT_ALWAYS(
        assertEn = true,
        coverEn = false,
        cvlLongSequence = true,
        clock = clock,
        reset = reset,
        name = "comp_older_than_enq",
        test_expr = compOlderThanEnqVec.reduce(_ || _)
      )

      CVL_ASSERT_ALWAYS(
        assertEn = true,
        coverEn = false,
        cvlLongSequence = true,
        clock = clock,
        reset = reset,
        name = "simp_older_than_enq",
        test_expr = simpOlderThanEnqVec.reduce(_ || _)
      )

      CVL_ASSERT_ALWAYS(
        assertEn = true,
        coverEn = false,
        cvlLongSequence = true,
        clock = clock,
        reset = reset,
        name = "comp_older_than_simp",
        test_expr = compOlderThanSimpVec.reduce(_ || _)
      )
    } else {
      val (enqOldest, enqValid) = oldestSelTree(robPtrVec.take(enqEntryNum), validVec.take(enqEntryNum))
      val otherOlderThanEnqVec = Wire(Vec(othersEntryNum, Bool()))

      otherOlderThanEnqVec.zipWithIndex.foreach {
        case (otherOlderThanEnq, idx) => {
          val otherRobPtr = robPtrVec.takeRight(othersEntryNum)(idx)
          val otherValid = validVec.takeRight(othersEntryNum)(idx)
          otherOlderThanEnq := Compare(otherRobPtr, otherValid, enqOldest, enqValid)
        }
      }

      CVL_ASSERT_ALWAYS(
        assertEn = true,
        coverEn = false,
        cvlLongSequence = true,
        clock = clock,
        reset = reset,
        name = "other_older_than_enq",
        test_expr = otherOlderThanEnqVec.reduce(_ || _)
      )
    }
  }
}
