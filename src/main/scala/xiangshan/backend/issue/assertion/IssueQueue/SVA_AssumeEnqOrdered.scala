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
import chisel3.ltl._

object SVA_AssumeEnqOrdered {
  def apply(
    params: IssueBlockParams,
    clock: Clock,
    reset: Reset,
    enqRobPtrVec: Seq[RobPtr],
    enqValidVec: Seq[Bool],
    robPtrVec: Seq[RobPtr],
    validVec: Seq[Bool]
  )(implicit p: Parameters): Unit = {

    val entriesNum = params.numEntries
    val enqEntryNum = params.numEnq

    require(entriesNum == robPtrVec.length)
    require(entriesNum == validVec.length)
    require(enqEntryNum == enqRobPtrVec.length)
    require(enqEntryNum == enqValidVec.length)

    val entriesRobPtrOlderThanEnqVec = Wire(Vec(enqEntryNum, Vec(entriesNum, Bool())))
    entriesRobPtrOlderThanEnqVec.zipWithIndex.foreach {
      case (robPtrOlderThanEnq, idx) => {
        robPtrOlderThanEnq.zipWithIndex.foreach {
          case (older, j) => {
            older := !validVec(j) || robPtrVec(j) <= enqRobPtrVec(idx)
          }
        }
      }
    }

    val enqIsYoungest = entriesRobPtrOlderThanEnqVec.map(_.asUInt)

    val lastCycleEnqValid = RegNext(VecInit(enqValidVec))
    val lastCycleEnqRobPtr = RegNext(VecInit(enqRobPtrVec))

    val intDqDepth = p(XSCoreParamsKey).dpParams.IntDqSize
    val vfDqDepth = p(XSCoreParamsKey).dpParams.FpDqSize
    val lsDqDepth = p(XSCoreParamsKey).dpParams.LsDqSize

    val dqSize = intDqDepth + vfDqDepth + lsDqDepth

    val enqValid = BoolSequence(!enqValidVec.reduce(_ || _))
    val rst = BoolSequence(reset.asBool)

    for(i <- 0 until enqEntryNum) {
      AssumeProperty(
        cond = (enqValidVec(i) === false.B || enqValidVec(i) === true.B),
        clock = clock,
        disable = reset.asDisable,
        label = "SVA_ASSUME_enq_valid"
      )
      // AssumeProperty(
      //   prop = rst |-> enqValid.delayRange(0, 10),
      //   clock = Some(clock),
      //   disable = Some(reset.asDisable),
      //   label = Some("SVA_ASSUME_enq_rob_ptr")
      // )
    }

    for(i <- 0 until enqEntryNum) {
      val enqValid = BoolSequence(enqValidVec(i))
      val enqPtrYoungerThanLasyCycle = BoolSequence(lastCycleEnqRobPtr.map{case ptr => ptr < enqRobPtrVec(i)}.reduce(_ && _))
      AssumeProperty(
        cond = enqIsYoungest(i).andR,
        clock = clock,
        disable = reset.asDisable,
        label = "SVA_ASSUME_enq_is_youngest"
      )
      AssumeProperty(
        enqValid |=> enqPtrYoungerThanLasyCycle,
        clock = Some(clock),
        disable = Some(reset.asDisable),
        label = Some("")
      )
    }

    if(enqEntryNum == 2) {
      val enq0LessEnq1Valid = BoolSequence(enqRobPtrVec(0) < enqRobPtrVec(1) + dqSize.U)
      val bothEnq = BoolSequence(enqValidVec.reduce(_ && _))
      AssumeProperty(
        bothEnq |-> enq0LessEnq1Valid,
        clock = Some(clock),
        disable = Some(reset.asDisable),
        label = Some("")
      )
      // val enq1LessEnq0Valid = BoolSequence(enqRobPtrVec(0) > enqRobPtrVec(1) + dqSize.U)
      // val bothEnq = BoolSequence(enqValidVec.reduce(_ && _))
      // AssumeProperty(bothEnq |-> (enq0LessEnq1Valid or enq1LessEnq0Valid), clock = Some(clock), disable = Some(reset.asDisable), label = Some(""))
    }
    
  }
}
