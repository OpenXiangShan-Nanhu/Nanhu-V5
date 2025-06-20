package xiangshan.backend.issue.assertion.IssueQueue

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters

import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.issue.IssueBlockParams
import xiangshan.backend.issue.EntryBundles._
import utils.OptionWrapper
import chisel3.ltl.Sequence.BoolSequence

import xs.utils.cvl.basic._
import xs.utils.cvl.advanced._

class SVA_IssueQueueAIP(formal_fpv: Boolean)(implicit p: Parameters, params: IssueBlockParams) extends XSModule {
  def entriesNum = params.numEntries
  def enqEntryNum = params.numEnq
  def compEntryNum = params.numComp
  def simpEnrtyNum = entriesNum - enqEntryNum - compEntryNum
  def hasCompAndSimp = params.hasCompAndSimp

  def enq2comp_idx = 0
  def enq2simp_idx = 1
  def simp2comp_idx = 2

  require(enqEntryNum == 1 || enqEntryNum == 2)

  val io = IO(new Bundle {
    val enq = Vec(params.numEnq, Input(Valid(new DynInst)))
    val probe_entries = Input(new SVA_ProbeEntries)
    val issueRobIdx = Input(Valid(new RobPtr))
  })

  val canIssueVec = io.probe_entries.entryCommonOutVec.map(_.canIssue)
  val validVec = io.probe_entries.entryValidVec
  val robPtrVec = io.probe_entries.entryRegVec.map(_.payload.robIdx)

  // 1. trans strategy mutex check
  SVA_AssertEntriesTrans(
    params = params,
    clock = clock,
    reset = reset,
    enqCanTrans2Comp = io.probe_entries.enqCanTrans2Comp,
    enqCanTrans2Simp = io.probe_entries.enqCanTrans2Simp,
    simpCanTrans2Comp = io.probe_entries.simpCanTrans2Comp,
    robPtrVec = robPtrVec,
    validVec = validVec
  )

  // 2. sel oldest
  SVA_AssertIqOldestSel(
    params = params,
    clock = clock,
    reset = reset,
    canIssueVec = canIssueVec,
    robPtrVec = robPtrVec,
    validVec = validVec,
    issue_valid = io.issueRobIdx.valid,
    selRobPtr = io.issueRobIdx.bits
  )

  // Constraint Assume
  // if(formal_fpv) {
    
  // }
  SVA_AssumeEnqOrdered(
    params = params,
    clock = clock,
    reset = reset,
    enqRobPtrVec = io.enq.map(_.bits.robIdx),
    enqValidVec = io.enq.map(_.valid),
    robPtrVec = robPtrVec,
    validVec = validVec
  )
}
