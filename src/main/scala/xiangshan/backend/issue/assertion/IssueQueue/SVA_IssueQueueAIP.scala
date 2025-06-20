package xiangshan.backend.issue.assertion.IssueQueue

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters

import xiangshan._
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
    val entries = Input(new SVA_ProbeEntries)
  })

  // 1. trans strategy mutex check
  if(hasCompAndSimp) {
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
      a = io.entries.enqCanTrans2Comp.get,
      b = io.entries.enqCanTrans2Simp.get || io.entries.simpCanTrans2Comp.get
    )
  } else {
    // trans strategy
    // only enq -> comp
  }

  // 2. TODO
  io.entries.entryRegVec.foreach {
    case entry => {
      CVL_ASSERT_MUTEX(
        assertEn = true,
        coverEn = false,
        cvlLongSequence = true,
        clock = clock,
        reset = reset,
        name = "Entries_trans_strategy",
        a = entry.status.canIssue,
        b = !entry.status.srcReady
      )
    }
  }

  // Constraint Assume
  // if(formal_fpv) {
    
  // }
}
