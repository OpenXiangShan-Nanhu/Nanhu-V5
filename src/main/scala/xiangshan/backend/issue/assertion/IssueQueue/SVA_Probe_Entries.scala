package xiangshan.backend.issue.assertion.IssueQueue

import chisel3._
import chisel3.util._
import chisel3.probe._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend._
import xiangshan.backend.issue._
import xiangshan.backend.issue.EntryBundles._
import utils.OptionWrapper

class SVA_ProbeEntries(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
  def entriesNum = params.numEntries
  def enqEntryNum = params.numEnq
  def compEntryNum = params.numComp
  def simpEnrtyNum = entriesNum - enqEntryNum - compEntryNum
  def hasCompAndSimp = params.hasCompAndSimp

  val entryRegVec = Vec(entriesNum, new EntryBundle)
  val entryValidVec = Vec(entriesNum, Bool())

  val entryCommonOutVec = Vec(entriesNum, new CommonOutBundle)

  val enqCanTrans2Comp = OptionWrapper(hasCompAndSimp, Bool())
  val enqCanTrans2Simp = OptionWrapper(hasCompAndSimp, Bool())
  val simpCanTrans2Comp = OptionWrapper(hasCompAndSimp, Bool())

}
