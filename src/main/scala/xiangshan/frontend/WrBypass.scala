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
package xiangshan.frontend

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xs.utils._
import xs.utils.perf._

class WrBypass[T <: Data](gen: T, val numEntries: Int, val idxWidth: Int,
  val numWays: Int = 1, val tagWidth: Int = 0, val extraPort: Option[Boolean] = None)(implicit p: Parameters) extends XSModule {
  require(numEntries >= 0)
  require(idxWidth > 0)
  require(numWays >= 1)
  require(tagWidth >= 0)
  def hasTag = tagWidth > 0
  def multipleWays = numWays > 1
  val io = IO(new Bundle {
    val wen = Input(Bool())
    val write_idx = Input(UInt(idxWidth.W))
    val write_tag = if (hasTag) Some(Input(UInt(tagWidth.W))) else None
    val write_data = Input(Vec(numWays, gen))
    val write_way_mask = if (multipleWays) Some(Input(Vec(numWays, Bool()))) else None

    val conflict_valid = if(extraPort.isDefined) Some(Input(Bool())) else None
    val conflict_write_data = if(extraPort.isDefined) Some(Input(Vec(numWays, gen))) else None
    val conflict_way_mask = if(extraPort.isDefined) Some(Input(UInt(numBr.W))) else None

    val hit = Output(Bool())
    val hit_data = Vec(numWays, Valid(gen))
    val has_conflict = if(extraPort.isDefined) Some(Output(Bool())) else None
    val update_idx = if(extraPort.isDefined) Some(Output(UInt(idxWidth.W))) else None
    val update_data = if(extraPort.isDefined) Some(Output(Vec(numWays, gen))) else None
    val update_way_mask = if(extraPort.isDefined) Some(Output(UInt(numBr.W))) else None

    val conflict_clean = if(extraPort.isDefined) Some(Input(Bool())) else None
  })

  class Idx_Tag extends Bundle {
    val idx = UInt(idxWidth.W)
    val tag = if (hasTag) Some(UInt(tagWidth.W)) else None
    def apply(idx: UInt, tag: UInt) = {
      this.idx := idx
      this.tag.map(_ := tag)
    }
  }

  val idx_tag_cam = Module(new IndexableCAMTemplate(new Idx_Tag, numEntries, 1, isIndexable = extraPort.isDefined))
  val data_mem = Mem(numEntries, Vec(numWays, gen))

  val valids = RegInit(0.U.asTypeOf(Vec(numEntries, Vec(numWays, Bool()))))
  val ever_written = RegInit(0.U.asTypeOf(Vec(numEntries, Bool())))


  idx_tag_cam.io.r.req(0)(io.write_idx, io.write_tag.getOrElse(0.U))
  val hits_oh = idx_tag_cam.io.r.resp(0).zip(ever_written).map {case (h, ew) => h && ew}
  val hit_idx = OHToUInt(hits_oh)
  val hit = hits_oh.reduce(_||_)

  io.hit := hit
  for (i <- 0 until numWays) {
    io.hit_data(i).valid := Mux1H(hits_oh, valids)(i)
    io.hit_data(i).bits  := data_mem.read(hit_idx)(i)
  }

  // Replacer
  // Because data_mem can only write to one index
  // Implementing a per-way replacer is meaningless
  // So here use one replacer for all ways
  val replacer = ReplacementPolicy.fromString("plru", numEntries) // numEntries in total
  val replacer_touch_ways = Wire(Vec(1, Valid(UInt(log2Ceil(numEntries).W)))) // One index at a time
  val enq_idx = replacer.way
  val full_mask = Fill(numWays, 1.U(1.W)).asTypeOf(Vec(numWays, Bool()))
  val update_way_mask = io.write_way_mask.getOrElse(full_mask)

  // write data on every request
  when (io.wen) {
    val data_write_idx = Mux(hit, hit_idx, enq_idx)
    data_mem.write(data_write_idx, io.write_data, update_way_mask)
  }
  replacer_touch_ways(0).valid := io.wen
  replacer_touch_ways(0).bits := Mux(hit, hit_idx, enq_idx)
  replacer.access(replacer_touch_ways)

  // update valids
  for (i <- 0 until numWays) {
    when (io.wen) {
      when (hit) {
        when (update_way_mask(i)) {
          valids(hit_idx)(i) := true.B
        }
      }.otherwise {
        ever_written(enq_idx) := true.B
        valids(enq_idx)(i) := false.B
        when (update_way_mask(i)) {
          valids(enq_idx)(i) := true.B
        }
      }
    }
  }

  val enq_en = io.wen && !hit
  idx_tag_cam.io.w.valid := enq_en
  idx_tag_cam.io.w.bits.index := enq_idx
  idx_tag_cam.io.w.bits.data(io.write_idx, io.write_tag.getOrElse(0.U))

  //Extra ports are used to handle dual port read/write conflicts
  if (extraPort.isDefined) {
    val conflict_flags = RegInit(0.U.asTypeOf(Vec(numEntries, Bool())))
    val conflict_way_mask = RegInit(0.U.asTypeOf(io.conflict_way_mask.get))
    val conflict_data = RegInit(VecInit(Seq.tabulate(numWays)( i => 0.U.asTypeOf(gen))))
    val conflict_idx = OHToUInt(conflict_flags)

    idx_tag_cam.io.ridx.get := conflict_idx

    when (io.wen && io.conflict_valid.getOrElse(false.B)) {
      conflict_flags(Mux(hit, hit_idx, enq_idx)) := true.B
      conflict_way_mask := io.conflict_way_mask.get
      conflict_data := io.conflict_write_data.get
    }
    when (io.conflict_clean.getOrElse(false.B)) {
      conflict_flags(conflict_idx) := false.B
    }
    // for update the cached data
    io.has_conflict.get := conflict_flags.reduce(_||_)
    io.update_idx.get := idx_tag_cam.io.rdata.get.idx
    io.update_way_mask.get := conflict_way_mask
    io.update_data.foreach(_ := conflict_data)
  } else None

  XSPerfAccumulate("wrbypass_hit",  io.wen &&  hit)
  XSPerfAccumulate("wrbypass_miss", io.wen && !hit)

  XSDebug(io.wen && hit,  p"wrbypass hit entry #${hit_idx}, idx ${io.write_idx}" +
    p"tag ${io.write_tag.getOrElse(0.U)}data ${io.write_data}\n")
  XSDebug(io.wen && !hit, p"wrbypass enq entry #${enq_idx}, idx ${io.write_idx}" +
    p"tag ${io.write_tag.getOrElse(0.U)}data ${io.write_data}\n")
}
