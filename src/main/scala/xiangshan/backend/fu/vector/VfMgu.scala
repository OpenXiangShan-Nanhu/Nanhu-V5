/****************************************************************************************
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
 ****************************************************************************************
 */


package xiangshan.backend.fu.vector

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.ChiselScalatestTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import top.{ArgParser, BaseConfig, DefaultConfig}
import xiangshan._
import xiangshan.backend.fu.vector.Bundles.{VSew, Vl}
import xiangshan.backend.fu.vector.Utils.VecDataToMaskDataVec
import yunsuan.vector._

class VfMguIO(vlen: Int)(implicit p: Parameters) extends Bundle {
  val in = new Bundle {
    val vd = Input(UInt(64.W))
    val oldVd = Input(UInt(64.W))
    val mask = Input(UInt(vlen.W))
    val info = Input(new VecInfo)
    val isHi = Input(Bool())
  }
  val out = new Bundle {
    val vd = Output(UInt(64.W))
    val active = Output(UInt((64 / 8).W))
    val illegal = Output(Bool())
  }
}

class VfMgu(vlen: Int)(implicit p: Parameters) extends Module {
  private val numBytes = vlen / 8 / (vlen / 64)
  private val byteWidth = log2Up(numBytes)

  val io = IO(new VfMguIO(vlen))

  val in = io.in
  val out = io.out
  val info = in.info
  val vd = in.vd
  val oldVd = in.oldVd
  val narrow = io.in.info.narrow

  private val vdIdx = Mux(narrow, info.vdIdx(2, 1), info.vdIdx)

  private val maskTailGen = Module(new VfByteMaskTailGen(vlen))

  private val realEw = info.eew
  private val maskDataVec: Vec[UInt] = VecDataToMaskDataVec(in.mask, realEw)
  protected lazy val maskUsed = VecInit(maskDataVec.map(m => VecInit(m(m.getWidth/2-1, 0), m(m.getWidth-1, m.getWidth/2))))(vdIdx)(io.in.isHi)

  maskTailGen.io.in.begin := info.vstart
  maskTailGen.io.in.end := info.vl
  maskTailGen.io.in.vma := info.ma
  maskTailGen.io.in.vta := info.ta
  maskTailGen.io.in.vsew := realEw
  maskTailGen.io.in.maskUsed := maskUsed
  maskTailGen.io.in.vdIdx := vdIdx
  maskTailGen.io.in.isHi := io.in.isHi

  private val activeEn = maskTailGen.io.out.activeEn
  private val agnosticEn = maskTailGen.io.out.agnosticEn

  // the result of normal inst and narrow inst which does not need concat
  private val byte1s: UInt = (~0.U(8.W)).asUInt

  private val resVecByte = Wire(Vec(numBytes, UInt(8.W)))
  private val vdVecByte = vd.asTypeOf(resVecByte)
  private val oldVdVecByte = oldVd.asTypeOf(resVecByte)

  for (i <- 0 until numBytes) {
    resVecByte(i) := MuxCase(oldVdVecByte(i), Seq(
      activeEn(i) -> vdVecByte(i),
      agnosticEn(i) -> byte1s,
    ))
  }

  // mask vd is at most 16 bits
  private val maskOldVdBits = splitVdMask(oldVd, SewOH(info.eew))(vdIdx)
  private val maskBits = splitVdMask(in.mask, SewOH(info.eew))(vdIdx)
  private val maskVecByte = Wire(Vec(numBytes, UInt(1.W)))
  maskVecByte.zipWithIndex.foreach { case (mask, i) =>
    mask := Mux(maskBits(i), vd(i), Mux(info.ma, 1.U, maskOldVdBits(i)))
  }
  private val maskVd = maskVecByte.asUInt

  io.out.vd := resVecByte.asUInt
  io.out.active := activeEn
  io.out.illegal := false.B // (info.vl > vlMaxForAssert) && info.valid

  def splitVdMask(maskIn: UInt, sew: SewOH): Vec[UInt] = {
    val maskWidth = maskIn.getWidth
    val result = Wire(Vec(maskWidth / numBytes, UInt(numBytes.W)))
    for ((resultData, i) <- result.zipWithIndex) {
      resultData := Mux1H(Seq(
        sew.is8 -> maskIn(i * numBytes + (numBytes - 1), i * numBytes),
        sew.is16 -> Cat(0.U((numBytes - (numBytes / 2)).W), maskIn(i * (numBytes / 2) + (numBytes / 2) - 1, i * (numBytes / 2))),
        sew.is32 -> Cat(0.U((numBytes - (numBytes / 4)).W), maskIn(i * (numBytes / 4) + (numBytes / 4) - 1, i * (numBytes / 4))),
        sew.is64 -> Cat(0.U((numBytes - (numBytes / 8)).W), maskIn(i * (numBytes / 8) + (numBytes / 8) - 1, i * (numBytes / 8))),
      ))
    }
    result
  }
}