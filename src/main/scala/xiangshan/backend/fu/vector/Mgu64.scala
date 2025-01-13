package xiangshan.backend.fu.vector

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.chipsalliance.cde.config.Parameters
import top.{ArgParser, BaseConfig, DefaultConfig}
import xiangshan._
import xiangshan.backend.fu.vector.Bundles.{VSew, Vl}
import xiangshan.backend.fu.vector.Utils.VecDataToMaskDataVec
import yunsuan.vector._

class Mgu64IO(vlen: Int)(implicit p: Parameters) extends Bundle {
  val in = new Bundle {
    val vd = Input(UInt(vlen.W))
    val oldVd = Input(UInt(vlen.W))
    val mask = Input(UInt(vlen.W))
    val info = Input(new VecInfo)
    val isIndexedVls = Input(Bool())
    val isLo = Input(Bool())
  }
  val out = new Bundle {
    val vd = Output(UInt(64.W))
    val active = Output(UInt((vlen / 8).W))
    val illegal = Output(Bool())
  }
  val debugOnly = Output(new Bundle {
    val vstartMapVdIdx = UInt()
    val vlMapVdIdx = UInt()
    val begin = UInt()
    val end = UInt()
    val activeEn = UInt()
    val agnosticEn = UInt()
  })
}

class Mgu64(vlen: Int)(implicit p: Parameters) extends  Module {
  private val numBytes = vlen / 8
  private val byteWidth = log2Up(numBytes)

  val io = IO(new Mgu64IO(vlen))

  val in = io.in
  val out = io.out
  val info = in.info
  val vd = in.vd
  val oldVd = in.oldVd
  val narrow = io.in.info.narrow

  private val vdIdx = Mux(narrow, info.vdIdx(2, 1), info.vdIdx)

  private val maskTailGen = Module(new ByteMaskTailGen(vlen))

  private val eewOH = SewOH(info.eew).oneHot

  private val vstartMapVdIdx = elemIdxMapVdIdx(info.vstart)(2, 0) // 3bits 0~7
  private val vlMapVdIdx = elemIdxMapVdIdx(info.vl)(3, 0)         // 4bits 0~8
  private val uvlMax = numBytes.U >> info.eew
  private val uvlMaxForAssert = numBytes.U >> info.vsew
  private val vlMaxForAssert = Mux(io.in.info.vlmul(2), uvlMaxForAssert >> (-io.in.info.vlmul), uvlMaxForAssert << io.in.info.vlmul).asUInt

  private val realEw = Mux(in.isIndexedVls, info.vsew, info.eew)
  private val maskDataVec: Vec[UInt] = VecDataToMaskDataVec(in.mask, realEw)
  protected lazy val maskUsed = maskDataVec(vdIdx)

  maskTailGen.io.in.begin := info.vstart /*Mux1H(Seq(
    (vstartMapVdIdx < vdIdx) -> 0.U,
    (vstartMapVdIdx === vdIdx) -> elemIdxMapUElemIdx(info.vstart),
    (vstartMapVdIdx > vdIdx) -> uvlMax,
  ))*/
  maskTailGen.io.in.end := info.vl /*Mux1H(Seq(
    (vlMapVdIdx < vdIdx) -> 0.U,
    (vlMapVdIdx === vdIdx) -> elemIdxMapUElemIdx(info.vl),
    (vlMapVdIdx > vdIdx) -> uvlMax,
  ))*/
  maskTailGen.io.in.vma := info.ma
  maskTailGen.io.in.vta := info.ta
  maskTailGen.io.in.vsew := realEw
  maskTailGen.io.in.maskUsed := maskUsed
  maskTailGen.io.in.vdIdx := vdIdx

  private val activeEn = Mux(io.in.isLo, maskTailGen.io.out.activeEn(7, 0), maskTailGen.io.out.activeEn(15, 8))
  private val agnosticEn = Mux(io.in.isLo, maskTailGen.io.out.agnosticEn(7, 0), maskTailGen.io.out.agnosticEn(15, 8))

  // the result of normal inst and narrow inst which does not need concat
  private val byte1s: UInt = (~0.U(8.W)).asUInt

  private val resVecByte = Wire(Vec(8, UInt(8.W)))
  private val vdVecByte = vd(63, 0).asTypeOf(resVecByte)
  private val oldVdVecByte = oldVd(63, 0).asTypeOf(resVecByte)

  for (i <- 0 until 8) {
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

  // the result of mask-generating inst
  private val maxVdIdx = 8
  private val meaningfulBitsSeq = Seq(16, 8, 4, 2)
  private val allPossibleResBit = Wire(Vec(4, Vec(maxVdIdx, UInt(vlen.W))))

  for (sew <- 0 to 3) {
    if (sew == 0) {
      allPossibleResBit(sew)(maxVdIdx - 1) := Cat(maskVd(meaningfulBitsSeq(sew) - 1, 0),
        oldVd(meaningfulBitsSeq(sew) * (maxVdIdx - 1) - 1, 0))
    } else {
      allPossibleResBit(sew)(maxVdIdx - 1) := Cat(oldVd(vlen - 1, meaningfulBitsSeq(sew) * maxVdIdx),
        maskVd(meaningfulBitsSeq(sew) - 1, 0), oldVd(meaningfulBitsSeq(sew) * (maxVdIdx - 1) - 1, 0))
    }
    for (i <- 1 until maxVdIdx - 1) {
      allPossibleResBit(sew)(i) := Cat(oldVd(vlen - 1, meaningfulBitsSeq(sew) * (i + 1)),
        maskVd(meaningfulBitsSeq(sew) - 1, 0), oldVd(meaningfulBitsSeq(sew) * i - 1, 0))
    }
    allPossibleResBit(sew)(0) := Cat(oldVd(vlen - 1, meaningfulBitsSeq(sew)), maskVd(meaningfulBitsSeq(sew) - 1, 0))
  }

  private val resVecBit = allPossibleResBit(info.eew)(vdIdx)

  io.out.vd := MuxCase(resVecByte.asUInt, Seq(
    info.dstMask -> resVecBit.asUInt,
  ))
  io.out.active := activeEn
  io.out.illegal := false.B // (info.vl > vlMaxForAssert) && info.valid

  io.debugOnly.vstartMapVdIdx := vstartMapVdIdx
  io.debugOnly.vlMapVdIdx := vlMapVdIdx
  io.debugOnly.begin := maskTailGen.io.in.begin
  io.debugOnly.end := maskTailGen.io.in.end
  io.debugOnly.activeEn := activeEn
  io.debugOnly.agnosticEn := agnosticEn
  def elemIdxMapVdIdx(elemIdx: UInt) = {
    require(elemIdx.getWidth >= log2Up(vlen))
    // 3 = log2(8)
    Mux1H(eewOH, Seq.tabulate(eewOH.getWidth)(x => elemIdx(byteWidth - x + 3, byteWidth - x)))
  }

  def elemIdxMapUElemIdx(elemIdx: UInt) = {
    Mux1H(eewOH, Seq.tabulate(eewOH.getWidth)(x => elemIdx(byteWidth - x - 1, 0)))
  }

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