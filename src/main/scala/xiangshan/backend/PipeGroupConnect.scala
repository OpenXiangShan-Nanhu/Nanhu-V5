package xiangshan.backend

import chisel3._
import chisel3.util._

class PipeGroupConnect[T <: Data](n: Int, gen: => T) extends Module {
  val io = IO(new Bundle {
    val in = Vec(n, Flipped(DecoupledIO(gen)))
    val out = Vec(n, DecoupledIO(gen))
    val flush = Input(Bool())
    val outAllFire = Input(Bool())
  })

  // Input Alias
  // Use private[this] to limit the wrong usage for not IO hardware in object with the same name.
  private[this] val flush = io.flush
  private[this] val inValidSeq  = io.in.map(_.valid)
  private[this] val inDataSeq   = io.in.map(_.bits)
  private[this] val outReadySeq = io.out.map(_.ready)

  // Regs
  private[this] val validVec = RegInit(VecInit.fill(n)(false.B))
  private[this] val dataVec  = Reg(Vec(n, gen))

  // Logic
  private[this] val valids    = Cat(validVec.reverse)
  private[this] val inValids  = Cat(inValidSeq.reverse)
  private[this] val outReadys = Cat(outReadySeq.reverse)

  // Todo: canAccVec for each elem
  // Todo: no outReadys version for better timing and lower performance
  private[this] val canAcc = io.outAllFire || !valids.orR

  (validVec zip inValids.asBools zip outReadys.asBools).foreach { case ((valid, inValid), outReady) =>
    valid := MuxCase(
      default = valid /*keep*/,
      Seq(
        flush               -> false.B,
        (inValid && canAcc) -> true.B,
        outReady            -> false.B
      )
    )
  }

  (dataVec zip inValids.asBools zip inDataSeq).foreach { case ((data, inValid), inData) =>
    when (inValid && canAcc) {
      data := inData
    }
  }

  // Output connections
  for (i <- 0 until n) {
    io.in(i).ready  := canAcc
    io.out(i).valid := validVec(i)
    io.out(i).bits  := dataVec(i)
  }
}

class PipeGroupConnectLessFanOut[T <: Data](n: Int, fanoutNum: Int, gen: => T) extends Module {
  val io = IO(new Bundle {
    val in = Vec(n, Flipped(DecoupledIO(gen)))
    val out = Vec(fanoutNum, Vec(n, DecoupledIO(gen)))
    val flush = Input(Bool())
    val outAllFire = Input(Bool())
  })

  // Input Alias
  // Use private[this] to limit the wrong usage for not IO hardware in object with the same name.
  private[this] val flush = io.flush
  private[this] val inValidSeq  = io.in.map(_.valid)
  private[this] val inDataSeq   = io.in.map(_.bits)
  private[this] val outReadySeq = io.out(0).map(_.ready)

  // Regs
  private[this] val validVec_dup = RegInit(VecInit.fill(fanoutNum)(VecInit.fill(n)(false.B)))
  private[this] val dataVec_dup  = Reg(Vec(fanoutNum, Vec(n, gen)))

  // Logic
  private[this] val valids    = Cat(validVec_dup(0).reverse)
  private[this] val inValids  = Cat(inValidSeq.reverse)
  private[this] val outReadys = Cat(outReadySeq.reverse)

  // Todo: canAccVec for each elem
  // Todo: no outReadys version for better timing and lower performance
  private[this] val canAcc = io.outAllFire || !valids.orR

  validVec_dup.zipWithIndex.foreach{case (dup, j) =>
    (dup.zipWithIndex zip inValids.asBools zip outReadys.asBools).foreach { case (((valid, i), inValid), outReady) =>
        valid := MuxCase(
          default = valid /*keep*/,
          Seq(
            flush               -> false.B,
            (inValid && canAcc) -> true.B,
            outReady            -> false.B
          )
        )  
    }
  }

  dataVec_dup.zipWithIndex.foreach{case (dup, j) =>
    (dup zip inValids.asBools zip inDataSeq).foreach { case ((data, inValid), inData) =>
      when (inValid && canAcc) {
        data := inData
      }
    }
  }

  // Output connections
  for (i <- 0 until fanoutNum) {
    io.out(i).zipWithIndex.foreach{case (out, j) =>
      out.valid := validVec_dup(i)(j)
      out.bits := dataVec_dup(i)(j)
    }
  }
  for (i <- 0 until n) {
    io.in(i).ready  := canAcc
  }
}

object PipeGroupConnect {
  def apply[T <: Data](
    // Left can be not Vec, but right must be Vec
    left: Seq[DecoupledIO[T]],
    right: Vec[DecoupledIO[T]],
    flush: Bool,
    rightAllFire: Bool,
    suggestName: String = null,
  ): Unit =  {
    require(left.size == right.size, "The sizes of left and right Vec Bundle should be equal in PipeGroupConnect")
    require(left.size > 0, "The size of Vec Bundle in PipeGroupConnect should be more than 0")
    val mod = Module(new PipeGroupConnect(left.size, chiselTypeOf(left.head.bits)))
    mod.io.flush := flush
    mod.io.in.zipWithIndex.foreach { case (in, i) =>
      in.valid := left(i).valid
      in.bits := left(i).bits
      left(i).ready := in.ready
    }
    mod.io.outAllFire := rightAllFire
    right <> mod.io.out

    if (suggestName != null)
      mod.suggestName(suggestName)
  }
}

object PipeGroupConnectLessFanOut {
  def apply[T <: Data](
    // Left can be not Vec, but right must be Vec
    left: Seq[DecoupledIO[T]],
    right: Vec[Vec[DecoupledIO[T]]],
    flush: Bool,
    rightAllFire: Bool,
    suggestName: String = null,
    fanoutNum: Int = 4,
  ): Unit =  {
    require(left.size == right.head.size, "The sizes of left and right Vec Bundle should be equal in PipeGroupConnect")
    require(left.size > 0, "The size of Vec Bundle in PipeGroupConnect should be more than 0")
    val mod = Module(new PipeGroupConnectLessFanOut(left.size, fanoutNum, chiselTypeOf(left.head.bits)))
    mod.io.flush := flush
    mod.io.in.zipWithIndex.foreach { case (in, i) =>
      in.valid := left(i).valid
      in.bits := left(i).bits
      left(i).ready := in.ready
    }
    mod.io.outAllFire := rightAllFire
    right <> mod.io.out

    if (suggestName != null)
      mod.suggestName(suggestName)
  }
}
