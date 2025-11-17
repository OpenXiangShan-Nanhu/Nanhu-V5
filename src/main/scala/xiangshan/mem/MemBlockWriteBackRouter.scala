package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import xiangshan.XSModule
import chisel3._
import chisel3.util.{Arbiter, Decoupled}
import xiangshan.backend.Bundles.MemExuOutput
import xiangshan.backend.HasMemBlockParameters

class MemBlockWriteBackRouter(implicit p: Parameters) extends XSModule with HasMemBlockParameters{
  val io = IO(new Bundle {
      val lduIn = Vec(LduCnt,Flipped(Decoupled(new MemExuOutput)))
      val staIn = Vec(StaCnt,Flipped(Decoupled(new MemExuOutput)))
      val amoIn = Flipped(Decoupled(new MemExuOutput))
      val vecIn = Vec(4, Flipped(Decoupled(new MemExuOutput)))  //todo


      val ldu_amoOut = Vec(LduCnt,Decoupled(new MemExuOutput))
      val staOut = Vec(StaCnt,Decoupled(new MemExuOutput))
      val vecOut = Vec(1, Decoupled(new MemExuOutput))
  })

  //todo: remove ready

  //amo will use loadUnit_0 port to write back
  val ldu0WbArb = Module(new Arbiter(new MemExuOutput, 2))
  ldu0WbArb.io.in(0) <> io.lduIn.head
  ldu0WbArb.io.in(1) <> io.amoIn

  io.ldu_amoOut.head <> ldu0WbArb.io.out
  for(i <- 1 until LduCnt){
    io.ldu_amoOut(i) <> io.lduIn(i)
  }

  //sta write back
  for(i <- 0 until StaCnt){
    io.staOut(i) <> io.staIn(i)
  }

  //vector write back
  val vecWbArb = Module(new Arbiter(new MemExuOutput, 4))
  require(io.vecIn.length == vecWbArb.io.in.length)
  for(i <- 0 until io.vecIn.length){
    vecWbArb.io.in(i) <> io.vecIn
  }
  io.vecOut <> vecWbArb.io.out
}
