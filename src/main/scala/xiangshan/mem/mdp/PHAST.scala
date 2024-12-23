package xiangshan.mem.mdp

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSBundle
import xiangshan.mem.SqPtr
import xs.utils.ReplacementPolicy

trait HasPHASTMDPParameters {
  val PHAST_size = 128
  val PHAST_waySize = 4
  val PHAST_lruBit = log2Up(PHAST_waySize)

  val PHAST_cntWidth = 4
  val PHAST_tagWidth = 16
  val PHAST_replacer = "plru"

}

object PHASTMDPPCFold{
  def apply(input: UInt): UInt = {
    input ^ ((input >> 2).asUInt) ^ (input >> 5).asUInt
  }
}


class PHAST_MDPQuery(implicit p: Parameters) extends XSBundle {
  val ld_stIdx = new SqPtr
  val ld_pc = UInt(VAddrBits.W)
}

class PHAST_MDPResp(implicit p: Parameters) extends MDPResp{

}

class PHAST_MDPUpdateIO(implicit p: Parameters) extends XSBundle{
  val stIdx = new SqPtr
  val ld_stIdx = new SqPtr

  val ldPc = UInt(VAddrBits.W)
  val stPc = UInt(VAddrBits.W)

  def getDistance(): UInt = {
    stIdx.getDistance(front = stIdx, back = ld_stIdx)
  }
}



class PHASTEntry(implicit p: Parameters) extends XSBundle with HasPHASTMDPParameters{
  val tag = UInt(PHAST_tagWidth.W)
  val distance = UInt(log2Up(StoreQueueSize).W)
  val cnt = UInt(PHAST_cntWidth.W)
}

class PHAST(implicit p: Parameters) extends XSBundle with HasPHASTMDPParameters{
  val io = IO(new Bundle{
    val ldReq = Vec(LoadPipelineWidth, Input(ValidIO(new PHAST_MDPQuery)))
    val ldResp = Vec(LoadPipelineWidth, Output(ValidIO(new PHAST_MDPResp)))

    val reUpdate = Input(Valid(new PHAST_MDPUpdateIO))
  })

  val entry = Vec(PHAST_size, Vec(PHAST_waySize, RegInit(0.U.asTypeOf(new PHASTEntry))))

  val entryValid = entry.map(_.map(_.cnt =/= 0.U))
  val entryTag = entry.map(_.map(_.tag))
  val entryCnt = entry.map(_.map(_.cnt))
  val entryDis = entry.map(_.map(_.distance))

  val replacer = ReplacementPolicy.fromString(PHAST_replacer, PHAST_size, PHAST_waySize)





}



