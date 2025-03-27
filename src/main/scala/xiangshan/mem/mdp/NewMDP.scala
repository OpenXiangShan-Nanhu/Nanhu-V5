package xiangshan.mem.mdp

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.{OH1ToUInt, PseudoLRU, ReplacementPolicy}
import org.chipsalliance.cde.config.Parameters
import utility.XORFold
import xiangshan.backend.rob.RobPtr
import xiangshan.mem.SqPtr
import xiangshan.{CustomCSRCtrlIO, XSBundle, XSModule}
import xs.utils.perf.XSPerfAccumulate

trait HasMDPParameters {
  val mdpSize = 32
  val timerTableSize = 4
  val counterWidth = 3
  val timerWidth = 10

  def FullConfidence() :UInt = {
    Cat(Fill(counterWidth,1.U))
  }
}

object MDPPCFold {
  def apply(input: UInt, resWidth: Int, reserveLowBit:Int = 5): UInt = {
    Cat(XORFold(input(input.getWidth - 1, reserveLowBit),resWidth - reserveLowBit), input(reserveLowBit - 1, 0))
  }
}

class MDPEntry(implicit p: Parameters) extends XSBundle with HasMDPParameters{
//  val pc = UInt(VAddrBits.W)
  val fold_pc = UInt(MemPredPCWidth.W)
  val distance = UInt(log2Up(RobSize).W)
  val confidence = UInt(counterWidth.W)


  def confidenceValid(replayQNum: UInt = 0.U) : Bool = {
    this.confidence > replayQNum / 4.U
  }

  def writeConFull() = {
    this.confidence := Cat(Fill(counterWidth,1.U))
  }

}

class MDPTimerEntry(implicit p: Parameters) extends XSBundle with HasMDPParameters{
  //  val pc = UInt(VAddrBits.W)
  val fold_pc = UInt(MemPredPCWidth.W)
  val distance = UInt(log2Up(RobSize).W)
  val timer = UInt(timerWidth.W)


  def timerOutValid : Bool = {
    this.timer > (1 << timerWidth/2).U
  }

  def timerValid : Bool = {
    this.timer > 0.U
  }

  def writeTimerFull() = {
    this.timer := Cat(Fill(timerWidth,1.U))
  }

}


class MDPUpdateIO(implicit p: Parameters) extends XSBundle{
  val stIdx = new SqPtr
//  val stPc  = UInt(VAddrBits.W)
  val ld_stIdx = new SqPtr
//  val ldPC = UInt(VAddrBits.W)

  val distance = UInt(log2Up(RobSize).W)
  val ldFoldPc = UInt(MemPredPCWidth.W)
  val stFoldPc = UInt(MemPredPCWidth.W)

  def getDistance(): UInt = {
    stIdx.getDistance(front = stIdx, back = ld_stIdx)
  }
}

class MDPResUpdateIO(implicit p: Parameters) extends XSBundle{
//  val stIdx = new SqPtr
//  val stPc  = UInt(VAddrBits.W)
//  val ld_stIdx = new SqPtr
  val ldPC = UInt(VAddrBits.W)
  val distance = UInt(log2Up(RobSize).W)
  val fail = Bool()

//  def getDistance(): UInt = {
//    stIdx.getDistance(front = stIdx, back = ld_stIdx)
//  }
}


class MDPQuery(implicit p: Parameters) extends XSBundle{
//  val ldRob = new RobPtr
  val ld_stIdx = new SqPtr
//  val ldpc = UInt(VAddrBits.W)
  val ldFoldPc = UInt(MemPredPCWidth.W)
  def getTag(): UInt = {
    ldFoldPc
  }
}

class MDPResp(implicit p: Parameters) extends XSBundle{
  val waitStIdx = new SqPtr
  val hit = Bool()
}

class MDPCtrlInfo(implicit p: Parameters) extends XSBundle{
  val replayQValidNum = UInt(log2Up(LoadQueueReplaySize + 1).W)
}

class TimerEntry(implicit p: Parameters) extends XSBundle with HasMDPParameters{
  val timer = UInt(timerWidth.W)
}


class SmallestSelector(width:Int)(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val in = Input(Vec(width, Valid(new TimerEntry)))
    val out = Output(Valid(UInt(width.W)))
  })

  val onlyOne = PopCount(io.in.map(_.valid)) === 1.U
  val oldestOHMatrix = io.in.zipWithIndex.map({ case (self, idx) =>
    io.in.zipWithIndex.filterNot(_._2 == idx).map(i => (i._1.valid && self.valid && (self.bits.timer <= i._1.bits.timer)) ^ i._1.valid)
  })
  val smallestOHSeq = oldestOHMatrix.map(_.reduce(_|_)).map(!_)
  val smallestOH = PriorityEncoderOH(Cat(smallestOHSeq.reverse))
  val defaultValue = Cat(io.in.map(_.valid).reverse)
  io.out.valid := io.in.map(_.valid).reduce(_ | _)
  io.out.bits := Mux(onlyOne, defaultValue, smallestOH)

  val selEntryTimer = Mux1H(io.out.bits, io.in.map(_.bits.timer))
  for(i <- io.in.indices) {
    when(io.out.valid && !io.out.bits(i) && io.in(i).valid) {assert(selEntryTimer <= io.in(i).bits.timer)}
  }

  when(io.out.valid) {
    assert(PopCount(io.out.bits) === 1.U)
  }
}


class MiniMDPTimerModule(implicit p: Parameters) extends XSModule with HasMDPParameters{
  val io = IO(new Bundle {
    val ldReq = Vec(LoadPipelineWidth, Input(ValidIO(new MDPQuery)))
    val ldResp = Vec(LoadPipelineWidth, Output(ValidIO(new MDPResp)))

    val reUpdate = Input(Valid(new MDPUpdateIO))
    val ldUpdate = Vec(LoadPipelineWidth, Input(Valid(new MDPResUpdateIO)))
    val out = Output(Valid(new Bundle(){
      val fold_pc = UInt(MemPredPCWidth.W)
      val distance = UInt(log2Up(RobSize).W)
    }))

    val csrCtrl = Input(new MDPCtrlInfo)
  })

  val allocated = RegInit(VecInit(Seq.fill(timerTableSize)(false.B)))
  val tag = Reg(Vec(timerTableSize, new MDPTimerEntry))
  val selector = Module(new SmallestSelector(timerTableSize))
  dontTouch(allocated)
  dontTouch(tag)

  allocated.zip(tag).foreach({case (v,t) =>
    when(v && t.timer > 0.U){
      t.timer := t.timer - 1.U
    }
  })

  //select smallest tag
  val selValid = selector.io.out.valid
  val selIdx = OHToUInt(selector.io.out.bits)
  val selTag = Mux1H(selector.io.out.bits, tag)
  dontTouch(selTag)
  dontTouch(selIdx)


  for(i <- 0 until timerTableSize){
    selector.io.in(i).valid := allocated(i)
    selector.io.in(i).bits.timer := tag(i).timer
  }

  //read
  val s0_reqHitVec = Wire(Vec(LoadPipelineWidth, Vec(timerTableSize, Bool())))
  s0_reqHitVec.zipWithIndex.foreach({ case (q, i) =>
    //S0
    val s0_reqValid = io.ldReq(i).valid
    val s0_reqTag = io.ldReq(i).bits.getTag()
    for (j <- 0 until timerTableSize) {
      q(j) := s0_reqValid && s0_reqTag === tag(j).fold_pc && allocated(j) && tag(j).timerValid
    }
    when(s0_reqValid) {
      assert(PopCount(q) <= 1.U, "hit 2 entry?")
    }
  })

  //S1
  val s1_reqV = io.ldReq.map(r => RegNext(r.valid, false.B))
  val s1_reqB = io.ldReq.map(r => RegEnable(r.bits, r.valid))
  val s1_reqHitVec = s0_reqHitVec.zip(io.ldReq).map({ case (hit, q) => RegEnable(hit, q.valid) })
  val s1_reqHitIdx = s1_reqHitVec.map(OHToUInt(_))
  val s1_reqHit = s1_reqHitVec.map(_.reduce(_ | _))

  io.ldResp.zipWithIndex.foreach({ case (res, i) =>
    res.valid := s1_reqV(i)
    res.bits.hit := s1_reqHitVec(i).reduce(_ | _)
    res.bits.waitStIdx := s1_reqB(i).ld_stIdx - tag(s1_reqHitIdx(i)).distance
  })


  //-----update----
  //S0: find the same entry
  //0   : redirect update
  //1,2 : load update
  val UPDATE_NUM = 3
  val REDIRECT_PORT_IDX_S = 0
  val LOAD_PORT_IDC_S = 1

  val s0_updateValidVec = Wire(Vec(UPDATE_NUM, Bool()))
  val s0_updateTagVec = Wire(Vec(UPDATE_NUM, new MDPEntry))
  val s0_updateHitVec = Wire(Vec(UPDATE_NUM, Vec(timerTableSize, Bool())))
  val s0_updateUpVec = Wire(Vec(UPDATE_NUM, Bool()))

  val s1_updateValidVec = s0_updateValidVec.map(v => RegNext(v, false.B))
  val s1_updateTagVec = s0_updateTagVec.zip(s0_updateValidVec).map({ case t => RegEnable(t._1, t._2) })
  val s1_updateHitVec = s0_updateHitVec.zip(s0_updateValidVec).map({ case h => RegEnable(h._1, h._2) })
  val s1_updateUpVec = s0_updateUpVec.zip(s0_updateValidVec).map({ case u => RegEnable(u._1, u._2) })
  val s1_updateHitUIntVec = s1_updateHitVec.map(a => OHToUInt(a))
  val s1_updateHit = s1_updateHitVec.map(_.reduce(_ | _))

  s0_updateValidVec(REDIRECT_PORT_IDX_S) := io.reUpdate.valid &&
    !(s1_updateValidVec(REDIRECT_PORT_IDX_S) && s1_updateTagVec(REDIRECT_PORT_IDX_S).fold_pc === io.reUpdate.bits.ldFoldPc)
  s0_updateTagVec(REDIRECT_PORT_IDX_S).fold_pc := io.reUpdate.bits.ldFoldPc
  s0_updateTagVec(REDIRECT_PORT_IDX_S).distance := io.reUpdate.bits.getDistance()
  s0_updateTagVec(REDIRECT_PORT_IDX_S).confidence := DontCare
  s0_updateUpVec(REDIRECT_PORT_IDX_S) := true.B
  (0 until LoadPipelineWidth).foreach({ case i =>
    s0_updateValidVec(i + LOAD_PORT_IDC_S) := io.ldUpdate(i).valid
    s0_updateTagVec(i + LOAD_PORT_IDC_S).fold_pc := io.ldUpdate(i).bits.ldPC
    s0_updateTagVec(i + LOAD_PORT_IDC_S).distance := io.ldUpdate(i).bits.distance
    s0_updateTagVec(i + LOAD_PORT_IDC_S).confidence := DontCare
    s0_updateUpVec(i + LOAD_PORT_IDC_S) := !io.ldUpdate(i).bits.fail
  })

  for (i <- 0 until UPDATE_NUM) {
    for (j <- 0 until timerTableSize) {
      s0_updateHitVec(i)(j) := s0_updateTagVec(i).fold_pc === tag(j).fold_pc && s0_updateValidVec(i) && allocated(j)
    }

    when(s0_updateValidVec(i)) {
      assert(PopCount(s0_updateHitVec(i)) <= 1.U, "can not hit more than 2 entry in update_" + i.toString)
    }
  }

  //S1: update entry
  //redirect update
  for (i <- (0 until LoadPipelineWidth)) {
    val portIdx = i + LOAD_PORT_IDC_S
    //if hit
    when(s1_updateValidVec(portIdx) && s1_updateHit(portIdx) && allocated(s1_updateHitUIntVec(portIdx))) {
      tag(s1_updateHitUIntVec(portIdx)).writeTimerFull()
    }
  }


  //load info update
  for (i <- 0 until LOAD_PORT_IDC_S) {
    //if hit
    when(s1_updateValidVec(i) && s1_updateHit(i) && allocated(s1_updateHitUIntVec(i))) {
      //      assert(allocated(s1_updateHitUIntVec(i)),"why disable?")

      val oldDis = tag(s1_updateHitUIntVec(i)).distance
      val newDis = s1_updateTagVec(i).distance

      tag(s1_updateHitUIntVec(i)).distance := Mux(oldDis < newDis, oldDis, newDis)
      tag(s1_updateHitUIntVec(i)).writeTimerFull()
    }
    //if miss
    when(s1_updateValidVec(i) && !s1_updateHit(i)) {
      allocated(selIdx) := true.B
      tag(selIdx).fold_pc := s1_updateTagVec(i).fold_pc
      tag(selIdx).distance := s1_updateTagVec(i).distance
      tag(selIdx).writeTimerFull()
    }
  }

  val needReplace = selValid && allocated.reduce(_&_) && selTag.timerOutValid && io.reUpdate.valid
  dontTouch(needReplace)
  io.out.valid := needReplace
  io.out.bits.distance := selTag.distance
  io.out.bits.fold_pc := selTag.fold_pc
}


class MiniMDPWrapper(implicit p: Parameters) extends XSModule with HasMDPParameters{
  val io = IO(new Bundle {
    val ldReq = Vec(LoadPipelineWidth, Input(ValidIO(new MDPQuery)))
    val ldResp = Vec(LoadPipelineWidth, Output(ValidIO(new MDPResp)))

    val reUpdate = Input(Valid(new MDPUpdateIO))
    val ldUpdate = Vec(LoadPipelineWidth, Input(Valid(new MDPResUpdateIO)))

    val csrCtrl = Input(new MDPCtrlInfo)
  })

  val mdpTimerModule = Module(new MiniMDPTimerModule)
  mdpTimerModule.io.ldReq := io.ldReq
  mdpTimerModule.io.reUpdate := io.reUpdate
  mdpTimerModule.io.ldUpdate := io.ldUpdate
  mdpTimerModule.io.csrCtrl := io.csrCtrl

  val mdpCntModule = Module(new NewMDP)
  mdpCntModule.io.ldReq := io.ldReq
  mdpCntModule.io.ldUpdate := io.ldUpdate
  mdpCntModule.io.csrCtrl := io.csrCtrl
  mdpCntModule.io.reUpdate.valid := mdpTimerModule.io.out.valid
  mdpCntModule.io.reUpdate.bits.distance := mdpTimerModule.io.out.bits.distance
  mdpCntModule.io.reUpdate.bits.ldFoldPc := mdpTimerModule.io.out.bits.fold_pc
  mdpCntModule.io.reUpdate.bits.stFoldPc := DontCare
  mdpCntModule.io.reUpdate.bits.ld_stIdx := DontCare
  mdpCntModule.io.reUpdate.bits.stIdx := DontCare

  io.ldResp.zip(mdpTimerModule.io.ldResp).zip(mdpCntModule.io.ldResp).foreach({case ((resp, respTimer), respCnt) =>
    resp.valid := respTimer.valid | respCnt.valid
    resp.bits := Mux(respTimer.valid, respTimer.bits, respCnt.bits)
  })
}



class NewMDP(implicit p: Parameters) extends XSModule with HasMDPParameters{
  val io = IO(new Bundle{
    val ldReq = Vec(LoadPipelineWidth, Input(ValidIO(new MDPQuery)))
    val ldResp = Vec(LoadPipelineWidth, Output(ValidIO(new MDPResp)))

    val reUpdate = Input(Valid(new MDPUpdateIO))
    val ldUpdate = Vec(LoadPipelineWidth, Input(Valid(new MDPResUpdateIO)))

    val csrCtrl = Input(new MDPCtrlInfo)
  })

  //entry
  val allocated = RegInit(VecInit(Seq.fill(mdpSize)(false.B)))
  val tag = Reg(Vec(mdpSize, new MDPEntry))
  val replacer = new PseudoLRU(mdpSize)

  val replaceIdx = replacer.way
  dontTouch(replaceIdx)

  val accessIdx = Wire(Vec(LoadPipelineWidth + 1, Valid(UInt(mdpSize.W))))
  replacer.access(accessIdx)

  dontTouch(accessIdx)

  accessIdx.foreach({ case a =>
    a.valid := false.B
    a.bits := 0.U
  })

  //----read:
  //S0
  val s0_reqHitVec = Wire(Vec(LoadPipelineWidth, Vec(mdpSize, Bool())))
  s0_reqHitVec.zipWithIndex.foreach({ case (q, i) => {
    //S0
    val s0_reqValid = io.ldReq(i).valid
    val s0_reqTag = io.ldReq(i).bits.getTag()
    for(j <- 0 until mdpSize){
      q(j) := s0_reqValid && s0_reqTag === tag(j).fold_pc && allocated(j) && tag(j).confidenceValid(io.csrCtrl.replayQValidNum)
    }
    when(s0_reqValid){
      assert(PopCount(q) <= 1.U, "hit 2 entry?")
    }
  }})
  //S1
  val s1_reqV = io.ldReq.map(r => RegNext(r.valid, false.B))
  val s1_reqB = io.ldReq.map(r => RegEnable(r.bits, r.valid))
  val s1_reqHitVec = s0_reqHitVec.zip(io.ldReq).map({ case (hit, q) => RegEnable(hit, q.valid)})
  val s1_reqHitIdx = s1_reqHitVec.map(OHToUInt(_))
  val s1_reqHit = s1_reqHitVec.map(_.reduce(_|_))

  io.ldResp.zipWithIndex.foreach({case (res, i) => {
    res.valid := s1_reqV(i)
    res.bits.hit := s1_reqHitVec(i).reduce(_|_)
    res.bits.waitStIdx := s1_reqB(i).ld_stIdx - tag(s1_reqHitIdx(i)).distance

    when(s1_reqHit(i)){
      accessIdx(i).valid := true.B
      accessIdx(i).bits := s1_reqHitIdx(i)
    }
  }})


  //----update:
  //S0: find the same entry
  //0   : redirect update
  //1,2 : load update
  val UPDATE_NUM = 3
  val REDIRECT_PORT_IDX_S = 0
  val LOAD_PORT_IDC_S = 1

  val s0_updateValidVec = Wire(Vec(UPDATE_NUM, Bool()))
  val s0_updateTagVec = Wire(Vec(UPDATE_NUM, new MDPEntry))
  val s0_updateHitVec = Wire(Vec(UPDATE_NUM, Vec(mdpSize, Bool())))
  val s0_updateUpVec = Wire(Vec(UPDATE_NUM, Bool()))

  val s1_updateValidVec = s0_updateValidVec.map(v => RegNext(v, false.B))
  val s1_updateTagVec = s0_updateTagVec.zip(s0_updateValidVec).map({ case t => RegEnable(t._1, t._2) })
  val s1_updateHitVec = s0_updateHitVec.zip(s0_updateValidVec).map({ case h => RegEnable(h._1, h._2) })
  val s1_updateUpVec = s0_updateUpVec.zip(s0_updateValidVec).map({ case u => RegEnable(u._1, u._2)})
  val s1_updateHitUIntVec = s1_updateHitVec.map(a => OHToUInt(a))
  val s1_updateHit = s1_updateHitVec.map(_.reduce(_ | _))

  s0_updateValidVec(REDIRECT_PORT_IDX_S) := io.reUpdate.valid &&
    !(s1_updateValidVec(REDIRECT_PORT_IDX_S) && s1_updateTagVec(REDIRECT_PORT_IDX_S).fold_pc === io.reUpdate.bits.ldFoldPc)
  s0_updateTagVec(REDIRECT_PORT_IDX_S).fold_pc := io.reUpdate.bits.ldFoldPc
  s0_updateTagVec(REDIRECT_PORT_IDX_S).distance := io.reUpdate.bits.distance
  s0_updateTagVec(REDIRECT_PORT_IDX_S).confidence := DontCare
  s0_updateUpVec(REDIRECT_PORT_IDX_S) := true.B
  (0 until LoadPipelineWidth).foreach({case i => {
    s0_updateValidVec(i + LOAD_PORT_IDC_S) := io.ldUpdate(i).valid
    s0_updateTagVec(i + LOAD_PORT_IDC_S).fold_pc := io.ldUpdate(i).bits.ldPC
    s0_updateTagVec(i + LOAD_PORT_IDC_S).distance := io.ldUpdate(i).bits.distance
    s0_updateTagVec(i + LOAD_PORT_IDC_S).confidence := DontCare
    s0_updateUpVec(i + LOAD_PORT_IDC_S) := !io.ldUpdate(i).bits.fail
  }})

  for(i <- 0 until UPDATE_NUM){
    for(j <- 0 until mdpSize){
      s0_updateHitVec(i)(j) := s0_updateTagVec(i).fold_pc === tag(j).fold_pc && s0_updateValidVec(i) && allocated(j)
    }

    when(s0_updateValidVec(i)){
      assert(PopCount(s0_updateHitVec(i)) <= 1.U,"can not hit more than 2 entry in update_" + i.toString)
    }
  }

  //S1: update entry
  for(i <- (0 until LoadPipelineWidth)){
    val portIdx = i + LOAD_PORT_IDC_S
    //if hit
    when(s1_updateValidVec(portIdx) && s1_updateHit(portIdx) && allocated(s1_updateHitUIntVec(portIdx))){
      val oldC = tag(s1_updateHitUIntVec(portIdx)).confidence
      when(s1_updateUpVec(portIdx) && oldC =/= FullConfidence()){
        tag(s1_updateHitUIntVec(portIdx)).confidence := oldC + 1.U
      }.elsewhen(!s1_updateUpVec(portIdx) && oldC =/= 1.U){
        tag(s1_updateHitUIntVec(portIdx)).confidence := oldC - 1.U
      }.elsewhen(!s1_updateUpVec(portIdx) && oldC === 1.U){
        allocated(s1_updateHitUIntVec(portIdx)) := false.B
      }
    }
  }

  for(i <- 0 until LOAD_PORT_IDC_S){
    //if hit
    when(s1_updateValidVec(i) && s1_updateHit(i) && allocated(s1_updateHitUIntVec(i))){
//      assert(allocated(s1_updateHitUIntVec(i)),"why disable?")

      val oldDis = tag(s1_updateHitUIntVec(i)).distance
      val newDis = s1_updateTagVec(i).distance

      accessIdx(2).valid := true.B
      accessIdx(2).bits := s1_updateHitUIntVec(i)
      tag(s1_updateHitUIntVec(i)).distance := Mux(oldDis < newDis, oldDis, newDis)
      tag(s1_updateHitUIntVec(i)).writeConFull()
    }
    //if miss
    when(s1_updateValidVec(i) && !s1_updateHit(i)) {
      allocated(replaceIdx) := true.B
      tag(replaceIdx).fold_pc := s1_updateTagVec(i).fold_pc
      tag(replaceIdx).distance := s1_updateTagVec(i).distance
      tag(replaceIdx).writeConFull()
    }
  }

  XSPerfAccumulate("MDP_allocate_num_greaterThan_16", PopCount(allocated) > 16.U)
  XSPerfAccumulate("MDP_allocate_full", PopCount(allocated) === mdpSize.U)
  for (i <- 0 until io.ldReq.length) {
    XSPerfAccumulate(s"mdp_req_${i}", io.ldReq(i).valid)
  }
}