package xiangshan.backend

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xs.utils._
import xs.utils.perf._
import xiangshan._
import xiangshan.backend.datapath.DataConfig.VAddrData
import xiangshan.frontend.{FtqPtr, FtqToCtrlIO, Ftq_RF_Components}
import xiangshan.frontend.FtqRead

class PcTargetMem(params: BackendParams)(implicit p: Parameters) extends LazyModule {
  override def shouldBeInlined: Boolean = false

  lazy val module = new PcTargetMemImp(this)(p, params)
}

class PcTargetMemImp(override val wrapper: PcTargetMem)(implicit p: Parameters, params: BackendParams) extends LazyModuleImp(wrapper) with HasXSParameter {

  require(params.numTargetReadPort == params.numPcMemReadPort, "The EXUs which need PC must be the same as the EXUs which need Target PC.")

  val pcMemRdIndexes = new NamedIndexes(Seq(
    "numTargetReadPort"  -> params.numTargetReadPort,
    "numPcMemReadPort"   -> params.numPcMemReadPort,
    "redirect"  -> 1,
    "memPred"   -> 1,
    "load"      -> params.LduCnt,
    "hybrid"    -> params.HyuCnt,
    "store"     -> (if(EnableStorePrefetchSMS) params.StaCnt else 0),
    "exception"     -> 1,
    "trace"      -> TraceGroupNum
  ))

  private val numPcMemReadForCtrlMem = pcMemRdIndexes.maxIdx

  // private val numTargetMemRead = params.numTargetReadPort + params.numPcMemReadPort + numPcMemReadForCtrlMem
  private val numTargetMemRead = pcMemRdIndexes.maxIdx
  private val numPcMemReadForExu = params.numPcReadPort
  println(s"pcMem read num: $numTargetMemRead")
  println(s"pcMem read num for exu: $numPcMemReadForExu")

  val io = IO(new PcTargetMemIO())
  private val readValid = io.toDataPath.fromDataPathValid

  private def hasRen: Boolean = true
  private val targetMem = Module(new SyncDataModuleTemplate(new Ftq_RF_Components, FtqSize, numTargetMemRead, 1, hasRen = hasRen))
  private val targetPCVec : Vec[UInt] = Wire(Vec(params.numTargetReadPort, UInt(VAddrData().dataWidth.W)))
  private val pcVec       : Vec[UInt] = Wire(Vec(params.numPcMemReadPort, UInt(VAddrData().dataWidth.W)))

  targetMem.io.wen.head := GatedValidRegNext(io.fromFrontendFtq.pc_mem_wen)
  targetMem.io.waddr.head := RegEnable(io.fromFrontendFtq.pc_mem_waddr, io.fromFrontendFtq.pc_mem_wen)
  targetMem.io.wdata.head := RegEnable(io.fromFrontendFtq.pc_mem_wdata, io.fromFrontendFtq.pc_mem_wen)

  private val newestEn: Bool = io.fromFrontendFtq.newest_entry_en
  private val newestTarget: UInt = io.fromFrontendFtq.newest_entry_target

  for ((pcExuIdx, i) <- pcMemRdIndexes("numTargetReadPort").zipWithIndex) {
    val targetPtr = io.toDataPath.fromDataPathFtqPtr(i)
    // target pc stored in next entry
    targetMem.io.ren.get(pcExuIdx) := readValid(i)
    targetMem.io.raddr(pcExuIdx) := (targetPtr + 1.U).value

    val needNewestTarget = RegEnable(targetPtr === io.fromFrontendFtq.newest_entry_ptr, false.B, readValid(i))
    targetPCVec(i) := Mux(
      needNewestTarget,
      RegEnable(newestTarget, newestEn),
      targetMem.io.rdata(pcExuIdx).startAddr
    )
  }

  for ((pcExuIdx, i) <- pcMemRdIndexes("numPcMemReadPort").zipWithIndex) {
    val pcAddr = io.toDataPath.fromDataPathFtqPtr(i)
    val offset = io.toDataPath.fromDataPathFtqOffset(i)
    targetMem.io.ren.get(pcExuIdx) := readValid(i)
    targetMem.io.raddr(pcExuIdx) := pcAddr.value
    pcVec(i) := targetMem.io.rdata(pcExuIdx).getPc(RegEnable(offset, readValid(i)))
  }

  targetMem.io.ren.get(pcMemRdIndexes("redirect").head) := io.toCtrl.redirectRead.valid
  targetMem.io.raddr(pcMemRdIndexes("redirect").head) := io.toCtrl.redirectRead.ptr.value
  io.toCtrl.redirectRead.data := targetMem.io.rdata(pcMemRdIndexes("redirect").head).getPc(RegEnable(io.toCtrl.redirectRead.offset, io.toCtrl.redirectRead.valid))
  targetMem.io.ren.get(pcMemRdIndexes("memPred").head) := io.toCtrl.memPredRead.valid
  targetMem.io.raddr(pcMemRdIndexes("memPred").head) := io.toCtrl.memPredRead.ptr.value
  io.toCtrl.memPredRead.data := targetMem.io.rdata(pcMemRdIndexes("memPred").head).getPc(RegEnable(io.toCtrl.memPredRead.offset, io.toCtrl.memPredRead.valid))
  targetMem.io.ren.get(pcMemRdIndexes("exception").head) := io.toCtrl.exceptionRead.valid
  targetMem.io.raddr(pcMemRdIndexes("exception").head) := io.toCtrl.exceptionRead.ptr.value
  io.toCtrl.exceptionRead.data := targetMem.io.rdata(pcMemRdIndexes("exception").head).getPc(RegEnable(io.toCtrl.exceptionRead.offset, io.toCtrl.exceptionRead.valid))

  for ((pcMemIdx, i) <- pcMemRdIndexes("load").zipWithIndex) {
    targetMem.io.ren.get(pcMemIdx) := io.toMem.memLdPcRead(i).valid
    targetMem.io.raddr(pcMemIdx) := io.toMem.memLdPcRead(i).ptr.value
    io.toMem.memLdPcRead(i).data := targetMem.io.rdata(pcMemIdx).getPc(RegEnable(io.toMem.memLdPcRead(i).offset, io.toMem.memLdPcRead(i).valid))
  }

  for ((pcMemIdx, i) <- pcMemRdIndexes("hybrid").zipWithIndex) {
    targetMem.io.ren.get(pcMemIdx) := io.toMem.memHyPcRead(i).valid
    targetMem.io.raddr(pcMemIdx) := io.toMem.memHyPcRead(i).ptr.value
    io.toMem.memHyPcRead(i).data := targetMem.io.rdata(pcMemIdx).getPc(RegEnable(io.toMem.memHyPcRead(i).offset, io.toMem.memHyPcRead(i).valid))
  }

  if (EnableStorePrefetchSMS) {
    for ((pcMemIdx, i) <- pcMemRdIndexes("store").zipWithIndex) {
      targetMem.io.ren.get(pcMemIdx) := io.toMem.memStPcRead(i).valid
      targetMem.io.raddr(pcMemIdx) := io.toMem.memStPcRead(i).ptr.value
      io.toMem.memStPcRead(i).data := targetMem.io.rdata(pcMemIdx).getPc(RegEnable(io.toMem.memStPcRead(i).offset, io.toMem.memStPcRead(i).valid))
    }
  } else {
    io.toMem.memStPcRead.foreach(_.data := 0.U)
  }

  for ((pcMemIdx, i) <- pcMemRdIndexes("trace").zipWithIndex) {
    targetMem.io.ren.get(pcMemIdx) := io.toCtrl.tracePcRead(i).valid
    targetMem.io.raddr(pcMemIdx) := io.toCtrl.tracePcRead(i).ptr.value
    io.toCtrl.tracePcRead(i).data := targetMem.io.rdata(pcMemIdx).getPc(RegEnable(io.toCtrl.tracePcRead(i).offset, io.toCtrl.tracePcRead(i).valid))
  }

  io.toDataPath.toDataPathTargetPC := targetPCVec
  io.toDataPath.toDataPathPC := pcVec
}

class PcToDataPathIO(params: BackendParams)(implicit p: Parameters) extends XSBundle {
  //Ftq
  val fromDataPathValid = Input(Vec(params.numPcMemReadPort, Bool()))
  val fromDataPathFtqPtr = Input(Vec(params.numPcMemReadPort, new FtqPtr))
  val fromDataPathFtqOffset = Input(Vec(params.numPcMemReadPort, UInt(log2Up(PredictWidth).W)))
  //Target PC
  val toDataPathTargetPC = Output(Vec(params.numTargetReadPort, UInt(VAddrData().dataWidth.W)))
  //PC
  val toDataPathPC = Output(Vec(params.numPcMemReadPort, UInt(VAddrData().dataWidth.W)))
}

class PcToCtrlIO(params: BackendParams)(implicit p: Parameters) extends XSBundle {
  val redirectRead = Flipped(new FtqRead(UInt(VAddrBits.W)))
  val memPredRead = Flipped(new FtqRead(UInt(VAddrBits.W)))
  val exceptionRead = Flipped(new FtqRead(UInt(VAddrBits.W)))
  val tracePcRead = Vec(TraceGroupNum, Flipped(new FtqRead(UInt(VAddrBits.W))))
}

class PcToMemIO(params: BackendParams)(implicit p: Parameters) extends XSBundle {
  val memLdPcRead = Vec(params.LduCnt, Flipped(new FtqRead(UInt(VAddrBits.W))))
  val memStPcRead = Vec(params.StaCnt, Flipped(new FtqRead(UInt(VAddrBits.W))))
  val memHyPcRead = Vec(params.HyuCnt, Flipped(new FtqRead(UInt(VAddrBits.W))))
}

class PcTargetMemIO()(implicit p: Parameters, params: BackendParams) extends XSBundle {
  //from frontend
  val fromFrontendFtq = Flipped(new FtqToCtrlIO)
  //to backend
  val toDataPath = new PcToDataPathIO(params)
  val toMem = new PcToMemIO(params)
  val toCtrl = new PcToCtrlIO(params)
}