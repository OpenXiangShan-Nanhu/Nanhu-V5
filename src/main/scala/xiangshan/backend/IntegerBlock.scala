package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.exu.Exu.{jumpExeUnitCfg, ldExeUnitCfg, stExeUnitCfg}
import xiangshan.backend.exu.{AluExeUnit, ExuConfig, JumpExeUnit, MulDivExeUnit, Wb}
import xiangshan.backend.fu.FenceToSbuffer
import xiangshan.backend.issue.ReservationStationNew
import xiangshan.backend.regfile.Regfile

class WakeUpBundle(numFast: Int, numSlow: Int) extends XSBundle {
  val fastUops = Vec(numFast, Flipped(ValidIO(new MicroOp)))
  val fast = Vec(numFast, Flipped(DecoupledIO(new ExuOutput))) //one cycle later than fastUops
  val slow = Vec(numSlow, Flipped(DecoupledIO(new ExuOutput)))

  override def cloneType = (new WakeUpBundle(numFast, numSlow)).asInstanceOf[this.type]

}

class IntBlockToCtrlIO extends XSBundle {
  // write back regfile signals after arbiter
  // used to update busytable and roq state
  val wbRegs = Vec(NRIntWritePorts, ValidIO(new ExuOutput))

  // write back to brq
  val exuRedirect = Vec(exuParameters.AluCnt+exuParameters.JmpCnt, ValidIO(new ExuOutput))

  val numExist = Vec(exuParameters.IntExuCnt, Output(UInt(log2Ceil(IssQueSize).W)))
  val sfence = Output(new SfenceBundle)
  val tlbCsrIO = Output(new TlbCsrBundle)
}

trait HasExeBlockHelper {
  def fpFastFilter(cfg: ExuConfig): Boolean = {
    cfg.hasCertainLatency && cfg.writeFpRf
  }
  def fpSlowFilter(cfg: ExuConfig): Boolean = {
    cfg.hasUncertainlatency && cfg.writeFpRf
  }
  def intFastFilter(cfg: ExuConfig): Boolean = {
    cfg.hasCertainLatency && cfg.writeIntRf
  }
  def intSlowFilter(cfg: ExuConfig): Boolean = {
    cfg.hasUncertainlatency && cfg.writeIntRf
  }
}

class IntegerBlock
(
  fastWakeUpIn: Seq[ExuConfig],
  slowWakeUpIn: Seq[ExuConfig],
  fastFpOut: Seq[ExuConfig],
  slowFpOut: Seq[ExuConfig],
  fastIntOut: Seq[ExuConfig],
  slowIntOut: Seq[ExuConfig]
) extends XSModule with HasExeBlockHelper with NeedImpl
{
  val io = IO(new Bundle {
    val fromCtrlBlock = Flipped(new CtrlToIntBlockIO)
    val toCtrlBlock = new IntBlockToCtrlIO

    val wakeUpIn = new WakeUpBundle(fastWakeUpIn.size, slowWakeUpIn.size)
    val wakeUpFpOut = Flipped(new WakeUpBundle(fastFpOut.size, slowFpOut.size))
    val wakeUpIntOut = Flipped(new WakeUpBundle(fastIntOut.size, slowIntOut.size))

    val externalInterrupt = new ExternalInterruptIO
    val sfence = Output(new SfenceBundle)
    val fencei = Output(Bool())
    val tlbCsrIO = Output(new TlbCsrBundle)
    val csrOnly = new CSRSpecialIO
    val fenceToSbuffer = new FenceToSbuffer
  })

  val redirect = io.fromCtrlBlock.redirect

  val intRf = Module(new Regfile(
    numReadPorts = NRIntReadPorts,
    numWirtePorts = NRIntWritePorts,
    hasZero = true,
    len = XLEN
  ))

  val aluExeUnits = Array.tabulate(exuParameters.AluCnt)(_ => Module(new AluExeUnit))
  val jmpExeUnit = Module(new JumpExeUnit)
  val mduExeUnits = Array.tabulate(exuParameters.MduCnt)(_ => Module(new MulDivExeUnit))

  val exeUnits = jmpExeUnit +: (aluExeUnits ++ mduExeUnits)

  def needWakeup(cfg: ExuConfig): Boolean =
    (cfg.readIntRf && cfg.writeIntRf) || (cfg.readFpRf && cfg.writeFpRf)

  def needData(a: ExuConfig, b: ExuConfig): Boolean =
    (a.readIntRf && b.writeIntRf) || (a.readFpRf && b.writeFpRf)

  val reservedStations = exeUnits.map(_.config).zipWithIndex.map({ case (cfg, i) =>
    var certainLatency = -1
    if (cfg.hasCertainLatency) {
      certainLatency = cfg.latency.latencyVal.get
    }

    val readIntRf = cfg.readIntRf

    val inBlockWbData = exeUnits.filter(e => e.config.hasCertainLatency && readIntRf).map(_.io.toInt.bits.data)
    val writeBackData = inBlockWbData ++ io.wakeUpIn.fast.map(_.bits.data)
    val wakeupCnt = writeBackData.length

    val inBlockListenPorts = exeUnits.filter(e => e.config.hasUncertainlatency && readIntRf).map(_.io.toInt)
    val extraListenPorts = inBlockListenPorts ++ io.wakeUpIn.slow
    val extraListenPortsCnt = extraListenPorts.length

    val feedback = (cfg == ldExeUnitCfg) || (cfg == stExeUnitCfg)

    println(s"${i}: exu:${cfg.name} wakeupCnt: ${wakeupCnt} extraListenPorts: ${extraListenPortsCnt} delay:${certainLatency} feedback:${feedback}")

    val rs = Module(new ReservationStationNew(
      cfg, wakeupCnt, extraListenPortsCnt, fixedDelay = certainLatency, feedback = feedback
    ))

    rs.io.redirect <> redirect
    rs.io.numExist <> io.toCtrlBlock.numExist(i)
    rs.io.enqCtrl <> io.fromCtrlBlock.enqIqCtrl(i)
    rs.io.enqData <> io.fromCtrlBlock.enqIqData(i)

    rs.io.writeBackedData <> writeBackData
    for ((x, y) <- rs.io.extraListenPorts.zip(extraListenPorts)) {
      x.valid := y.fire()
      x.bits := y.bits
    }

    exeUnits(i).io.redirect <> redirect
    exeUnits(i).io.fromInt <> rs.io.deq
    rs.io.tlbFeedback := DontCare

    rs.suggestName(s"rs_${cfg.name}")

    rs
  })

  for(rs <- reservedStations){
    val inBlockUops = reservedStations.filter(x =>
      x.exuCfg.hasCertainLatency && x.exuCfg.writeIntRf
    ).map(x => {
      val raw = WireInit(x.io.selectedUop)
      raw.valid := x.io.selectedUop.valid && raw.bits.ctrl.rfWen
      raw
    })
    rs.io.broadcastedUops <> inBlockUops ++ io.wakeUpIn.fastUops
  }

  io.wakeUpFpOut.fastUops <> reservedStations.filter(
    rs => fpFastFilter(rs.exuCfg)
  ).map(_.io.selectedUop)

  io.wakeUpFpOut.fast <> exeUnits.filter(
    x => fpFastFilter(x.config)
  ).map(_.io.toFp)

  io.wakeUpFpOut.slow <> exeUnits.filter(
    x => fpSlowFilter(x.config)
  ).map(_.io.toFp)

  io.wakeUpIntOut.fastUops <> reservedStations.filter(
    rs => intFastFilter(rs.exuCfg)
  ).map(_.io.selectedUop)

  io.wakeUpIntOut.fast <> exeUnits.filter(
    x => intFastFilter(x.config)
  ).map(_.io.toInt)

  io.wakeUpIntOut.slow <> exeUnits.filter(
    x => intSlowFilter(x.config)
  ).map(_.io.toInt)

  // send misprediction to brq
  io.toCtrlBlock.exuRedirect.zip(
    exeUnits.filter(_.config.hasRedirect).map(_.io.toInt)
  ).foreach{
    case (x, y) =>
      x.valid := y.fire() && y.bits.redirectValid
      x.bits := y.bits
  }

  io.csrOnly <> jmpExeUnit.csrOnly
  io.toCtrlBlock.sfence <> jmpExeUnit.sfence
  io.toCtrlBlock.tlbCsrIO <> jmpExeUnit.tlbCsrIO
  jmpExeUnit.fflags <> io.fromCtrlBlock.roqToCSR.fflags
  jmpExeUnit.dirty_fs <> io.fromCtrlBlock.roqToCSR.dirty_fs
  jmpExeUnit.fenceToSbuffer <> io.fenceToSbuffer

  // read int rf from ctrl block
  intRf.io.readPorts <> io.fromCtrlBlock.readRf
  // write int rf arbiter
  val intWbArbiter = Module(new Wb(
    (exeUnits.map(_.config) ++ fastWakeUpIn ++ slowWakeUpIn).map(_.wbIntPriority),
    NRIntWritePorts
  ))
  intWbArbiter.io.in <> exeUnits.map(_.io.toInt) ++ io.wakeUpIn.fast ++ io.wakeUpIn.slow

  // set busytable and update roq
  io.toCtrlBlock.wbRegs <> intWbArbiter.io.out

  intRf.io.writePorts.zip(intWbArbiter.io.out).foreach{
    case (rf, wb) =>
      rf.wen := wb.valid && wb.bits.uop.ctrl.rfWen
      rf.addr := wb.bits.uop.pdest
      rf.data := wb.bits.data
  }
}
