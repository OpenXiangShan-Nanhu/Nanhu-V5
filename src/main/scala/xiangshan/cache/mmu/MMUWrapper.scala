package xiangshan.cache.mmu

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.TLBuffer
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.{HasMemBlockParameters, MemBlockInlinedImp}
import xiangshan.backend.fu.{PMP, PMPChecker, PMPEntry, PMPReqBundle, PMPRespBundle}
import xiangshan.{CustomCSRCtrlIO, DistributedCSRIO, HasPMParameters, HasXSParameter, Redirect, SfenceBundle, TlbCsrBundle, XSBundle, XSModule}
import xiangshan.frontend.icache.{ICachePMPBundle, ICacheParameters}
import xs.utils.DelayN


class PMPWrapper(implicit p: Parameters) extends XSModule with HasXSParameter{
  val io = IO(new Bundle {
    val distribute_csr = Input(new DistributedCSRIO())
    val imode = Input(UInt(2.W))
    val pmpIO = Vec(coreParams.ipmpPortNum, Flipped(new ICachePMPBundle))
  })

  val pmp = Module(new PMP())
  val pmpChecker = Seq.fill(coreParams.ipmpPortNum)(Module(new PMPChecker(3, sameCycle = true)))
  pmp.io.distribute_csr := io.distribute_csr
  pmpChecker.zip(io.pmpIO).foreach { case (checker, pmpio) =>
    checker.io.apply(io.imode, pmp.io.pmp, pmp.io.pma, pmpio.req)
    pmpio.resp := checker.io.resp
  }
}


class MemBlockPMPWrapper(pmpCheckSize : Int, lgMaxSize : Int = 4)(implicit p: Parameters) extends XSModule with HasPMParameters {
  val io = IO(new Bundle {
    val distribute_csr = Input(new DistributedCSRIO())
    val pmpInfo = new Bundle {
      val pmp = Output(Vec(NumPMP, new PMPEntry()))
      val pma = Output(Vec(NumPMA, new PMPEntry()))
    }
    val privDmode = Input(UInt(2.W))
    val dtlbPmps = Vec(pmpCheckSize, Flipped(ValidIO(new PMPReqBundle(lgMaxSize))))
    val pmpCheckerResp = Vec(pmpCheckSize, new PMPRespBundle)
  })

  val pmp = Module(new PMP())
  val pmp_checkers = Seq.fill(pmpCheckSize)(Module(new PMPChecker(lgMaxSize, leaveHitMux = true)))

  for ((p, d) <- pmp_checkers zip io.dtlbPmps) {
    p.io.apply(io.privDmode, io.pmpInfo.pmp, io.pmpInfo.pma, d)
    require(p.io.req.bits.size.getWidth == d.bits.size.getWidth)
  }

  for ((p,res) <- pmp_checkers zip io.pmpCheckerResp){
    res := p.io.resp
  }

  io.pmpInfo.pmp := pmp.io.pmp
  io.pmpInfo.pma := pmp.io.pma
  pmp.io.distribute_csr := io.distribute_csr
}

//frontend to mmu bundle
class frontendMMUBundle(implicit p: Parameters) extends XSBundle{
  val pmpIO = Vec(coreParams.ipmpPortNum, new ICachePMPBundle)
  val itlb = Vec(coreParams.itlbPortNum, new TlbRequestIO(1))
  val itlbFlushPipe = Output(Bool())
}



class MMUWrapper(implicit p: Parameters) extends LazyModule with HasMemBlockParameters {
  override def shouldBeInlined: Boolean = false

  val ptw = LazyModule(new L2TLBWrapper())
  val ptw_to_l2_buffer = if (!coreParams.softPTW) LazyModule(new TLBuffer) else null

  lazy val module = new MMU(this)

  if(!coreParams.softPTW){
    ptw_to_l2_buffer.node := ptw.node
  }
}

class MMU(outer: MMUWrapper) extends LazyModuleImp(outer) with HasMemBlockParameters {
  val pmpCheckSize = 6 //todo
  val nRespDups = 2
  val ldExuNum = 2

  val io = IO(new Bundle(){
    val hartId = Input(UInt(hartIdLen.W))
    val csr = new Bundle {
      val csrCtrl = Input(new CustomCSRCtrlIO)
      val sfence = Input(new SfenceBundle)
      val tlbCsr = Input(new TlbCsrBundle)
    }

    val frontend = Flipped(new frontendMMUBundle)

    val memblock = new Bundle {
      val redirect = Flipped(ValidIO(new Redirect))
      val pmpCheckResp = Vec(pmpCheckSize, new PMPRespBundle)
      val tlb = Vec(pmpCheckSize, Flipped(new TlbRequestIO(nRespDups)))

      val lduHint = new TlbHintIO
      val lqHint = new TlbHintIO
    }
  })

  val FrontendPortNumber = ICacheParameters().PortNumber
  val DTLB_PORT_NUM = LduCnt + StaCnt + 1 + 2 //+ bop + sms + stride
  val DTLB_PORT_START_LD = 0
  val DTLB_PORT_START_ST = LduCnt
  val DTLB_PORT_START_BOP = LduCnt + StaCnt
  val DTLB_PORT_START_SMS = LduCnt + StaCnt + 1
  val DTLB_PORT_START_STRIDE = LduCnt + StaCnt + 2

  val tlbCsr = DelayN(io.csr.tlbCsr, 2)
  val csrCtrl = DelayN(io.csr.csrCtrl, 2)
  val sfence = RegNext(RegNext(io.csr.sfence))

  val iPMPWrapper = Module(new PMPWrapper)
  val iTLBWrapper = Module(new TLB(coreParams.itlbPortNum, 1, Seq.fill(FrontendPortNumber)(false) ++ Seq(true), itlbParams))
  val iTLBPtw = Wire(new VectorTlbPtwIO(coreParams.itlbPortNum))  //from itlb
  val frontendPtw = Wire(new TlbPtwIO())  //from repeater2
  val itlbRepeater1 = PTWFilter(itlbParams.fenceDelay, iTLBPtw, sfence, tlbCsr, l2tlbParams.ifilterSize)
  val itlbRepeater2 = PTWRepeaterNB(passReady = false, itlbParams.fenceDelay, itlbRepeater1.io.ptw, frontendPtw, sfence, tlbCsr)

  //pmp connect
  iPMPWrapper.io.distribute_csr := csrCtrl.distribute_csr
  iPMPWrapper.io.imode := tlbCsr.priv.imode
  iPMPWrapper.io.pmpIO <> io.frontend.pmpIO
  //itlb connect
  iTLBWrapper.io.hartId := io.hartId
  io.frontend.itlb <> iTLBWrapper.io.requestor
  iTLBWrapper.io.sfence := sfence
  iTLBWrapper.io.csr := tlbCsr
  iTLBWrapper.io.flushPipe.foreach(_ := io.frontend.itlbFlushPipe)
  iTLBWrapper.io.redirect := DontCare

  //itlb ptw connect
  iTLBPtw.req <> iTLBWrapper.io.ptw.req
  iTLBPtw.resp.ready := iTLBWrapper.io.ptw.resp.ready
  iTLBWrapper.io.ptw.resp.bits := iTLBPtw.resp.bits.data
  iTLBWrapper.io.ptw.resp.valid := iTLBPtw.resp.valid

  //other
  itlbRepeater1.io.debugTopDown := DontCare

  private val ptw = outer.ptw.module


  ptw.io.hartId := io.hartId
  ptw.io.sfence := sfence
  ptw.io.csr.tlb := tlbCsr
  ptw.io.csr.distribute_csr <> csrCtrl.distribute_csr

  //memblock pmp
  val dPmpWrapper = Module(new MemBlockPMPWrapper(DTLB_PORT_NUM))
  io.memblock.pmpCheckResp := dPmpWrapper.io.pmpCheckerResp

  val dTLBWrapper = Module(new TLBNonBlock(DTLB_PORT_NUM, 2, dtlbParams))
  val dtlbIO = dTLBWrapper.io
  val dptwio = Wire(new VectorTlbPtwIO(DTLB_PORT_NUM))
  val dtlb_reqs = dtlbIO.requestor
  val dtlb_pmps = dtlbIO.pmp


  io.memblock.tlb <> dtlb_reqs
  dPmpWrapper.io.privDmode := tlbCsr.priv.dmode
  dPmpWrapper.io.distribute_csr <> csrCtrl.distribute_csr
  dPmpWrapper.io.dtlbPmps := dtlb_pmps
  ptw.io.pmpInfo.pmp := dPmpWrapper.io.pmpInfo.pmp
  ptw.io.pmpInfo.pma := dPmpWrapper.io.pmpInfo.pma


  dtlbIO.hartId := io.hartId
  dtlbIO.sfence := sfence
  dtlbIO.csr := tlbCsr
  dtlbIO.flushPipe.map(a => a := false.B) // non-block doesn't need
  dtlbIO.redirect := io.memblock.redirect

  val ptw_resp_next = RegEnable(dptwio.resp.bits, dptwio.resp.valid)
  val pte_resp_next_data_dup = ptw_resp_next.data
  val sfenceDelay = RegNext(RegNext(sfence))
  val ptw_resp_v = RegNext(dptwio.resp.valid && !(sfenceDelay.valid || tlbCsr.satp.changed || tlbCsr.vsatp.changed || tlbCsr.hgatp.changed), init = false.B)
  dptwio.resp.ready := true.B

  val tlbreplay = WireInit(VecInit(Seq.fill(LdExuCnt)(false.B)))
  val tlbreplay_reg = RegNext(tlbreplay)
  val dtlb_ld0_tlbreplay_reg = RegNext(dtlbIO.tlbreplay)

  dontTouch(tlbreplay)
  for (i <- 0 until LdExuCnt) {
    tlbreplay(i) := dtlbIO.ptw.req(i).valid && ptw_resp_next.vector(0) && ptw_resp_v &&
      ptw_resp_next.data.hit(dtlbIO.ptw.req(i).bits.vpn, tlbCsr.satp.asid, tlbCsr.vsatp.asid, tlbCsr.hgatp.vmid, allType = true, ignoreAsid = true)
  }


  dtlbIO.ptw.req.map(r => r)
    .zipWithIndex
    .foreach {
      case (tlb, i) =>
        val vector_hit = Cat(ptw_resp_next.vector).orR
        tlb.ready := dptwio.req(i).ready
        dptwio.req(i).bits := tlb.bits
        dptwio.req(i).valid := tlb.valid && !(ptw_resp_v && vector_hit && pte_resp_next_data_dup.hit(tlb.bits.vpn, tlbCsr.satp.asid, tlbCsr.vsatp.asid, tlbCsr.hgatp.vmid, allType = true, ignoreAsid = true))
    }

  dtlbIO.ptw.resp.bits := pte_resp_next_data_dup
  dtlbIO.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector).orR
  dtlbIO.ptw.resp.bits.getGpa := false.B //todo tmp

  val dtlbRepeater = PTWNewFilter(ldtlbParams.fenceDelay, dptwio, ptw.io.tlb(1), sfence, tlbCsr, l2tlbParams.dfilterSize)
  val itlbRepeater3 = PTWRepeaterNB(passReady = false, itlbParams.fenceDelay, frontendPtw, ptw.io.tlb(0), sfence, tlbCsr)

  for(i <- 0 until ldExuNum){
    io.memblock.lduHint.req(i).id := dtlbRepeater.io.hint.get.req(i).id
    io.memblock.lduHint.req(i).full := dtlbRepeater.io.hint.get.req(i).full | tlbreplay_reg(i) | dtlb_ld0_tlbreplay_reg(i)
  }
  io.memblock.lqHint := dtlbRepeater.io.hint.get


  io.memblock.lduHint.resp := DontCare
  //todo
  dtlbRepeater.io.debugTopDown.robHeadVaddr := DontCare
}
