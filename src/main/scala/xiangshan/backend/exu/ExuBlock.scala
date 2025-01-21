package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}
import xs.utils._
import xs.utils.perf._
import xiangshan.{HasXSParameter, Redirect, XSBundle}
import xiangshan.backend.Bundles._
import xiangshan.backend.issue.SchdBlockParams
import xiangshan.backend.fu.{CSRFileIO, FenceIO}
import xiangshan.backend.fu.FuConfig.{AluCfg, BrhCfg}
import xiangshan.backend.fu.vector.Bundles.{VType, Vxrm}
import xiangshan.backend.fu.fpu.Bundles.Frm
import xiangshan.backend.fu.wrapper.{CSRInput, CSRToDecode}
import scala.annotation.meta.param

class ExuBlock(params: SchdBlockParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false

  val exeUnits: Seq[Seq[ExeUnit]] = params.issueBlockParams.map(_.exuBlockParams.map(x => LazyModule(x.genExuModule)))
  val exus: Seq[ExeUnit] = exeUnits.flatten

  lazy val module = new ExuBlockImp(this)(p, params)
}

class ExuBlockImp(
  override val wrapper: ExuBlock
)(implicit
  p: Parameters,
  params: SchdBlockParams
) extends LazyModuleImp(wrapper) {
  val io = IO(new ExuBlockIO)

  override val desiredName = params.getExuBlockName

  private val exus = wrapper.exus.map(_.module)
  private val exeUnits = wrapper.exeUnits.map(x => x.map(xx => xx.module))

  private val ins: collection.IndexedSeq[DecoupledIO[ExuInput]] = io.in.flatten
  private val outs: collection.IndexedSeq[DecoupledIO[ExuOutput]] = io.out.flatten

  (ins zip exus zip outs).foreach { case ((input, exu), output) =>
    exu.io.flush <> io.flush
    exu.io.csrio.foreach(exuio => io.csrio.get <> exuio)
    exu.io.csrin.foreach(exuio => io.csrin.get <> exuio)
    exu.io.fenceio.foreach(exuio => io.fenceio.get <> exuio)
    exu.io.frm.foreach(exuio => exuio := RegNext(io.frm.get))  // each vf exu pipe frm from csr
    exu.io.vxrm.foreach(exuio => io.vxrm.get <> exuio)
    exu.io.vlIsZero.foreach(exuio => io.vlIsZero.get := exuio)
    exu.io.vlIsVlmax.foreach(exuio => io.vlIsVlmax.get := exuio)
    exu.io.vtype.foreach(exuio => io.vtype.get := exuio)
    exu.io.in <> input
    output <> exu.io.out
    io.csrToDecode.foreach(toDecode => exu.io.csrToDecode.foreach(exuOut => toDecode := exuOut))
    XSPerfAccumulate(s"${(exu.wrapper.exuParams.name)}_fire_cnt", PopCount(exu.io.in.fire))
  }
  exus.find(_.io.csrio.nonEmpty).map(_.io.csrio.get).foreach { csrio =>
    exus.map(_.io.instrAddrTransType.foreach(_ := csrio.instrAddrTransType))
  }

  exeUnits.zip(io.out).zip(params.issueBlockParams).foreach{
    case ((exes, out), param) => {
      if(param.sharedVf) {
        val wbMgu = Module(new SharedVfWbMgu(param.exuBlockParams.head))
        wbMgu.io.ins.head <> exes.head.io.out
        wbMgu.io.ins.last <> exes.last.io.out
        out.head <> wbMgu.io.outs.head
        out.last <> wbMgu.io.outs.last
        when(((exes.head.io.out.bits.vecWen.getOrElse(false.B) && exes.head.io.out.bits.vfWenL.getOrElse(false.B) && !exes.head.io.out.bits.vfWenH.getOrElse(true.B)) || 
                                    (exes.head.io.out.bits.v0Wen.getOrElse(false.B) && exes.head.io.out.bits.v0WenL.getOrElse(false.B) && !exes.head.io.out.bits.v0WenH.getOrElse(true.B)))) {
          out.head.bits.fflags.foreach(_ := exes.map(_.io.out.bits.fflags.get).reduce(_ | _))
        }
      }
    }
  }

  val aluFireSeq = exus.filter(_.wrapper.exuParams.fuConfigs.contains(AluCfg)).map(_.io.in.fire)
  for (i <- 0 until (aluFireSeq.size + 1)){
    XSPerfAccumulate(s"alu_fire_${i}_cnt", PopCount(aluFireSeq) === i.U)
  }
  val brhFireSeq = exus.filter(_.wrapper.exuParams.fuConfigs.contains(BrhCfg)).map(_.io.in.fire)
  for (i <- 0 until (brhFireSeq.size + 1)) {
    XSPerfAccumulate(s"brh_fire_${i}_cnt", PopCount(brhFireSeq) === i.U)
  }
}

class ExuBlockIO(implicit p: Parameters, params: SchdBlockParams) extends XSBundle {
  val flush = Flipped(ValidIO(new Redirect))
  // in(i)(j): issueblock(i), exu(j)
  val in: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = Flipped(params.genExuInputBundle)
  // out(i)(j): issueblock(i), exu(j).
  val out: MixedVec[MixedVec[DecoupledIO[ExuOutput]]] = params.genExuOutputDecoupledBundle

  val csrio = Option.when(params.hasCSR)(new CSRFileIO)
  val csrin = Option.when(params.hasCSR)(new CSRInput)
  val csrToDecode = Option.when(params.hasCSR)(Output(new CSRToDecode))

  val fenceio = Option.when(params.hasFence)(new FenceIO)
  val frm = Option.when(params.needSrcFrm)(Input(Frm()))
  val vxrm = Option.when(params.needSrcVxrm)(Input(Vxrm()))
  val vtype = Option.when(params.writeVConfig)((Valid(new VType)))
  val vlIsZero = Option.when(params.writeVConfig)(Output(Bool()))
  val vlIsVlmax = Option.when(params.writeVConfig)(Output(Bool()))
}
