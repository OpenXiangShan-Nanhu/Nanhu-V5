package xiangshan.backend.regfile

import chisel3._
import chisel3.util._

class VFRegFileBank(readPorts: Int, writePorts: Int, dataBits: Int, regsNum: Int, hasDebugPort: Boolean) extends Module {
  require(dataBits <= 64 && dataBits > 0)
  val io = IO(new Bundle {
    val rports = Vec(readPorts, new RfReadPort(dataBits, log2Ceil(regsNum)))
    val wports = Vec(writePorts, new RfWritePort(dataBits, log2Ceil(regsNum)))
  })

  val regs = Reg(Vec(regsNum, UInt(dataBits.W)))

  // Read
  for (i <- 0 until readPorts) {
    io.rports(i).data := regs(io.rports(i).addr)
  }

  // Write
  for(i <- 0 until regsNum) {
    val wenVec = Wire(Vec(writePorts, Bool()))
    assert(PopCount(wenVec.asUInt) <= 1.U, s"VRegFileBank write port conflict at reg $i")
    for(j <- 0 until writePorts) {
      wenVec(j) := io.wports(j).wen && (io.wports(j).addr === i.U)
    }
    when(wenVec.asUInt.orR) {
      regs(i) := Mux1H(wenVec, io.wports.map(_.data))
    }
  }

  val debug = IO(new Bundle {
    val debug_vec_num = if(hasDebugPort) 31 else 0
    val debug_fp_num = if(hasDebugPort) 32 else 0
    val vec_regs = Vec(debug_vec_num, new RfReadPort(dataBits, log2Ceil(regsNum)))
    val fp_regs = Vec(debug_fp_num, new RfReadPort(dataBits, log2Ceil(regsNum)))
  })

  // Debug
  debug.vec_regs.foreach {
    case vreg => {
      vreg.data :<= regs(vreg.addr)
    }
  }

  debug.fp_regs.foreach {
    case freg => {
      freg.data :<= regs(freg.addr)
    }
  }
}

class VFRegFile(readPorts: Int, writePorts: Int, VLEN: Int, regsNum: Int, bankNum: Int, hasDebugPort: Boolean) extends Module {
  val io = IO(new Bundle {
    val rports = Vec(readPorts, new RfReadPort(VLEN, log2Ceil(regsNum)))
    val wports = Vec(writePorts, new RfWritePort(VLEN, log2Ceil(regsNum)))
  })

  private val banks = Seq.fill(bankNum) {
    Module(new VFRegFileBank(readPorts, writePorts, VLEN / bankNum, regsNum, hasDebugPort))
  }

  // Connect read ports
  for (i <- 0 until readPorts) {
    val rdata = Wire(Vec(bankNum, UInt((VLEN / bankNum).W)))
    for (j <- 0 until bankNum) {
      banks(j).io.rports(i).addr := RegNext(io.rports(i).addr)
      rdata(j) := banks(j).io.rports(i).data
    }
    io.rports(i).data := rdata.asUInt
  }

  // Connect write ports
  for (i <- 0 until writePorts) {
    for (j <- 0 until bankNum) {
      banks(j).io.wports(i).wen := RegNext(io.wports(i).wen)
      banks(j).io.wports(i).addr := RegEnable(io.wports(i).addr, io.wports(i).wen)
      banks(j).io.wports(i).data := RegEnable(io.wports(i).data((j + 1) * VLEN / bankNum - 1, j * VLEN / bankNum), io.wports(i).wen)
    }
  }

  val debug = IO(new Bundle {
    val debug_vec_num = if(hasDebugPort) 31 else 0
    val debug_fp_num = if(hasDebugPort) 32 else 0
    val vec_regs = Vec(debug_vec_num, new RfReadPort(VLEN, log2Ceil(regsNum)))
    val fp_regs = Vec(debug_fp_num, new RfReadPort(64, log2Ceil(regsNum)))
  })

  // Debug
  debug.vec_regs.zipWithIndex.foreach {
    case (vreg, i) => {
      val rdata = Wire(Vec(bankNum, UInt((VLEN / bankNum).W)))
      for(j <- 0 until bankNum) {
        banks(j).debug.vec_regs(i).addr :<= vreg.addr
        rdata(j) :<= banks(j).debug.vec_regs(i).data
      }
      vreg.data :<= rdata.asUInt
    }
  }
  
  debug.fp_regs.zipWithIndex.foreach {
    case (freg, i) => {
      val rdata = Wire(Vec(bankNum * 64 / VLEN, UInt((VLEN / bankNum).W)))
      for(j <- 0 until bankNum * 64 / VLEN) {
        banks(j).debug.fp_regs(i).addr :<= freg.addr
        rdata(j) :<= banks(j).debug.fp_regs(i).data
      }
      for(j <- bankNum * 64 / VLEN until bankNum) {
        banks(j).debug.fp_regs(i).addr :<= DontCare
      }
      freg.data :<= rdata.asUInt
    }
  }
}

import _root_.circt.stage.{ChiselStage,FirtoolOption}
import chisel3.stage.ChiselGeneratorAnnotation
object EmitVRegFile extends App {
  (new ChiselStage).execute(
    Array("--target", "systemverilog") ++ args,
    Seq(ChiselGeneratorAnnotation(() => new VFRegFile(6, 4, 128, 175, 4, true)))
  )
}