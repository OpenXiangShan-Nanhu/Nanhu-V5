/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.backend.regfile

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.exu.ExeUnitParams

class RfReadPort(dataWidth: Int, addrWidth: Int) extends Bundle {
  val addr = Input(UInt(addrWidth.W))
  val data = Output(UInt(dataWidth.W))
}

class RfWritePort(dataWidth: Int, addrWidth: Int) extends Bundle {
  val wen = Input(Bool())
  val addr = Input(UInt(addrWidth.W))
  val data = Input(UInt(dataWidth.W))
}

class RfReadPortWithConfig(addrWidth: Int) extends Bundle {
  val addr: UInt = Input(UInt(addrWidth.W))
  val srcType: UInt = Input(UInt(3.W))
}

class RfWritePortWithConfig(pregParams: PregParams) extends Bundle {
  val addrWidth = pregParams.addrWidth
  val dataWidth = pregParams.dataCfg.map(_.dataWidth).max

  val wen = Input(Bool())
  val addr = Input(UInt(addrWidth.W))
  val data = Input(UInt(dataWidth.W))
  val intWen = Input(Bool())
  val fpWen = Input(Bool())
  val vecWen = Input(Bool())
  val v0Wen = Input(Bool())
  val vlWen = Input(Bool())
}

class Regfile
(
  name: String,
  numPregs: Int,
  numReadPorts: Int,
  numWritePorts: Int,
  hasZero: Boolean,
  len: Int,
  width: Int,
  bankNum: Int = 1,
  isVlRegfile: Boolean = false,
) extends Module {
  val io = IO(new Bundle() {
    val readPorts = Vec(numReadPorts, new RfReadPort(len, width))
    val writePorts = Vec(numWritePorts, new RfWritePort(len, width))
    val debug_rports = Vec(33, new RfReadPort(len, width))
    val extra_debug_rports = Vec(33, new RfReadPort(len, width))
  })
  override def desiredName = name
  println(name + ": size:" + numPregs + " read: " + numReadPorts + " write: " + numWritePorts)

  val mem_0 = if (isVlRegfile) RegInit(0.U(len.W)) else Reg(UInt(len.W))
  val mem = Reg(Vec(numPregs, UInt(len.W)))
  val memForRead = Wire(Vec(numPregs, UInt(len.W)))
  memForRead.zipWithIndex.map{ case(m, i) =>
    if (i == 0) m := mem_0
    else m := mem(i)
  }
  require(Seq(1, 2, 4).contains(bankNum), "bankNum must be 1 or 2 or 4")
  for (r <- io.readPorts) {
    if (bankNum == 1) {
      r.data := memForRead(RegNext(r.addr))
    }
    else {
      val banks = (0 until bankNum).map { case i =>
        memForRead.zipWithIndex.filter{ case (m, index) => (index % bankNum) == i }.map(_._1)
      }
      val bankWidth = bankNum.U.getWidth - 1
      val hitBankWire = VecInit((0 until bankNum).map { case i => r.addr(bankWidth - 1, 0) === i.U })
      val hitBankReg = Reg(Vec(bankNum, Bool()))
      hitBankReg := hitBankWire
      val banksRdata = Wire(Vec(bankNum, UInt(len.W)))
      for (i <- 0 until bankNum) {
        banksRdata(i) := RegEnable(VecInit(banks(i))(r.addr(r.addr.getWidth - 1, bankWidth)), hitBankWire(i))
      }
      r.data := Mux1H(hitBankReg, banksRdata)
    }
  }
  val writePorts = io.writePorts
  for (i <- writePorts.indices) {
    if (i < writePorts.size-1) {
      val hasSameWrite = writePorts.drop(i + 1).map(w => w.wen && w.addr === writePorts(i).addr && writePorts(i).wen).reduce(_ || _)
      assert(!hasSameWrite, "RegFile two or more writePorts write same addr")
    }
  }
  for (i <- mem.indices) {
    if (hasZero && i == 0) {
      mem_0 := 0.U
    }
    else {
      val wenOH = VecInit(io.writePorts.map(w => w.wen && w.addr === i.U))
      val wData = Mux1H(wenOH, io.writePorts.map(_.data))
      when(wenOH.asUInt.orR) {
        if (i == 0) mem_0 := wData
        else mem(i) := wData
      }
    }
  }

  for (rport <- io.debug_rports) {
    rport.data := memForRead(rport.addr)
  }
  for (rport <- io.extra_debug_rports) {
    rport.data := memForRead(rport.addr)
  }
}

object Regfile {
  // non-return version
  def apply(
    name         : String,
    numEntries   : Int,
    raddr        : Seq[UInt],
    rdata        : Vec[UInt],
    wen          : Seq[Bool],
    waddr        : Seq[UInt],
    wdata        : Seq[UInt],
    hasZero      : Boolean,
    withReset    : Boolean = false,
    bankNum      : Int = 1,
    debugReadAddr: Option[Seq[UInt]],
    debugReadData: Option[Vec[UInt]],
    extradebugReadAddr: Option[Seq[UInt]],
    extradebugReadData: Option[Vec[UInt]],
    isVlRegfile  : Boolean = false,
  )(implicit p: Parameters): Unit = {
    val numReadPorts = raddr.length
    val numWritePorts = wen.length
    require(wen.length == waddr.length)
    require(wen.length == wdata.length)
    val dataBits = wdata.map(_.getWidth).min
    require(wdata.map(_.getWidth).min == wdata.map(_.getWidth).max, s"dataBits != $dataBits")
    val addrBits = waddr.map(_.getWidth).min
    require(waddr.map(_.getWidth).min == waddr.map(_.getWidth).max, s"addrBits != $addrBits")

    val instanceName = name(0).toLower.toString() + name.drop(1)
    require(instanceName != name, "Regfile Instance Name can't be same as Module name")
    val regfile = Module(new Regfile(name, numEntries, numReadPorts, numWritePorts, hasZero, dataBits, addrBits, bankNum, isVlRegfile)).suggestName(instanceName)
    rdata := regfile.io.readPorts.zip(raddr).map { case (rport, addr) =>
      rport.addr := addr
      rport.data
    }

    regfile.io.writePorts.zip(wen).zip(waddr).zip(wdata).foreach{ case (((wport, en), addr), data) =>
      wport.wen := en
      wport.addr := addr
      wport.data := data
    }
    if (withReset) {
      val numResetCycles = math.ceil(numEntries / numWritePorts).toInt
      val resetCounter = RegInit(numResetCycles.U)
      val resetWaddr = RegInit(VecInit((0 until numWritePorts).map(_.U(log2Up(numEntries + 1).W))))
      val inReset = resetCounter =/= 0.U
      when (inReset) {
        resetCounter := resetCounter - 1.U
        resetWaddr := VecInit(resetWaddr.map(_ + numWritePorts.U))
      }
      when (!inReset) {
        resetWaddr.map(_ := 0.U)
      }
      for ((wport, i) <- regfile.io.writePorts.zipWithIndex) {
        wport.wen := inReset || wen(i)
        wport.addr := Mux(inReset, resetWaddr(i), waddr(i))
        wport.data := wdata(i)
      }
    }

    require(debugReadAddr.nonEmpty == debugReadData.nonEmpty, "Both debug addr and data bundles should be empty or not")
    require(extradebugReadAddr.nonEmpty == extradebugReadData.nonEmpty, "Both debug addr and data bundles should be empty or not")
    regfile.io.debug_rports := DontCare
    regfile.io.extra_debug_rports := DontCare
    if (debugReadAddr.nonEmpty && debugReadData.nonEmpty) {
      debugReadData.get := VecInit(regfile.io.debug_rports.zip(debugReadAddr.get).map { case (rport, addr) =>
        rport.addr := addr
        rport.data
      })
      extradebugReadData.get := VecInit(regfile.io.extra_debug_rports.zip(extradebugReadAddr.get).map { case (rport, addr) =>
        rport.addr := addr
        rport.data
      })
    }
  }
}

object IntRegFile {
  // non-return version
  def apply(
    name         : String,
    numEntries   : Int,
    raddr        : Seq[UInt],
    rdata        : Vec[UInt],
    wen          : Seq[Bool],
    waddr        : Seq[UInt],
    wdata        : Seq[UInt],
    debugReadAddr: Option[Seq[UInt]],
    debugReadData: Option[Vec[UInt]],
    withReset    : Boolean = false,
    bankNum      : Int,
  )(implicit p: Parameters): Unit = {
    Regfile(
      name, numEntries, raddr, rdata, wen, waddr, wdata,
      hasZero = true, withReset, bankNum, debugReadAddr, debugReadData, debugReadAddr, debugReadData)
  }
}

object FpRegFile {
  // non-return version
  def apply(
             name         : String,
             numEntries   : Int,
             raddr        : Seq[UInt],
             rdata        : Vec[UInt],
             wen          : Seq[Bool],
             waddr        : Seq[UInt],
             wdata        : Seq[UInt],
             debugReadAddr: Option[Seq[UInt]],
             debugReadData: Option[Vec[UInt]],
             withReset    : Boolean = false,
             bankNum      : Int,
             isVlRegfile  : Boolean = false,
           )(implicit p: Parameters): Unit = {
    Regfile(
      name, numEntries, raddr, rdata, wen, waddr, wdata,
      hasZero = false, withReset, bankNum, debugReadAddr, debugReadData, debugReadAddr, debugReadData, isVlRegfile)
  }
}

object VfRegFile {
  // non-return version
  def apply(
    name         : String,
    numEntries   : Int,
    splitNum     : Int,
    raddr        : Seq[UInt],
    rdata        : Vec[UInt],
    wen          : Seq[Seq[Bool]],
    waddr        : Seq[UInt],
    wdata        : Seq[UInt],
    vecdebugReadAddr: Option[Seq[UInt]],
    vecdebugReadData: Option[Vec[UInt]],
    fpdebugReadAddr: Option[Seq[UInt]],
    fpdebugReadData: Option[Vec[UInt]],
    withReset    : Boolean = false,
  )(implicit p: Parameters): Unit = {
    require(splitNum >= 1, "splitNum should be no less than 1")
    require(splitNum == wen.length, "splitNum should be equal to length of wen vec")
    if (splitNum == 1) {
      Regfile(
        name, numEntries, raddr, rdata, wen.head, waddr, wdata,
        hasZero = false, withReset, bankNum = 1, vecdebugReadAddr, vecdebugReadData, fpdebugReadAddr, fpdebugReadData)
    } else {
      val dataWidth = 64
      val numReadPorts = raddr.length
      require(splitNum > 1 && wdata.head.getWidth == dataWidth * splitNum)
      val wdataVec = Wire(Vec(splitNum, Vec(wdata.length, UInt(dataWidth.W))))
      val rdataVec = Wire(Vec(splitNum, Vec(raddr.length, UInt(dataWidth.W))))
      val vecdebugRDataVec: Option[Vec[Vec[UInt]]] = vecdebugReadData.map(x => Wire(Vec(splitNum, Vec(x.length, UInt(dataWidth.W)))))
      val fpdebugRDataVec: Option[Vec[Vec[UInt]]] = fpdebugReadData.map(x => Wire(Vec(splitNum, Vec(x.length, UInt(dataWidth.W)))))
      for (i <- 0 until splitNum) {
        wdataVec(i) := wdata.map(_ ((i + 1) * dataWidth - 1, i * dataWidth))
        Regfile(
          name + s"Part${i}", numEntries, raddr, rdataVec(i), wen(i), waddr, wdataVec(i),
          hasZero = false, withReset, bankNum = 1, vecdebugReadAddr, vecdebugRDataVec.map(_(i)), fpdebugReadAddr, fpdebugRDataVec.map(_(i))
        )
      }
      for (i <- 0 until rdata.length) {
        rdata(i) := Cat(rdataVec.map(_ (i)).reverse)
      }
      if (vecdebugReadData.nonEmpty) {
        for (i <- 0 until vecdebugReadData.get.length) {
          vecdebugReadData.get(i) := Cat(vecdebugRDataVec.get.map(_ (i)).reverse)
        }
        for (i <- 0 until fpdebugReadData.get.length) {
          fpdebugReadData.get(i) := Cat(fpdebugRDataVec.get.map(_ (i)).reverse)
        }
      }
    }
  }
}