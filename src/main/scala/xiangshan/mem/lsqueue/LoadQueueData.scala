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

package xiangshan.mem

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.{DCacheWordIO, DCacheLineIO, MemoryOpConstants}
import xiangshan.mem._
import xiangshan.backend.rob.RobPtr
import utils._
import xs.utils._
import xs.utils.perf._

// Data module define
// These raw data modules are like SyncDataModuleTemplate, but support cam-like ops
abstract class LqRawDataModule[T <: Data] (gen: T, numEntries: Int, numRead: Int, numWrite: Int, numWBank: Int, numWDelay: Int, numCamPort: Int = 0, enableCacheLineCheck: Boolean = false)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val ren   = Input(Vec(numRead, Bool()))
    val raddr = Input(Vec(numRead, UInt(log2Up(numEntries).W)))
    val rdata = Output(Vec(numRead, gen))
    val wen   = Input(Vec(numWrite, Bool()))
    val waddr = Input(Vec(numWrite, UInt(log2Up(numEntries).W)))
    val wdata = Input(Vec(numWrite, gen))
    // violation cam: hit if addr is in the same cacheline
    val violationMdataValid = Input(Vec(numCamPort, Bool()))
    val violationMdata = Input(Vec(numCamPort, gen)) // addr
    val violationMmask = Output(Vec(numCamPort, Vec(numEntries, Bool()))) // cam result mask
    // This `store` writes the whole `cacheline`.(cbo zero).
    val violationCheckLine = OptionWrapper(enableCacheLineCheck, Input(Vec(numCamPort, Bool())))
    // refill cam: hit if addr is in the same cacheline
    val releaseMdataValid = Input(Vec(numCamPort, Bool()))
    val releaseMdata = Input(Vec(numCamPort, gen))
    val releaseMmask = Output(Vec(numCamPort, Vec(numEntries, Bool())))  // cam result mask
    // release violation cam: hit if addr is in the same cacheline
    val releaseViolationMdataValid = Input(Vec(numCamPort, Bool()))
    val releaseViolationMdata = Input(Vec(numCamPort, gen))
    val releaseViolationMmask = Output(Vec(numCamPort, Vec(numEntries, Bool()))) // cam result mask result
  })

  require(isPow2(numWBank), "write bank must be a power of two!")
  require(numWBank >= 2, "write bank must be greater than or equal to two!")
  require(numWDelay >= 1, "write delay must be greater than or equal to one!")
  require(numCamPort >= 0, "camport must be greater than or equal to zero!")
  require((numEntries % numWBank == 0), "numEntries must be divided by numWBank!")

  val numEntryPerBank = numEntries / numWBank
  val dataWidth = gen.getWidth    // namely: PAddrBits=48

  val data = Reg(Vec(numEntries, gen))
  // read ports
  for (i <- 0 until numRead) {
    io.rdata(i) := RegEnable(data(io.raddr(i)), io.ren(i))
  }

  // write ports
  val writeAddrDec = io.waddr.map(a => UIntToOH(a))
  def selectBankMask(in: UInt, bank: Int): UInt = {
    in((bank + 1) * numEntryPerBank - 1, bank * numEntryPerBank)
  }
  for (bank <- 0 until numWBank) {
    // write ports
    // s0: write to bank level buffer
    val s0_bankWriteAddrDec = writeAddrDec.map(a => selectBankMask(a, bank))
    val s0_bankWriteEn = io.wen.zip(s0_bankWriteAddrDec).map(w => w._1 && w._2.orR)
    s0_bankWriteAddrDec.zipWithIndex.map(a =>
      a._1.suggestName("s0_bankWriteAddrDec" + bank + "_" + a._2)
    )
    s0_bankWriteEn.zipWithIndex.map(a =>
      a._1.suggestName("s0_bankWriteEn" + bank + "_" + a._2)
    )
    // sx: write data to entries
    val sx_bankWriteAddrDec_resp = (0 until numWrite).map(w => DelayNWithValid(s0_bankWriteAddrDec(w), io.wen(w), numWDelay - 1))
    val sx_bankWriteAddrDec = (0 until numWrite).map(w => sx_bankWriteAddrDec_resp(w)._2)
    val sx_bankWriteEn = s0_bankWriteEn.map(w => DelayN(w, numWDelay - 1))
     val sx_writeData_resp = (0 until numWrite).map(w => DelayNWithValid(io.wdata(w), io.wen(w), numWDelay - 1))
     val sx_writeData =  (0 until numWrite).map(w => sx_writeData_resp(w)._2)

    sx_bankWriteAddrDec.zipWithIndex.map(a =>
      a._1.suggestName("sx_bankWriteAddrDec" + bank + "_" + a._2)
    )
    sx_bankWriteEn.zipWithIndex.map(a =>
      a._1.suggestName("sx_bankWriteEn" + bank + "_" + a._2)
    )
    sx_writeData.zipWithIndex.map(a =>
      a._1.suggestName("sx_writeData" + bank + "_" + a._2)
    )

    // entry write
    for (entry <- 0 until numEntryPerBank) {
      // write ports
      val sx_entryWriteEnVec = sx_bankWriteEn.zip(sx_bankWriteAddrDec).map(w => w._1 && w._2(entry))
      val sx_entryWriteEn = VecInit(sx_entryWriteEnVec).asUInt.orR
      val sx_entryWriteData = Mux1H(sx_entryWriteEnVec, sx_writeData)
      when (sx_entryWriteEn) {
        data(bank * numEntryPerBank + entry) := sx_entryWriteData
      }
      sx_entryWriteEnVec.zipWithIndex.map(a =>
        a._1.suggestName("sx_entryWriteEnVec" + bank + "_" + entry + "_" + a._2)
      )
      sx_entryWriteEn.suggestName("sx_entryWriteEn" + bank + "_" + entry)
      sx_entryWriteData.suggestName("sx_entryWriteData" + bank + "_" + entry)
    }
  }

  // DataModuleTemplate should not be used when there're any write conflicts
  for (i <- 0 until numWrite) {
    for (j <- i+1 until numWrite) {
      assert(!(io.wen(i) && io.wen(j) && io.waddr(i) === io.waddr(j)))
    }
  }
}

// Load queue physical address module
class LqPAddrModule[T <: UInt](
  gen: T,
  numEntries: Int,
  numRead: Int,
  numWrite: Int,
  numWBank: Int,
  numWDelay: Int = 1,
  numCamPort: Int = 1,
  enableCacheLineCheck: Boolean = false, // Check the entire cacheline. when enabled, set `DCacheVWordOffset` correctly.
  )(implicit p: Parameters) extends LqRawDataModule(gen, numEntries, numRead, numWrite, numWBank, numWDelay, numCamPort, enableCacheLineCheck)
  with HasDCacheParameters
{
  // content addressed match
  // 128-bits aligned

  val needCacheLineCheck = enableCacheLineCheck && DCacheLineOffset > DCacheVWordOffset   // DCacheLineOffset = 6, and DCacheVWordOffset=4
  for (i <- 0 until numCamPort) {
    for (j <- 0 until numEntries) {
       when(io.violationMdataValid(i)) {
        if (needCacheLineCheck) {
          val cacheLineHit    = io.violationMdata(i)(dataWidth - 1, DCacheLineOffset) === data(j)(dataWidth - 1, DCacheLineOffset)
          val lowAddrHit      = io.violationMdata(i)(DCacheLineOffset - 1, DCacheVWordOffset) === data(j)(DCacheLineOffset - 1, DCacheVWordOffset)
          io.violationMmask(i)(j) := cacheLineHit && (io.violationCheckLine.get(i) || lowAddrHit)
        } else {
          io.violationMmask(i)(j) := io.violationMdata(i)(PAddrBits-1, DCacheVWordOffset) === data(j)(PAddrBits-1, DCacheVWordOffset)
        }
      } .otherwise {
        io.violationMmask(i)(j) := false.B
      }
    }
  }


  // content addressed match
  // cacheline aligned
  for (i <- 0 until numCamPort) {
    for (j <- 0 until numEntries) {
      when(io.releaseViolationMdataValid(i)) {
        io.releaseViolationMmask(i)(j) := io.releaseViolationMdata(i)(PAddrBits-1, DCacheLineOffset) === data(j)(PAddrBits-1, DCacheLineOffset)
      } .otherwise {
        io.releaseViolationMmask(i)(j) := false.B
      }
    }
  }

  // content addressed match
  // cacheline aligned
  for (i <- 0 until numCamPort) {
    for (j <- 0 until numEntries) {
      when(io.releaseMdataValid(i)) {
        io.releaseMmask(i)(j) := io.releaseMdata(i)(PAddrBits-1, DCacheLineOffset) === data(j)(PAddrBits-1, DCacheLineOffset)
      } .otherwise {
        io.releaseMmask(i)(j) := false.B
      }
    }
  }
}

// Load queue data module
class LqVAddrModule[T <: UInt](
  gen: T,
  numEntries: Int,
  numRead: Int,
  numWrite: Int,
  numWBank: Int,
  numWDelay: Int = 1,
  numCamPort: Int = 1)(implicit p: Parameters) extends LqRawDataModule(gen, numEntries, numRead, numWrite, numWBank, numWDelay, numCamPort)
  with HasDCacheParameters
{
  // content addressed match
  for (i <- 0 until numCamPort) {
    for (j <- 0 until numEntries) {
      when(io.violationMdataValid(i)) {
        io.violationMmask(i)(j) := io.violationMdata(i)(VAddrBits-1, DCacheVWordOffset) === data(j)(VAddrBits-1, DCacheVWordOffset)
      } .otherwise {
        io.violationMmask(i)(j) := false.B
      }
    }
  }

  // content addressed match
  for (i <- 0 until numCamPort) {
    for (j <- 0 until numEntries) {
      when(io.releaseMdataValid(i)) {
        io.releaseMmask(i)(j) := io.releaseMdata(i)(VAddrBits-1, DCacheLineOffset) === data(j)(VAddrBits-1, DCacheLineOffset)
      } .otherwise {
        io.releaseMmask(i)(j) := false.B
      }
    }
  }
}

// Load queue mask module
class LqMaskModule[T <: UInt](
  gen: T,
  numEntries: Int,
  numRead: Int,
  numWrite: Int,
  numWBank: Int,
  numWDelay: Int = 1,
  numCamPort: Int = 1)(implicit p: Parameters) extends LqRawDataModule(gen, numEntries, numRead, numWrite, numWBank, numWDelay, numCamPort)
  with HasDCacheParameters
{
  // content addressed match
  for (i <- 0 until numCamPort) {
    for (j <- 0 until numEntries) {
      when(io.violationMdataValid(i)) {
        io.violationMmask(i)(j) := (io.violationMdata(i) & data(j)).orR
      } .otherwise {
        io.violationMmask(i)(j) := false.B
      }
    }
  }
  // content addressed match
  // cacheline aligned
  for (i <- 0 until numCamPort) {
    for (j <- 0 until numEntries) {
      when(io.releaseViolationMdataValid(i)) {
        io.releaseViolationMmask(i)(j) := (io.releaseViolationMdata(i) & data(j)).orR
      } .otherwise {
        io.releaseViolationMmask(i)(j) := false.B
      }
    }
  }

  // content addressed match
  for (i <- 0 until numCamPort) {
    for (j <- 0 until numEntries) {
      when(io.releaseMdataValid(i)) {
        io.releaseMmask(i)(j) := (io.releaseMdata(i) & data(j)).orR
      } .otherwise {
        io.releaseMmask(i)(j) := false.B
      }
    }
  }
}
