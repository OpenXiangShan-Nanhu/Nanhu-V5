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

package nanhuv5.backend.rob

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import difftest._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xs.utils._
import xs.utils.perf._
import utils._
import nanhuv5._
import nanhuv5.backend.BackendParams
import nanhuv5.backend.Bundles.{DynInst, ExceptionInfo, ExuOutput}
import nanhuv5.backend.fu.{FuConfig, FuType}
import nanhuv5.frontend.FtqPtr
import nanhuv5.mem.{LqPtr, LsqEnqIO, SqPtr}
import nanhuv5.backend.Bundles.{DynInst, ExceptionInfo, ExuOutput}
import nanhuv5.backend.ctrlblock.{DebugLSIO, DebugLsInfo, LsTopdownInfo}
import nanhuv5.backend.fu.vector.Bundles.VType
import nanhuv5.backend.rename.SnapshotGenerator

class RobEnqPtrWrapper(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    // for input redirect
    val redirect = Input(Valid(new Redirect))
    // for enqueue
    val allowEnqueue = Input(Bool())
    val hasBlockBackward = Input(Bool())
    val enq = Vec(RenameWidth, Input(Bool()))
    val out = Output(Vec(RenameWidth, new RobPtr))
  })

  val enqPtrVec = RegInit(VecInit.tabulate(RenameWidth)(_.U.asTypeOf(new RobPtr)))

  // enqueue
  val canAccept = io.allowEnqueue && !io.hasBlockBackward
  val dispatchNum = Mux(canAccept, PopCount(io.enq), 0.U)

  for ((ptr, i) <- enqPtrVec.zipWithIndex) {
    when(io.redirect.valid) {
      ptr := Mux(io.redirect.bits.flushItself(), io.redirect.bits.robIdx + i.U, io.redirect.bits.robIdx + (i + 1).U)
    }.otherwise {
      ptr := ptr + dispatchNum
    }
  }

  io.out := enqPtrVec

}