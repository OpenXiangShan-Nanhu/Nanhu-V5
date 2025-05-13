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

package xiangshan

import chisel3.util.log2Ceil
import org.chipsalliance.cde.config.{Field, Parameters}
import xiangshan.backend.fu.{MMPMAConfig, MMPMAMethod}
import xiangshan.backend.fu.{MemoryRange, PMAConfigEntry, PMAConst}
import system.SoCParamsKey

case object PMParameKey extends Field[PMParameters]

case class PMParameters
(
  NumPMP: Int = 16,
  NumPMA: Int = 16,

  PlatformGrain: Int = log2Ceil(4*1024), // 4KB, a normal page
  mmpma: MMPMAConfig = MMPMAConfig(
    address = 0x38021000,
    mask = 0xfff,
    lgMaxSize = 3,
    sameCycle = true,
    num = 2
  ),
  PmemRanges: Seq[MemoryRange] =  Seq(MemoryRange(0x80000000L, 0x80000000000L)), //p(SoCParamsKey).PmemRanges
  PMAConfigs: Seq[PMAConfigEntry] = Seq(
    PMAConfigEntry(0x0L, range = 0x1000000000000L, a = 3),
    PMAConfigEntry(0x80000000000L, c = true, atomic = true, a = 1, x = true, w = true, r = true),
    PMAConfigEntry(0x80000000L, a = 1, w = true, r = true),
    PMAConfigEntry(0x3A000000L, a = 1),
    PMAConfigEntry(0x39002000L, a = 1, w = true, r = true),
    PMAConfigEntry(0x39000000L, a = 1),
    PMAConfigEntry(0x38022000L, a = 1, w = true, r = true),
    PMAConfigEntry(0x38021000L, a = 1, x = true, w = true, r = true),
    PMAConfigEntry(0x38020000L, a = 1, w = true, r = true),
    PMAConfigEntry(0x30050000L, a = 1, w = true, r = true), // FIXME: GPU space is cacheable?
    PMAConfigEntry(0x30010000L, a = 1, w = true, r = true),
    PMAConfigEntry(0x20000000L, a = 1, x = true, w = true, r = true),
    PMAConfigEntry(0x10000000L, a = 1, w = true, r = true),
    PMAConfigEntry(0)
  )
)
//trait HasPMParameters extends PMParameters
trait HasPMParameters {
  implicit val p: Parameters

  def PMPAddrBits = 48 //p(SoCParamsKey).PAddrBits
  def PMAConfigs = p(PMParameKey).PMAConfigs
  def PMXLEN = p(XLen)
  def pmParams = p(PMParameKey)
  def NumPMP = pmParams.NumPMP
  def NumPMA = pmParams.NumPMA

  def PlatformGrain = pmParams.PlatformGrain
  def mmpma = pmParams.mmpma
}
