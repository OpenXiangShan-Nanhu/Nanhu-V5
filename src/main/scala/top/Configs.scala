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

package top

import chisel3._
import chisel3.util._
import xiangshan._
import xs.utils.perf.{DebugOptions, DebugOptionsKey, LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import org.chipsalliance.cde.config._
import xiangshan.frontend.icache.ICacheParameters
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.tile.MaxHartIdBits
import xiangshan.cache.DCacheParameters


class BaseConfig(n: Int) extends Config((site, here, up) => {
  case XLen => 64
  case DebugOptionsKey => DebugOptions(ResetGen = true)
  case PMParameKey => PMParameters()
  case XSCoreParamsKey => XSCoreParameters(hasMbist = true)
  case ExportDebug => DebugAttachParams(protocols = Set(JTAG))
  case JtagDTMKey => JtagDTMKey
  case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, perfDBHartID = 0)
  case LogUtilsOptionsKey => LogUtilsOptions(false, false, true)
  case MaxHartIdBits => log2Up(n) max 8
})

class WithNKBL1I(n: Int, ways: Int = 4) extends Config((site, here, up) => {
  case XSCoreParamsKey =>
    val sets = n * 1024 / ways / 64
    up(XSCoreParamsKey).copy(
    icacheParameters = ICacheParameters(
      nSets = sets,
      nWays = ways,
      tagECC = Some("none"),
      dataECC = Some("parity"),
      replacer = Some("setplru")
    ))
})

class WithNKBL1D(n: Int, ways: Int = 8) extends Config((site, here, up) => {
  case XSCoreParamsKey =>
    val sets = n * 1024 / ways / 64
    up(XSCoreParamsKey).copy(
      dcacheParametersOpt = Some(DCacheParameters(
        nSets = sets,
        nWays = ways,
        tagECC = Some("none"),
        dataECC = Some("secded"),
        replacer = Some("setplru"),
        nMissEntries = 32,
        nProbeEntries = 4,
        nReleaseEntries = 4,
        nMaxPrefetchEntry = 6,
        enableDataEcc = true,
        enableTagEcc = false
      )))
})

class DefaultConfig(n: Int = 1) extends Config(
  new WithNKBL1D(64, ways = 4)
  ++ new WithNKBL1I(64, ways = 4)
    ++ new BaseConfig(n)
)

