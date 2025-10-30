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
import utils._
import system._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams}
import xiangshan.frontend.icache.ICacheParameters
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.tile.{MaxHartIdBits}

import xiangshan.backend.dispatch.DispatchParameters
import xiangshan.backend.regfile.{IntPregParams, VfPregParams}
import xiangshan.cache.DCacheParameters
import xiangshan.cache.mmu.{L2TLBParameters, TLBParameters}
import device.{EnableJtag, XSDebugModuleParams}

import xiangshan.frontend.icache.ICacheParameters
import xs.utils.cache.{HCCacheParameters, CacheCtrl}
import xs.utils.cache.common._
import xs.utils.perf.{DebugOptionsKey, DebugOptions}
import xs.utils.cache.prefetch.{TPParameters, BOPParameters, L3PrefetchReceiverParams}
import xs.utils.cache.{L2Param, L1Param, EnableCHI}
import xiangshan.backend.regfile.V0PregParams
import xiangshan.backend.regfile.VlPregParams

class BaseConfig(n: Int) extends Config((site, here, up) => {
  case XLen => 64
  case DebugOptionsKey => DebugOptions(ResetGen = true)
  case SoCParamsKey => SoCParameters()
  case PMParameKey => PMParameters()
  case XSTileKey => Seq.tabulate(n){ i => XSCoreParameters(HartId = i, hasMbist = false) }
  case L2ParamKey => L2Param()
  case ExportDebug => DebugAttachParams(protocols = Set(JTAG))
  case DebugModuleKey => Some(XSDebugModuleParams(site(XLen)))
  case JtagDTMKey => JtagDTMKey
  case MaxHartIdBits => log2Up(n) max 6
  case EnableJtag => true.B
})

class WithNKBL1I(n: Int, ways: Int = 4) extends Config((site, here, up) => {
  case XSTileKey =>
    val sets = n * 1024 / ways / 64
    up(XSTileKey).map(_.copy(
      icacheParameters = ICacheParameters(
        nSets = sets,
        nWays = ways,
        tagECC = Some("none"),
        dataECC = Some("parity"),
        replacer = Some("setplru")
      )
    ))
})


class WithNKBL1D(n: Int, ways: Int = 8) extends Config((site, here, up) => {
  case XSTileKey =>
    val sets = n * 1024 / ways / 64
    up(XSTileKey).map(_.copy(
      dcacheParametersOpt = Some(DCacheParameters(
        nSets = sets,
        nWays = ways,
        tagECC = Some("none"),
        dataECC = Some("parity"),
        replacer = Some("setplru"),
        nMissEntries = 32,
        nProbeEntries = 4,
        nReleaseEntries = 4,
        nMaxPrefetchEntry = 6,
        enableDataEcc = true,
        enableTagEcc = false
      ))
    ))
})


class RtsConfig(n: Int = 1) extends Config(
    new WithNKBL1I(8, ways = 4)
    ++ new WithNKBL1D(8, ways = 4)
    ++ new BaseConfig(n) alter ((site, here, up) => {
    case XSTileKey => up(XSTileKey).map(_.copy(
      DecodeWidth = 2, //2
      RenameWidth = 2, //4
      IBufSize = 16, //32
      FtqSize = 16, //48
      FtbSize = 2048,

      //backend
      RobSize = 64, //96
      RabSize = 96, //96
      intPreg = IntPregParams(
        numEntries = 64, //128
        numRead = None,
        numWrite = None,
      ),
      vfPreg = VfPregParams(
        numEntries = 96, //160
        numRead = None,
        numWrite = None,
      ),
      v0Preg = V0PregParams(
        numEntries = 8, //22
        numRead = None,
        numWrite = None,
      ),
      vlPreg = VlPregParams(
        numEntries = 8, //32
        numRead = None,
        numWrite = None,
      ),
      IntRegCacheSize = 4,
      MemRegCacheSize = 4,
      EnableMiniConfig = true,
      dpParams = DispatchParameters(
        IntDqSize = 8,
        FpDqSize = 8,
        LsDqSize = 8,
        IntDqDeqWidth = 8,
        FpDqDeqWidth = 6,
        VecDqDeqWidth = 6,
        LsDqDeqWidth = 6
      ),
      //Memblock
      VirtualLoadQueueSize = 24, //56
      LoadQueueRAWSize = 12, //24
      LoadQueueReplaySize = 24, //32
      LoadUncacheBufferSize = 4, //8
      StoreQueueSize = 20, //32
      StoreBufferSize = 4, //8
      StoreQueueNWriteBanks = 4, //8
      StoreBufferThreshold = 3, //7
      VlMergeBufferSize = 4, //16
      VsMergeBufferSize = 4, //16
      VSegmentBufferSize = 4, //8

      icacheParameters = ICacheParameters(
        nSets = 32,  // 8KiB
        nWays = 4,
      ),
      dcacheParametersOpt = Some(DCacheParameters(
        nSets = 16, //32 kB DCache, 16*1024/4/64
        nWays = 8,
        nMissEntries = 4, //16
        nProbeEntries = 2, //4
        nReleaseEntries = 2, //4
        nMaxPrefetchEntry = 2, //6
      )),
      itlbParameters = TLBParameters(
        name = "itlb",
        fetchi = true,
        useDmode = false,
        NWays = 4,
      ),
      dtlbParameters = TLBParameters(
        name = "dtlb",
        NWays = 4,
        outReplace = false,
        partialStaticPMP = true,
        outsideRecvFlush = true,
        saveLevel = false,
        lgMaxSize = 4
      ),
      ldtlbParameters = TLBParameters(
        name = "ldtlb",
        NWays = 4,
        outReplace = false,
        partialStaticPMP = true,
        outsideRecvFlush = true,
        saveLevel = false,
        lgMaxSize = 4
      ),
      l2tlbParameters = L2TLBParameters(
        l3Size = 4,
        l2Size = 4,
        l1nSets = 4,
        l1nWays = 4,
        l1ReservedBits = 1,
        l0nSets = 4,
        l0nWays = 8,
        l0ReservedBits = 0,
        spSize = 4,
      )
    ))
  })
)