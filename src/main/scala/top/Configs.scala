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

import coupledL2._
import coupledL2.prefetch._
import xiangshan.frontend.icache.ICacheParameters
import xs.utils.cache.{HCCacheParameters, CacheCtrl}
import xs.utils.cache.common._
import xs.utils.perf.{DebugOptionsKey, DebugOptions}
import xs.utils.cache.prefetch.{TPParameters, BOPParameters, L3PrefetchReceiverParams}
import xs.utils.cache.{L2Param, L1Param, EnableCHI}
import xiangshan.backend.regfile.V0PregParams
import xiangshan.backend.regfile.VlPregParams
import coupledL2.tl2chi.DecoupledCHI

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

// Synthesizable minimal XiangShan
// * It is still an out-of-order, super-scalaer arch
// * L1 cache included
// * L2 cache NOT included
// * L3 cache included
class MinimalConfig(n: Int = 1) extends Config(
  new BaseConfig(n).alter((site, here, up) => {
    case XSTileKey => up(XSTileKey).map(
      p => p.copy(
        DecodeWidth = 6,
        RenameWidth = 6,
        RobCommitWidth = 8,
        FetchWidth = 4,
        VirtualLoadQueueSize = 24,
        LoadQueueRAWSize = 12,
        LoadQueueReplaySize = 24,
        LoadUncacheBufferSize = 8,
        LoadQueueNWriteBanks = 4, // NOTE: make sure that LoadQueue{RAR, RAW, Replay}Size is divided by LoadQueueNWriteBanks.
        RollbackGroupSize = 8,
        StoreQueueSize = 20,
        StoreQueueNWriteBanks = 4, // NOTE: make sure that StoreQueueSize is divided by StoreQueueNWriteBanks
        StoreQueueForwardWithMask = true,
        // ============ VLSU ============
        VlMergeBufferSize = 16,
        VsMergeBufferSize = 8,
        UopWritebackWidth = 2,
        // ==============================
        RobSize = 48,
        RabSize = 96,
        FtqSize = 8,
        IBufSize = 24,
        IBufNBank = 6,
        StoreBufferSize = 4,
        StoreBufferThreshold = 3,
        dpParams = DispatchParameters(
          IntDqSize = 12,
          FpDqSize = 12,
          LsDqSize = 12,
          IntDqDeqWidth = 8,
          FpDqDeqWidth = 6,
          VecDqDeqWidth = 6,
          LsDqDeqWidth = 6
        ),
        intPreg = IntPregParams(
          numEntries = 64,
          numRead = None,
          numWrite = None,
        ),
        vfPreg = VfPregParams(
          numEntries = 160,
          numRead = None,
          numWrite = None,
        ),
        icacheParameters = ICacheParameters(
          nSets = 64, // 16KB ICache
          tagECC = Some("parity"),
          dataECC = Some("parity"),
          replacer = Some("setplru"),
        ),
        dcacheParametersOpt = Some(DCacheParameters(
          nSets = 64, // 32KB DCache
          nWays = 8,
          tagECC = Some("secded"),
          dataECC = Some("secded"),
          replacer = Some("setplru"),
          nMissEntries = 4,
          nProbeEntries = 4,
          nReleaseEntries = 8,
          nMaxPrefetchEntry = 2,
        )),
        // ============ BPU ===============
        EnableGHistDiff = false,
        FtbSize = 256,
        FtbWays = 2,
        RasSize = 8,
        RasSpecSize = 16,
        TageTableInfos =
          Seq((512, 4, 6),
            (512, 9, 6),
            (1024, 19, 6)),
        SCNRows = 128,
        SCNTables = 2,
        SCHistLens = Seq(0, 5),
        ITTageTableInfos =
          Seq((256, 4, 7),
            (256, 8, 7),
            (512, 16, 7)),
        // ================================
        itlbParameters = TLBParameters(
          name = "itlb",
          fetchi = true,
          useDmode = false,
          NWays = 4,
        ),
        dtlbParameters = TLBParameters(
          name = "ldtlb",
          NWays = 4,
          partialStaticPMP = true,
          outsideRecvFlush = true,
          outReplace = false,
          lgMaxSize = 4
        ),
        ldtlbParameters = TLBParameters(
          name = "ldtlb",
          NWays = 4,
          partialStaticPMP = true,
          outsideRecvFlush = true,
          outReplace = false,
          lgMaxSize = 4
        ),
        sttlbParameters = TLBParameters(
          name = "sttlb",
          NWays = 4,
          partialStaticPMP = true,
          outsideRecvFlush = true,
          outReplace = false,
          lgMaxSize = 4
        ),
        hytlbParameters = TLBParameters(
          name = "hytlb",
          NWays = 4,
          partialStaticPMP = true,
          outsideRecvFlush = true,
          outReplace = false,
          lgMaxSize = 4
        ),
        pftlbParameters = TLBParameters(
          name = "pftlb",
          NWays = 4,
          partialStaticPMP = true,
          outsideRecvFlush = true,
          outReplace = false,
          lgMaxSize = 4
        ),
        btlbParameters = TLBParameters(
          name = "btlb",
          NWays = 4,
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
        ),
        L2NBanks = 2,
        prefetcher = None // if L2 pf_recv_node does not exist, disable SMS prefetcher
      )
    )
    case L2ParamKey =>
      val core = site(XSTileKey).head
      up(L2ParamKey).copy(
        sets = 128,
        echoField = Seq(DirtyField()),
        prefetch = Nil,
        clientCaches = Seq(L1Param(
          "dcache",
          isKeywordBitsOpt = core.dcacheParametersOpt.get.isKeywordBitsOpt
        )),
      )
    case SoCParamsKey =>
      val tiles = site(XSTileKey)
      val l2 = site(L2ParamKey)
      up(SoCParamsKey).copy(
        L3CacheParamsOpt = Some(up(SoCParamsKey).L3CacheParamsOpt.get.copy(
          sets = 1024,
          inclusive = false,
          clientCaches = tiles.map{ core =>
            val clientDirBytes = tiles.map{ t =>
              t.L2NBanks * l2.toCacheParams.capacity
            }.sum
            val l2params = l2.toCacheParams
            l2params.copy(sets = 2 * clientDirBytes / core.L2NBanks / l2params.ways / 64)
          },
          simulation = !site(DebugOptionsKey).FPGAPlatform,
          prefetch = None
        )),
        L3NBanks = 1
      )
  })
)

// Non-synthesizable MinimalConfig, for fast simulation only
class MinimalSimConfig(n: Int = 1) extends Config(
  new MinimalConfig(n).alter((site, here, up) => {
    case XSTileKey => up(XSTileKey).map(_.copy(
      dcacheParametersOpt = None,
      softPTW = true
    ))
    case SoCParamsKey => up(SoCParamsKey).copy(
      L3CacheParamsOpt = None
    )
  })
)

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
        nMissEntries = 16,
        nProbeEntries = 4,
        nReleaseEntries = 4,
        nMaxPrefetchEntry = 6,
        enableDataEcc = true,
        enableTagEcc = false
      ))
    ))
})

class WithNKBL2
(
  n: Int,
  ways: Int = 8,
  inclusive: Boolean = true,
  banks: Int = 1,
  tp: Boolean = true
) extends Config((site, here, up) => {
  case XSTileKey =>
    up(XSTileKey).map(p => p.copy(
      L2NBanks = banks
    ))
  case L2ParamKey =>
    require(inclusive, "L2 must be inclusive")
    val core = site(XSTileKey).head
    val l2sets = n * 1024 / banks / ways / 64
    L2Param(
      name = "L2",
      ways = ways,
      sets = l2sets,
      clientCaches = Seq(L1Param(
        "dcache",
        sets = 2 * core.dcacheParametersOpt.get.nSets / banks,
        ways = core.dcacheParametersOpt.get.nWays + 2,
        aliasBitsOpt = core.dcacheParametersOpt.get.aliasBitsOpt,
        vaddrBitsOpt = Some(core.GPAddrBitsSv48x4 - log2Up(core.dcacheParametersOpt.get.blockBytes)),
        isKeywordBitsOpt = core.dcacheParametersOpt.get.isKeywordBitsOpt
      )),
      reqField = Seq(utility.ReqSourceField()),
      echoField = Seq(DirtyField()),
      prefetch = Seq(BOPParameters()) ++
        (if (tp) Seq(TPParameters()) else Nil) ++
        (if (core.prefetcher.nonEmpty) Seq(PrefetchReceiverParams()) else Nil),
      enablePerf = !site(DebugOptionsKey).FPGAPlatform && site(DebugOptionsKey).EnablePerfDebug,
      enableRollingDB = site(DebugOptionsKey).EnableRollingDB,
      enableMonitor = site(DebugOptionsKey).AlwaysBasicDB,
      elaboratedTopDown = !site(DebugOptionsKey).FPGAPlatform,
    )
})

class WithNKBL3(n: Int, ways: Int = 8, inclusive: Boolean = true, banks: Int = 1) extends Config((site, here, up) => {
  case SoCParamsKey =>
    val sets = n * 1024 / banks / ways / 64
    val tiles = site(XSTileKey)
    val l2 = site(L2ParamKey)
    val clientDirBytes = tiles.map{ t =>
      t.L2NBanks * l2.toCacheParams.capacity
    }.sum
    up(SoCParamsKey).copy(
      L3NBanks = banks,
      L3CacheParamsOpt = Some(HCCacheParameters(
        name = "L3",
        level = 3,
        ways = ways,
        sets = sets,
        inclusive = inclusive,
        clientCaches = tiles.map{ core =>
          val l2params = l2.toCacheParams
          l2params.copy(sets = 2 * clientDirBytes / core.L2NBanks / l2params.ways / 64, ways = l2params.ways + 2)
        },
        enablePerf = !site(DebugOptionsKey).FPGAPlatform && site(DebugOptionsKey).EnablePerfDebug,
        ctrl = Some(CacheCtrl(
          address = 0x39000000,
          numCores = tiles.size
        )),
        reqField = Seq(utility.ReqSourceField()),
        sramClkDivBy2 = false,
        sramDepthDiv = 4,
        tagECC = Some("secded"),
        dataECC = Some("secded"),
        simulation = !site(DebugOptionsKey).FPGAPlatform,
        prefetch = Some(L3PrefetchReceiverParams()),
        tpmeta = None
      ))
    )
})

class WithL3DebugConfig extends Config(
  new WithNKBL3(256, inclusive = false) ++ new WithNKBL2(64)
)

class WithFuzzer extends Config((site, here, up) => {
  case DebugOptionsKey => up(DebugOptionsKey).copy(
    EnablePerfDebug = false,
  )
  case SoCParamsKey => up(SoCParamsKey).copy(
    L3CacheParamsOpt = Some(up(SoCParamsKey).L3CacheParamsOpt.get.copy(
      enablePerf = false,
    )),
  )
  case L2ParamKey => up(L2ParamKey).copy(
    enablePerf = false,
  )
})

class MinimalAliasDebugConfig(n: Int = 1) extends Config(
  new WithNKBL3(512, inclusive = false) ++
    new WithNKBL2(256, inclusive = true) ++
    new WithNKBL1D(128) ++
    new MinimalConfig(n)
)

class MediumConfig(n: Int = 1) extends Config(
  new WithNKBL3(4096, inclusive = false, banks = 4)
    ++ new WithNKBL2(512, inclusive = true)
    ++ new WithNKBL1D(128)
    ++ new BaseConfig(n)
)

class FuzzConfig(dummy: Int = 0) extends Config(
  new WithFuzzer
    ++ new DefaultConfig(1)
)

class DefaultConfig(n: Int = 1) extends Config(
  new WithNKBL3(16 * 1024, inclusive = false, banks = 4, ways = 16)
    ++ new WithNKBL2(2 * 512, inclusive = true, banks = 4)
    ++ new WithNKBL1D(64, ways = 8)
    ++ new BaseConfig(n)
)


/** Nanhu V5.3 Config
 *  WithNanhuV5_3Config (resize queue, l2tlb)
 *  32KB L1i + 32KB L1d
 *  128KB L2
 *  4096KB L3
 */
class NanhuV5_3Config(n: Int = 1) extends Config(
  new WithNKBL3(4 * 1024, inclusive = false, banks = 4, ways = 8)
    ++ new WithNKBL2(128, inclusive = true, banks = 2, ways = 8, tp = false)
    ++ new WithNKBL1I(32, ways = 4)
    ++ new WithNKBL1D(32, ways = 4)
    ++ new BaseConfig(n)
)

class NactConfig(n: Int = 1) extends Config(
  new WithNKBL3(4 * 1024, inclusive = false, banks = 4, ways = 8)
    ++ new WithNKBL2(512, inclusive = true, banks = 2, ways = 8, tp = false)
    ++ new WithNKBL1I(64, ways = 4)
    ++ new WithNKBL1D(64, ways = 4)
    ++ new BaseConfig(n)
)

class NactNocConfig(n: Int = 1) extends Config(
  new WithNKBL3(n).alter((site, here, up) => {
    case SoCParamsKey => up(SoCParamsKey).copy(UseXSNoCTop = true)
  })
  ++ new WithCHI
  ++ (new WithNKBL2(512, inclusive = true, banks = 2, ways = 8, tp = false)).alter((site, here, up) => {
    case DecoupledCHI => false
  })
  ++ new WithNKBL1I(64, ways = 4)
  ++ new WithNKBL1D(64, ways = 4)
  ++ new BaseConfig(n)
)

class NactMiniConfig(n: Int = 1)extends Config(
  new NactConfig(n).alter((site, here, up) => {
    case XSTileKey => up(XSTileKey).map(_.copy(
        //Frontend
        DecodeWidth = 2,      //2
        RenameWidth = 2,      //4
        IBufSize = 32,        //32
        FtqSize = 32,         //8
        //Backend
        RobSize = 72,         //96
        RabSize = 96,         //96
        intPreg = IntPregParams(
          numEntries = 48,    //128
          numRead = None,
          numWrite = None,
        ),
        vfPreg = VfPregParams(
          numEntries = 96,    //160
          numRead = None,
          numWrite = None,
        ),
        v0Preg = V0PregParams(
          numEntries = 8,     //22
          numRead = None,
          numWrite = None,
        ),
        vlPreg = VlPregParams(
          numEntries = 8,     //32
          numRead = None,
          numWrite = None,
        ),
        IntRegCacheSize = 4,
        MemRegCacheSize = 4,
        EnableMiniConfig = true,
        //Memblock
        VirtualLoadQueueSize = 48,    //56
        LoadQueueRAWSize = 32,        //24
        LoadQueueReplaySize = 32,     //32
        LoadUncacheBufferSize = 4,    //8
        StoreQueueSize = 32,          //32
        StoreBufferSize = 4,          //8
        StoreBufferThreshold = 3,     //7
        VlMergeBufferSize = 4,        //16
        VsMergeBufferSize = 4,        //16
        VSegmentBufferSize = 4,       //8
        //L1 i&d
        icacheParameters = ICacheParameters(
          nSets = 128, //32 kB ICache, 16*1024/4/64
          nWays = 2,
        ),
        dcacheParametersOpt = Some(DCacheParameters(
          nSets = 128, //32 kB DCache, 16*1024/4/64
          nWays = 2,
          nMissEntries = 4,     //16
          nProbeEntries = 2,    //4
          nReleaseEntries = 2,  //4
          nMaxPrefetchEntry = 2,//6
        ))
      ))
    case L2ParamKey =>
      up(L2ParamKey).copy(
        sets = 64, //64k L2Cache, 64*1024/2*8*64
      )
  })
)

class WithCHI extends Config((_, _, _) => {
  case EnableCHI => true
})

class KunminghuV2Config(n: Int = 1) extends Config(
  new WithCHI
    ++ new Config((site, here, up) => {
      case SoCParamsKey => up(SoCParamsKey).copy(L3CacheParamsOpt = None) // There will be no L3
    })
    ++ new WithNKBL2(2 * 512, inclusive = true, banks = 4, tp = false)
    ++ new WithNKBL1D(64, ways = 8)
    ++ new DefaultConfig(n)
)

class XSNoCTopConfig(n: Int = 1) extends Config(
  (new KunminghuV2Config(n)).alter((site, here, up) => {
    case SoCParamsKey => up(SoCParamsKey).copy(UseXSNoCTop = true)
  })
)

class FpgaDefaultConfig(n: Int = 1) extends Config(
  (new WithNKBL3(3 * 1024, inclusive = false, banks = 1, ways = 6)
    ++ new WithNKBL2(2 * 512, inclusive = true, banks = 4)
    ++ new WithNKBL1D(64, ways = 8)
    ++ new BaseConfig(n)).alter((site, here, up) => {
    case DebugOptionsKey => up(DebugOptionsKey).copy(
      AlwaysBasicDiff = false,
      AlwaysBasicDB = false
    )
    case SoCParamsKey => up(SoCParamsKey).copy(
      L3CacheParamsOpt = Some(up(SoCParamsKey).L3CacheParamsOpt.get.copy(
        sramClkDivBy2 = false,
      )),
    )
  })
)
