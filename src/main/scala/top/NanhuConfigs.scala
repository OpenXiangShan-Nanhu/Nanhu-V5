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


import xiangshan._
import org.chipsalliance.cde.config._
import xiangshan.backend.regfile.{IntPregParams, FpPregParams, VfPregParams}
import xiangshan.cache.DCacheParameters
import xiangshan.cache.mmu.{L2TLBParameters, TLBParameters}
import device.{EnableJtag, XSDebugModuleParams}
import huancun._
import coupledL2._
import coupledL2.prefetch._
import xiangshan.frontend.icache.ICacheParameters

class WithNanhuV5Config extends Config((site, here, up) =>{
  case XSTileKey => up(XSTileKey).map(_.copy(

    ITTageTableInfos = Seq(         // Default: 5table
      ( 256,    8,    9),
      ( 512,   32,    9)
    ),

    IBufSize = 32,                  // Default: 48
    IBufNBank = 4,                  // Default: 6
    DecodeWidth = 4,                // Default: 6
    RenameWidth = 4,                // Default: 6
    CommitWidth = 4,                // Default: 8
    RobCommitWidth = 4,             // Default: 8
    RabCommitWidth = 4,             // Default: 6
    RabSize = 96,                   // Default: 256
    IssueQueueSize = 12,            // Default: 24
    IssueQueueCompEntrySize = 8,    // Default: 16
    NRPhyRegs = 128,                // Default: 192
    intPreg = IntPregParams(        // Default:
      numEntries = 128,             // Default: 224
      numRead = None,               // Default:
      numWrite = None,              // Default:
    ),                              // Default:
    fpPreg = FpPregParams(          // Default:
      numEntries = 128,             // Default: 192
      numRead = None,               // Default:
      numWrite = None,              // Default:
    ),                              // Default:
    VirtualLoadQueueSize = 32,      // Default: 72
    LoadQueueRARSize = 32,          // Default: 72
    LoadQueueRAWSize = 32,          // Default: 64
    LoadQueueReplaySize = 48,       // Default: 72
    LoadUncacheBufferSize = 16,     // Default: 20
    StoreQueueSize = 48,            // Default: 64
    LoadPipelineWidth = 2,          // Default: 3
  ))
})


class WithNanhuV5_2Config extends Config((site, here, up) =>{
  case XSTileKey => up(XSTileKey).map(_.copy(

    ITTageTableInfos = Seq(         // Default: 5table
      ( 256,    8,    9),
      ( 512,   32,    9)
    ),

    IBufSize = 32,                  // Default: 48
    IBufNBank = 4,                  // Default: 6
    DecodeWidth = 4,                // Default: 6
    RenameWidth = 4,                // Default: 6
    CommitWidth = 8,                // Default: 8
    RobCommitWidth = 8,             // Default: 8
    RabCommitWidth = 4,             // Default: 6
    RobSize = 96,                   // Default: 160
    RabSize = 120,                  // Default: 256
    IssueQueueSize = 16,            // Default: 24
    IssueQueueCompEntrySize = 12,   // Default: 16
    NRPhyRegs = 128,                // Default: 192
    intPreg = IntPregParams(        // Default:
      numEntries = 128,             // Default: 224
      numRead = None,               // Default:
      numWrite = None,              // Default:
    ),                              // Default:
    fpPreg = FpPregParams(          // Default:
      numEntries = 128,             // Default: 192
      numRead = None,               // Default:
      numWrite = None,              // Default:
    ),                              // Default:
    VirtualLoadQueueSize = 48,      // Default: 72
    LoadQueueRARSize = 48,          // Default: 72
    LoadQueueRAWSize = 24,          // Default: 64
    LoadQueueReplaySize = 48,       // Default: 72
    LoadUncacheBufferSize = 8,      // Default: 20
    StoreQueueSize = 40,            // Default: 64
    LoadPipelineWidth = 2,          // Default: 3
  ))
})

class WithNanhuV5_3Config extends Config((site, here, up) =>{
  case XSTileKey => up(XSTileKey).map(_.copy(

    ITTageTableInfos = Seq(         // Default: 5table
      ( 256,    8,    9),
      ( 512,   32,    9)
    ),

    IBufSize = 32,                  // Default: 48
    IBufNBank = 4,                  // Default: 6
    DecodeWidth = 4,                // Default: 6
    RenameWidth = 4,                // Default: 6
    CommitWidth = 8,                // Default: 8
    RobCommitWidth = 8,             // Default: 8
    RabCommitWidth = 6,             // Default: 6
    RobSize = 96,                   // Default: 160
    RabSize = 120,                  // Default: 256
    IssueQueueSize = 16,            // Default: 24
    IssueQueueCompEntrySize = 12,    // Default: 16
    NRPhyRegs = 128,                // Default: 192
    intPreg = IntPregParams(        // Default:
      numEntries = 128,             // Default: 224
      numRead = None,               // Default:
      numWrite = None,              // Default:
    ),                              // Default:
    fpPreg = FpPregParams(          // Default:
      numEntries = 128,             // Default: 192
      numRead = None,               // Default:
      numWrite = None,              // Default:
    ),                              // Default:
    VirtualLoadQueueSize = 64,      // Default: 72
    LoadQueueRARSize = 48,          // Default: 72
    LoadQueueRAWSize = 24,          // Default: 64
    LoadQueueReplaySize = 36,       // Default: 72
    LoadQueueNWriteBanks = 4,       // Default: 8
    LoadUncacheBufferSize = 8,      // Default: 20
    StoreQueueSize = 32,            // Default: 64
    StoreBufferSize = 8,            // Default: 16
    LoadPipelineWidth = 2,          // Default: 3
    StorePipelineWidth = 1,         // Default: 2
    VecStorePipelineWidth = 1,      // Default: 2

    l2tlbParameters = L2TLBParameters(
      name = "l2tlb",
      // l3
      l3Size = 16,
      l3Associative = "fa",
      l3Replacer = Some("plru"),
      l2Size = 16,
      l2Associative = "fa",
      l2Replacer = Some("plru"),
      // l1
      l1nSets = 8,
      l1nWays = 4,
      l1ReservedBits = 10,
      l1Replacer = Some("setplru"),
      // l0
      l0nSets = 64,
      l0nWays = 4,
      l0ReservedBits = 0,
      l0Replacer = Some("setplru"),
      // sp
      spSize = 16,
      spReplacer = Some("plru"),
      // filter
      ifilterSize = 8,
      dfilterSize = 32,
      // miss queue, add more entries than 'must require'
      // 0 for easier bug trigger, please set as big as u can, 8 maybe
      missqueueExtendSize = 0,
      // llptw
      llptwsize = 6,
      // way size
      blockBytes = 64,
      // prefetch
      enablePrefetch = true,
      // ecc
      ecc = Some("secded"),
      // enable ecc
      enablePTWECC = false
    ),
  ))
})

/** Nanhu V5.0 Config
 *  64KB L1i + 64KB L1d
 *  256KB L2
 *  4096KB L3
 */
class NanhuV5Config(n: Int = 1) extends Config(
  new WithNanhuV5Config
    ++ new WithNKBL3(4 * 1024, inclusive = false, banks = 4, ways = 8)
    ++ new WithNKBL2(256, inclusive = true, banks = 2, ways = 8)
    ++ new WithNKBL1D(64, ways = 8)
    ++ new BaseConfig(n)
)

/** Nanhu V5.1 Config
 *  32KB L1i + 32KB L1d
 *  128KB L2
 *  4096KB L3
 */
class NanhuV5_1Config(n: Int = 1) extends Config(
  new WithNanhuV5Config
    ++ new WithNKBL3(4 * 1024, inclusive = false, banks = 4, ways = 8)
    ++ new WithNKBL2(128, inclusive = true, banks = 2, ways = 8)
    ++ new WithNKBL1I(32, ways = 4)
    ++ new WithNKBL1D(32, ways = 4)
    ++ new BaseConfig(n)
)

/** Nanhu V5.2 Config
 *  WithNanhuV5_2Config (resize queue)
 *  32KB L1i + 32KB L1d
 *  128KB L2
 *  4096KB L3
 */
class NanhuV5_2Config(n: Int = 1) extends Config(
  new WithNanhuV5_2Config
    ++ new WithNKBL3(4 * 1024, inclusive = false, banks = 4, ways = 8)
    ++ new WithNKBL2(128, inclusive = true, banks = 2, ways = 8)
    ++ new WithNKBL1I(32, ways = 4)
    ++ new WithNKBL1D(32, ways = 4)
    ++ new BaseConfig(n)
)

/** Nanhu V5.3 Config
 *  WithNanhuV5_3Config (resize queue, l2tlb)
 *  32KB L1i + 32KB L1d
 *  128KB L2
 *  4096KB L3
 */
class NanhuV5_3Config(n: Int = 1) extends Config(
  new WithNanhuV5_3Config
    ++ new WithNKBL3(4 * 1024, inclusive = false, banks = 4, ways = 8)
    ++ new WithNKBL2(128, inclusive = true, banks = 2, ways = 8)
    ++ new WithNKBL1I(32, ways = 4)
    ++ new WithNKBL1D(32, ways = 4)
    ++ new BaseConfig(n)
)
