#***************************************************************************************
# Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
# Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
# Copyright (c) 2020-2021 Peng Cheng Laboratory
#
# XiangShan is licensed under Mulan PSL v2.
# You can use this software according to the terms and conditions of the Mulan PSL v2.
# You may obtain a copy of Mulan PSL v2 at:
#          http://license.coscl.org.cn/MulanPSL2
#
# THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
# EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
# MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
#
# See the Mulan PSL v2 for more details.
#***************************************************************************************

BUILD_DIR = ./build
RTL_DIR = $(BUILD_DIR)/rtl

TOP = $(XSTOP_PREFIX)XSTop
SIM_TOP = SimTop

FPGATOP = top.TopMain
SIMTOP  = top.SimTop

RTL_SUFFIX ?= sv
TOP_V = $(RTL_DIR)/$(TOP).$(RTL_SUFFIX)
SIM_TOP_V = $(RTL_DIR)/$(SIM_TOP).$(RTL_SUFFIX)

SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')
TEST_FILE = $(shell find ./src/test/scala -name '*.scala')

MEM_GEN = ./scripts/vlsi_mem_gen
MEM_GEN_SEP = ./scripts/gen_sep_mem.sh
ABS_WORK_DIR := $(shell pwd)

CONFIG ?= NanhuV5_3Config
NUM_CORES ?= 1
ISSUE ?= E.b

SUPPORT_CHI_ISSUE = B E.b
ifeq ($(findstring $(ISSUE), $(SUPPORT_CHI_ISSUE)),)
$(error "Unsupported CHI issue: $(ISSUE)")
endif

ifneq ($(shell echo "$(MAKECMDGOALS)" | grep ' '),)
$(error At most one target can be specified)
endif

ifeq ($(MAKECMDGOALS),)
GOALS = verilog
else
GOALS = $(MAKECMDGOALS)
endif

# common chisel args
FPGA_MEM_ARGS = --firtool-opt "--repl-seq-mem --repl-seq-mem-file=$(TOP).$(RTL_SUFFIX).conf"
SIM_MEM_ARGS = --firtool-opt "--repl-seq-mem --repl-seq-mem-file=$(SIM_TOP).$(RTL_SUFFIX).conf"
MFC_ARGS = --dump-fir --target systemverilog --split-verilog
MFC_ARGS += --firtool-opt "-O=release --disable-annotation-unknown --lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
RELEASE_ARGS += $(MFC_ARGS)
DEBUG_ARGS += $(MFC_ARGS)
PLDM_ARGS += $(MFC_ARGS)

ifneq ($(XSTOP_PREFIX),)
RELEASE_ARGS += --xstop-prefix $(XSTOP_PREFIX)
DEBUG_ARGS += --xstop-prefix $(XSTOP_PREFIX)
PLDM_ARGS += --xstop-prefix $(XSTOP_PREFIX)
endif

ifeq ($(IMSIC_USE_TL),1)
RELEASE_ARGS += --imsic-use-tl
DEBUG_ARGS += --imsic-use-tl
PLDM_ARGS += --imsic-use-tl
endif

# co-simulation with DRAMsim3
ifeq ($(WITH_DRAMSIM3),1)
ifndef DRAMSIM3_HOME
$(error DRAMSIM3_HOME is not set)
endif
override SIM_ARGS += --with-dramsim3
endif

# run emu with chisel-db
ifeq ($(WITH_CHISELDB),1)
override SIM_ARGS += --with-chiseldb
endif

# run emu with chisel-db
ifeq ($(WITH_ROLLINGDB),1)
override SIM_ARGS += --with-rollingdb
endif

# enable ResetGen
ifeq ($(WITH_RESETGEN),1)
override SIM_ARGS += --reset-gen
endif

# run with disable all perf
ifeq ($(DISABLE_PERF),1)
override SIM_ARGS += --disable-perf
endif

# run with disable all db
ifeq ($(DISABLE_ALWAYSDB),1)
override SIM_ARGS += --disable-alwaysdb
endif

# dynamic switch CONSTANTIN
ifeq ($(WITH_CONSTANTIN),1)
override SIM_ARGS += --with-constantin
endif

# emu for the release version
RELEASE_ARGS += --fpga-platform --disable-all --remove-assert --reset-gen --firtool-opt --ignore-read-enable-mem
DEBUG_ARGS   += --enable-difftest
PLDM_ARGS    += --fpga-platform --enable-difftest
ifeq ($(RELEASE),1)
override SIM_ARGS += $(RELEASE_ARGS)
RELEASE_ARGS += --enable-difftest
else ifeq ($(PLDM),1)
override SIM_ARGS += $(PLDM_ARGS)
else
override SIM_ARGS += $(DEBUG_ARGS)
endif

TIMELOG = $(BUILD_DIR)/time.log
TIME_CMD = time -avp -o $(TIMELOG)

ifeq ($(PLDM),1)
SED_IFNDEF = `ifndef SYNTHESIS	// src/main/scala/device/RocketDebugWrapper.scala
SED_ENDIF  = `endif // not def SYNTHESIS
endif

.DEFAULT_GOAL = verilog

help:
	mill -i xiangshan.runMain $(FPGATOP) --help

version:
	mill -i xiangshan.runMain $(FPGATOP) --version

jar:
	mill -i xiangshan.assembly

test-jar:
	mill -i xiangshan.test.assembly

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	$(TIME_CMD) mill -i xiangshan.runMain $(FPGATOP)   \
		--target-dir $(@D) --config $(CONFIG) --issue $(ISSUE) $(FPGA_MEM_ARGS)		\
		--num-cores $(NUM_CORES) $(RELEASE_ARGS) --full-stacktrace
	$(MEM_GEN_SEP) "$(MEM_GEN)" "$@.conf" "$(@D)"
	@git log -n 1 >> .__head__
	@git diff >> .__diff__
	@sed -i 's/^/\/\// ' .__head__
	@sed -i 's/^/\/\//' .__diff__
	@cat .__head__ .__diff__ $@ > .__out__
	@mv .__out__ $@
	@rm .__head__ .__diff__

verilog: $(TOP_V)

$(SIM_TOP_V): $(SCALA_FILE) $(TEST_FILE)
	mkdir -p $(@D)
	@echo -e "\n[mill] Generating Verilog files..." > $(TIMELOG)
	@date -R | tee -a $(TIMELOG)
	$(TIME_CMD) mill -i xiangshan.test.runMain $(SIMTOP)    \
		--target-dir $(@D) --config $(CONFIG) --issue $(ISSUE) $(SIM_MEM_ARGS)		\
		--num-cores $(NUM_CORES) $(SIM_ARGS) --full-stacktrace
	$(MEM_GEN_SEP) "$(MEM_GEN)" "$@.conf" "$(@D)"
	@git log -n 1 >> .__head__
	@git diff >> .__diff__
	@sed -i 's/^/\/\// ' .__head__
	@sed -i 's/^/\/\//' .__diff__
	@cat .__head__ .__diff__ $@ > .__out__
	@mv .__out__ $@
	@rm .__head__ .__diff__
ifeq ($(PLDM),1)
	sed -i -e 's/$$fatal/$$finish/g' $(RTL_DIR)/*.$(RTL_SUFFIX)
	sed -i -e '/sed/! { \|$(SED_IFNDEF)|, \|$(SED_ENDIF)| { \|$(SED_IFNDEF)|d; \|$(SED_ENDIF)|d; } }' $(RTL_DIR)/*.$(RTL_SUFFIX)
else
ifeq ($(ENABLE_XPROP),1)
	sed -i -e "s/\$$fatal/assert(1\'b0)/g" $(RTL_DIR)/*.$(RTL_SUFFIX)
else
	sed -i -e 's/$$fatal/xs_assert_v2(`__FILE__, `__LINE__)/g' $(RTL_DIR)/*.$(RTL_SUFFIX)
endif
endif
	sed -i -e "s/\$$error(/\$$fwrite(32\'h80000002, /g" $(RTL_DIR)/*.$(RTL_SUFFIX)

sim-verilog: $(SIM_TOP_V)

clean:
	$(MAKE) -C ./difftest clean
	rm -rf $(BUILD_DIR)
	rm -rf sim

init:
	git submodule update --init
	cd rocket-chip && git submodule update --init

bump:
	git submodule foreach "git fetch origin&&git checkout master&&git reset --hard origin/master"

bsp:
	mill -i mill.bsp.BSP/install

idea:
	mill -i mill.idea.GenIdea/idea

# verilator simulation
emu: sim-verilog
	$(MAKE) -C ./difftest emu SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

emu-run: emu
	$(MAKE) -C ./difftest emu-run SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

emu_rtl: sim-verilog
	$(MAKE) -C ./difftest emu SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX) \
	WITH_DRAMSIM3=$(WITH_DRAMSIM3) EMU_TRACE=1 EMU_THREADS=16 SIMDIR=1

RANDOM = $(shell echo $$RANDOM)
RUN_BIN_DIR ?= $(ABS_WORK_DIR)/ready-to-run
EMU_RUN_OPTS = -i $(RUN_BIN_DIR)/$(RUN_BIN)
EMU_RUN_OPTS += --diff $(ABS_WORK_DIR)/ready-to-run/riscv64-nemu-interpreter-so
EMU_RUN_OPTS += --wave-path $(ABS_WORK_DIR)/sim/emu/$(RUN_BIN)/tb_top.vcd
EMU_RUN_OPTS += --enable-fork --fork-interval=15 -s 0
emu_rtl-run:
	$(shell if [ ! -e $(ABS_WORK_DIR)/sim/emu/$(RUN_BIN) ];then mkdir -p $(ABS_WORK_DIR)/sim/emu/$(RUN_BIN); fi)
	touch ./sim/emu/$(RUN_BIN)/sim.log
	$(shell if [ -e $(ABS_WORK_DIR)/sim/emu/$(RUN_BIN)/emu ];then rm -f $(ABS_WORK_DIR)/sim/emu/$(RUN_BIN)/emu; fi)
	ln -s $(ABS_WORK_DIR)/sim/emu/comp/emu $(ABS_WORK_DIR)/sim/emu/$(RUN_BIN)/emu
	cd sim/emu/$(RUN_BIN) && (./emu $(EMU_RUN_OPTS) 2> assert.log | tee sim.log)

# vcs simulation
vcs-rtl:
	$(MAKE) sim-verilog ENABLE_XPROP=1 WITH_CHISELDB=0

simv: vcs-rtl
	$(MAKE) -C ./difftest simv SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX) \
	CONSIDER_FSDB=1 ENABLE_XPROP=1 WITH_CHISELDB=0

simv-run:
	$(MAKE) -C ./difftest simv-run SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX) RUN_BIN=$(RUN_BIN) RUN_BIN_DIR=$(RUN_BIN_DIR) \
	TRACE=1 CONSIDER_FSDB=1 REF_SO=$(NOOP_HOME)/ready-to-run/riscv64-nemu-interpreter-so

verdi:
	cd sim/vcs/$(RUN_BIN) && verdi -sv -2001 +verilog2001ext+v +systemverilogext+v -dbdir simv.daidir -ssf simv.fsdb -f $(ABS_WORK_DIR)/build/rtl/filelist.f

# palladium simulation
pldm-build: sim-verilog
	$(MAKE) -C ./difftest pldm-build SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

pldm-run:
	$(MAKE) -C ./difftest pldm-run SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

pldm-debug:
	$(MAKE) -C ./difftest pldm-debug SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

include Makefile.test

include src/main/scala/device/standalone/standalone_device.mk

.PHONY: verilog sim-verilog vcs-rtl emu clean help init bump bsp $(REF_SO)
