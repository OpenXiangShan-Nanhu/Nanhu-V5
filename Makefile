help:
	mill -i xiangshan.runMain top.CoreMain --help

verilog:
	mkdir -p build
	mill -i xiangshan.runMain top.CoreMain --split-verilog --target systemverilog --full-stacktrace -td build/rtl
init:
	git submodule update --init
	cd rocket-chip/dependencies && git submodule update --init hardfloat cde diplomacy

idea:
	mill -i mill.idea.GenIdea/idea

comp:
	mill -i xiangshan.compile

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
	TRACE=1 CONSIDER_FSDB=1 REF_SO=$(NEMU_HOME)/build/riscv64-nemu-interpreter-so

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
