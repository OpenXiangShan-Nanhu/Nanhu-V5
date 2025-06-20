#!/bin/bash
export NOOP_HOME=$(pwd)
#rm build -rf

#emu + Spike
make emu_rtl EMU_TRACE=1 EMU_THREADS=16 -j CONFIG=NanhuV5_3Config
#make emu_rtl-run RUN_BIN=microbench.bin
#make emu_rtl-run RUN_BIN=dhrystone.bin
make emu_rtl-run RUN_BIN=linux.bin
date
date > date.log
