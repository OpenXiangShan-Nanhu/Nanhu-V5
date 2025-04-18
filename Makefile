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

.PHONY: idea comp init verilog