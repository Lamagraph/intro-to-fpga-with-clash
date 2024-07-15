TOPLEVEL_LANG ?= verilog

PWD=$(shell pwd)


ifeq ($(TOPLEVEL_LANG),verilog)
    VERILOG_SOURCES = $(PWD)/../../02-combination-logic/adder_logic_1_bit.sv
else
    $(error A valid value (verilog) was not provided for TOPLEVEL_LANG=$(TOPLEVEL_LANG))
endif

TOPLEVEL := adder_logic_1_bit
MODULE   := adder_1_bit_tb

include $(shell cocotb-config --makefiles)/Makefile.sim
