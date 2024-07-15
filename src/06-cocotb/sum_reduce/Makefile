TOPLEVEL_LANG ?= verilog

PWD=$(shell pwd)


ifeq ($(TOPLEVEL_LANG),verilog)
    VERILOG_SOURCES = $(PWD)/../../04-sequential-logic/sum_reduce/sum_reduce.sv
else
    $(error A valid value (verilog) was not provided for TOPLEVEL_LANG=$(TOPLEVEL_LANG))
endif

TOPLEVEL := sum_reduce
MODULE   := reduce

include $(shell cocotb-config --makefiles)/Makefile.sim