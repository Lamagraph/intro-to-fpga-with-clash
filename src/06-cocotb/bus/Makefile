TOPLEVEL_LANG ?= verilog

PWD=$(shell pwd)


ifeq ($(TOPLEVEL_LANG),verilog)
    VERILOG_SOURCES = $(PWD)/../../05-bus/serial_to_parallel/serial_to_parallel.sv
else
    $(error A valid value (verilog) was not provided for TOPLEVEL_LANG=$(TOPLEVEL_LANG))
endif

TOPLEVEL := serial_to_parallel
MODULE   := bus_test

include $(shell cocotb-config --makefiles)/Makefile.sim