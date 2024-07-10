IVERILOG ?= iverilog
VVP ?= vvp
SOURCES = sum_reduce.sv testbenches/sum_reduce_tb.sv
TOPMODULE = sum_reduce_tb
IVERILOG_FLAGS = -g2012

all: tests

build:
	mkdir -p build
	$(IVERILOG) $(IVERILOG_FLAGS) $(SOURCES) -s $(TOPMODULE) -o build/$(TOPMODULE)

tests: build
	$(VVP) build/$(TOPMODULE)

clean:
	$(RM) -R build

.PHONY: tests
.PHONY: clean
.PHONY: build
