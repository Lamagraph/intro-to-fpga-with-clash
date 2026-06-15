IVERILOG ?= iverilog
VVP ?= vvp
SOURCES = adder_logic_1_bit.sv testbenches/adder_1_bit_tb.sv
TOPMODULE = adder_1_bit_tb
IVERILOG_FLAGS = -g2012

all: tests

build:
	mkdir -p build
	$(IVERILOG) $(IVERILOG_FLAGS) -s $(TOPMODULE) -o build/$(TOPMODULE) $(SOURCES)

tests: build
	$(VVP) build/$(TOPMODULE)

clean:
	$(RM) -R build

.PHONY: tests
.PHONY: clean
.PHONY: build
