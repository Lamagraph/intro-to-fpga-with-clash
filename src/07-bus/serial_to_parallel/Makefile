IVERILOG ?= iverilog
VVP ?= vvp
SOURCES = serial_to_parallel.sv serial_to_parallel_tb.sv
TOPMODULE = serial_to_parallel_tb
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
