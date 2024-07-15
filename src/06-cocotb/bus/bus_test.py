import cocotb
from cocotb.clock import Clock
import cocotb.decorators
from cocotb.triggers import ClockCycles, RisingEdge
from cocotb.types import LogicArray
import random


class HelperSerialParallel:
    OutWidth = 8
    counter = 0
    res = [0] * OutWidth

    def __init__(self, dut):
        self.dut = dut

    async def generate_rnd_input(self):
        self.dut.s_valid.value = random.randint(0, 1)
        self.dut.s_data.value = random.randint(0, 1)
        self.dut.m_ready.value = random.randint(0, 1)
        await RisingEdge(self.dut.clk)

    async def initialize_rst(self):
        self.dut.aresetn.value = 0
        await ClockCycles(self.dut.clk, 2)
        self.dut.aresetn.value = 1

    async def setup(self):
        self.dut.s_valid.value = 0
        self.dut.m_ready.value = random.randint(0, 1)

    async def my_serial_to_parallel(self):
        if not self.dut.aresetn.value:
            self.res = [0] * self.OutWidth
            self.counter = 0
        else:
            if self.dut.s_valid.value and self.dut.s_ready.value:
                self.res.insert(0, self.dut.s_data.value.to_unsigned())
                self.res = self.res[0 : self.OutWidth]
                if self.counter == self.OutWidth - 1:
                    self.counter = 0
                else:
                    self.counter += 1


@cocotb.test()
async def bus_test(dut):
    NOfIterations = 1000

    clock = Clock(dut.clk, 10, units="ns")
    helper = HelperSerialParallel(dut)
    cocotb.start_soon(clock.start(start_high=False))

    await RisingEdge(dut.clk)

    cocotb.start_soon(helper.setup())
    cocotb.start_soon(helper.initialize_rst())

    await RisingEdge(dut.aresetn)
    for _ in range(NOfIterations):
        cocotb.start_soon(helper.generate_rnd_input())

        await RisingEdge(dut.clk)

        cocotb.start_soon(helper.my_serial_to_parallel())

        if helper.counter == helper.OutWidth:
            assert dut.m_valid, f"Incorrect m_valid = {dut.m_valid.value}"
            assert (
                LogicArray(helper.res) == dut.m_data.value
            ), f"m_data = {dut.m_data.value}, res = {LogicArray(helper.res)}"
