from cocotb.triggers import Timer, RisingEdge, FallingEdge
from cocotb.clock import Clock
from cocotb.types import Logic
import cocotb


@cocotb.test()
async def test_reduce(dut):

    clock = Clock(dut.clk, 5, "ns")
    await cocotb.start(clock.start(start_high=False))

    dut.rst.value = 1
    await RisingEdge(dut.clk)
    dut.rst.value = 0

    await FallingEdge(dut.rst)
    expected_sum = 0
    for i in range(5):
        dut.num.value = i
        expected_sum += i

        await RisingEdge(dut.clk)
        assert dut.sum.value == expected_sum, f"wrong!  sum = {dut.sum.value}"
