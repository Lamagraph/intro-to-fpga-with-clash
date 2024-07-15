import cocotb
from cocotb.triggers import Timer


@cocotb.test()  # <1>
async def adder_test(dut):  # <2>
    a = 1  # <3>
    b = 0
    c_in = 0

    expected_sum, excpected_c_out = (1, 0)

    dut.a.value = a  # <4>
    dut.b.value = b
    dut.c_in.value = c_in

    await Timer(1, units="ns")  # <5>

    assert (
        dut.sum.value == expected_sum
    ), f"sum = {dut.sum.value} is not equals 1"  # <6>
    assert (
        dut.c_out.value == excpected_c_out
    ), f"c_out = {dut.c_out.value} is not equals 0"
