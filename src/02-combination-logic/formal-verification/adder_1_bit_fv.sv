module adder_1_bit_fv (
    input  logic a,
    input  logic b,
    input  logic c_in,
    output logic c_out,
    output logic sum
);

  assign sum   = a ^ b ^ c_in;
  assign c_out = (a & b) | (c_in & (a ^ b));  // <1>

`ifdef FORMAL
  always @* begin
    assert ({c_out, sum} == (a + b + c_in));  // <2>
    cover (c_in == 1'b1 && sum == 1'b0);  // <3>
  end
`endif
endmodule
