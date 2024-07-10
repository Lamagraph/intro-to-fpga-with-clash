module adder_reuse_without_generate (
  input logic [1:0] a,
  input logic [1:0] b,
  input logic c_in,
  output logic c_out,
  output logic [1:0] sum
);
  logic carry_tmp;
  adder_logic_1_bit add1(
    .a(a[0]),
    .b(b[0]),
    .c_in(c_in),
    .c_out(carry_tmp),
    .sum(sum[0])
  );

  adder_logic_1_bit add2(
    .a(a[1]),
    .b(b[1]),
    .c_in(carry_tmp),
    .c_out(c_out),
    .sum(sum[1])
  );

endmodule
