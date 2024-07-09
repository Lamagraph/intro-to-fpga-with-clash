module adder_multibits_reuse #(
  parameter int COUNT_OF_BITS = 4 // <1>
) (
  input logic [COUNT_OF_BITS-1:0] a, // <2>
  input logic [COUNT_OF_BITS-1:0] b,
  input logic c_in,
  output logic c_out,
  output logic [COUNT_OF_BITS-1:0] sum
);

  logic [COUNT_OF_BITS:0] carry; // <3>

  assign carry[0] = c_in; // <4>
  assign c_out = carry[COUNT_OF_BITS];

  genvar i; // <5>
  generate // <6>
    for (i = 0; i < COUNT_OF_BITS; i++)
      adder_logic_1_bit add( // <7>
        .a(a[i]),
        .b(b[i]),
        .c_in(carry[i]),
        .c_out(carry[i + 1]), // <8>
        .sum(sum[i])
      );
  endgenerate
endmodule
