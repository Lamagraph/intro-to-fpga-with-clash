module adder_multibits_bitvectors #(
    parameter int COUNT_OF_BITS = 4
) (
    input  logic [COUNT_OF_BITS-1:0] a,
    input  logic [COUNT_OF_BITS-1:0] b,
    input logic c_in,
    output logic c_out,
    output logic [COUNT_OF_BITS-1:0] sum
);
  assign {c_out, sum} = a + b + c_in;
endmodule
