module sum_reduce #(
    parameter int COUNT_OF_BITS = 4
) (
    input logic clk,
    input logic rst,
    input logic [COUNT_OF_BITS-1:0] num,
    output logic [COUNT_OF_BITS-1:0] sum
);

  logic [COUNT_OF_BITS-1:0] acc;

  always_comb begin
    sum = acc + num;
  end

  always_ff @(posedge clk) begin
    if (rst) begin
      acc <= 'b0;
    end else begin
      acc <= sum;
    end
  end
endmodule
