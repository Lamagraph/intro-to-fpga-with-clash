module sum_reduce #(
    parameter int COUNT_OF_BITS = 4
) (
    input logic clk,  // <1>
    input logic rst,  // <2>
    input logic [COUNT_OF_BITS-1:0] num,
    output logic [COUNT_OF_BITS-1:0] sum
);

  logic [COUNT_OF_BITS-1:0] acc;  // <3>

  always_comb begin  // <4>
    sum = acc + num;  // <5>
  end

  always_ff @(posedge clk) begin  // <6>
    if (rst) begin  // <7>
      acc <= 'b0;  // <8>
    end else begin
      acc <= sum;  // <9>
    end
  end
endmodule
