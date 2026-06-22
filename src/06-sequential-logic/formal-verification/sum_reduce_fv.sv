module sum_reduce_fv #(
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

`ifdef FORMAL
  logic past_valid = 1'b0;  // <1>
  always @(posedge clk) begin
    past_valid <= 1'b1;
  end

  initial begin  // <2>
    assume (rst);
  end

  always @(posedge clk) begin  // <3>
    if (past_valid && $past(rst)) begin
      assert (acc == '0 && sum == num);
    end
  end

  always @(posedge clk) begin  // <4>
    if (past_valid && !rst && !$past(rst)) begin
      cover (sum == $past(sum) + num && num > 0);
    end
  end

  localparam logic [COUNT_OF_BITS-1:0] MAX_VAL = '1;
  always @(posedge clk) begin  // <5>
    if (past_valid && !rst && !$past(rst)) begin
      if ($past(acc) == MAX_VAL && $past(num) > 0) begin
        assert (acc == $past(num) - 1'b1);
      end
    end
  end
`endif
endmodule
