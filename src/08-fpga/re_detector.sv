// Rising edge detector
module re_detector (
    input  logic clk_i,
    input  logic rst_ni,
    input  logic in_i,
    output logic out_o
);

  logic prev, prev_prev;

  always_ff @(posedge clk_i or negedge rst_ni) begin
    if (!rst_ni) begin
      prev <= '0;
      prev_prev <= '0;
    end else begin
      prev <= in_i;
      prev_prev <= prev;
    end
  end

  assign out_o = prev & !prev_prev;

endmodule
