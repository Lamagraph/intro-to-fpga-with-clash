module debouncer #(
    parameter int DELAY = 'd5_000_000
) (
    input  logic clk_i,
    input  logic rst_ni,
    input  logic btn_i,
    output logic btn_db_o
);

  logic [$clog2(DELAY)-1:0] counter;
  logic state;

  always_ff @(posedge clk_i or negedge rst_ni) begin
    if (!rst_ni) begin
      counter <= '0;
    end else if (btn_i) begin
      counter <= '0;
    end else if (!btn_i) begin
      counter <= counter + 1;
    end else begin
      counter <= counter;
    end
  end

  always_ff @(posedge clk_i or negedge rst_ni) begin
    if (!rst_ni) begin
      state <= '0;
    end else if (btn_i) begin
      state <= '0;
    end else if (counter == DELAY - 1) begin
      state <= '1;
    end else begin
      state <= state;
    end
  end

  assign btn_db_o = state;
endmodule
