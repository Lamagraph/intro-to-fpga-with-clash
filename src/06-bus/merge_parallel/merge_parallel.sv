module merge_parallel #(
    parameter int IN_WIDTH_1 = 3,
    parameter int IN_WIDTH_2 = 8,
    parameter int OUT_WIDTH  = IN_WIDTH_1 + IN_WIDTH_2  // <1>
) (
    input logic clk,
    input logic aresetn,

    // Input from master 1
    input logic s_valid_1,
    output logic s_ready_1,
    input logic [IN_WIDTH_1-1:0] s_data_1,

    // Input from master 2
    input logic s_valid_2,
    output logic s_ready_2,
    input logic [IN_WIDTH_2-1:0] s_data_2,

    // Output to slave
    output logic m_valid,
    input logic m_ready,
    output logic [OUT_WIDTH-1:0] m_data
);

  logic [OUT_WIDTH-1:0] data_ff;
  logic valid_1_ff, valid_2_ff;

  always_ff @(posedge clk or negedge aresetn) begin  // <2>
    if (~aresetn) begin
      data_ff <= '0;
    end else begin
      if (s_ready_1 & s_valid_1) begin
        data_ff[IN_WIDTH_1-1:0] <= s_data_1;
      end
      if (s_ready_2 & s_valid_2) begin
        data_ff[OUT_WIDTH-1:IN_WIDTH_1] <= s_data_2;
      end
    end
  end

  always_ff @(posedge clk or negedge aresetn) begin  // <3>
    if (~aresetn) begin
      valid_1_ff <= '0;
      valid_2_ff <= '0;
    end else begin
      if (s_ready_1 & s_valid_1) begin
        valid_1_ff <= '1;
      end
      if (s_ready_2 & s_valid_2) begin
        valid_2_ff <= '1;
      end

      // This condition isn't perfect,
      // as it requires excess cycle to reset flags
      if (m_valid & m_ready) begin
        valid_1_ff <= '0;
        valid_2_ff <= '0;
      end
    end
  end

  always_comb begin
    m_valid = valid_1_ff & valid_2_ff;
    s_ready_1 = ~valid_1_ff;
    s_ready_2 = ~valid_2_ff;
    m_data = data_ff;
  end

endmodule
