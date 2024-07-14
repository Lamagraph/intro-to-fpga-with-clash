module merge_serial_to_parallel #(
    parameter int WIDTH_1   = 3,
    parameter int WIDTH_2   = 8,
    parameter int OUT_WIDTH = WIDTH_1 + WIDTH_2
) (
    input logic clk,
    input logic aresetn,

    // Input from master 1
    input  logic s_valid_1,
    output logic s_ready_1,
    input  logic s_data_1,

    // Input from master 2
    input  logic s_valid_2,
    output logic s_ready_2,
    input  logic s_data_2,

    // Output to slave
    output logic m_valid,
    input logic m_ready,
    output logic [OUT_WIDTH-1:0] m_data
);

  logic s2p_1_valid, s2p_1_ready, s2p_2_valid, s2p_2_ready;
  logic [WIDTH_1-1:0] s2p_1_data;
  logic [WIDTH_2-1:0] s2p_2_data;

  serial_to_parallel #(
      .OUT_WIDTH(WIDTH_1)
  ) s2p_1 (
      .clk,
      .aresetn,
      .s_valid(s_valid_1),
      .s_ready(s_ready_1),
      .s_data (s_data_1),
      .m_valid(s2p_1_valid),
      .m_ready(s2p_1_ready),
      .m_data (s2p_1_data)
  );

  serial_to_parallel #(
      .OUT_WIDTH(WIDTH_2)
  ) s2p_2 (
      .clk,
      .aresetn,
      .s_valid(s_valid_2),
      .s_ready(s_ready_2),
      .s_data (s_data_2),
      .m_valid(s2p_2_valid),
      .m_ready(s2p_2_ready),
      .m_data (s2p_2_data)
  );

  merge_parallel #(
      .IN_WIDTH_1(WIDTH_1),
      .IN_WIDTH_2(WIDTH_2)
  ) merge (
      .clk,
      .aresetn,
      .s_valid_1(s2p_1_valid),
      .s_ready_1(s2p_1_ready),
      .s_data_1 (s2p_1_data),
      .s_valid_2(s2p_2_valid),
      .s_ready_2(s2p_2_ready),
      .s_data_2 (s2p_2_data),
      .m_valid,
      .m_ready,
      .m_data
  );

endmodule
