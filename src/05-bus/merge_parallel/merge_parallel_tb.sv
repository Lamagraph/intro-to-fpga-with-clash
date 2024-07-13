`timescale 1ns / 1ps

module merge_parallel_tb;
  localparam int InWidth1 = 3;
  localparam int InWidth2 = 8;
  localparam int OutWidth = 11;

  logic clk;
  logic aresetn;

  logic s_valid_1;
  logic s_ready_1;
  logic [InWidth1-1:0] s_data_1;

  logic s_valid_2;
  logic s_ready_2;
  logic [InWidth2-1:0] s_data_2;

  logic m_valid;
  logic m_ready;
  logic [OutWidth-1:0] m_data;

  merge_parallel DUT (
      .clk,
      .aresetn,
      .s_valid_1,
      .s_ready_1,
      .s_data_1,
      .s_valid_2,
      .s_ready_2,
      .s_data_2,
      .m_valid,
      .m_ready,
      .m_data
  );

  localparam int ClkPeriod = 10;

  initial begin
    clk <= '0;
    forever begin
      #(ClkPeriod / 2) clk <= ~clk;
    end
  end

  initial begin
    aresetn <= '0;
    #(ClkPeriod);
    aresetn <= '1;
  end

  initial begin
    wait (~aresetn);
    s_valid_1 <= '0;
    s_valid_2 <= '0;
    s_data_1  <= '0;
    s_data_2  <= '0;
    m_ready   <= '1;
    wait (aresetn);

    @(posedge clk);
    s_valid_1 <= '1;
    s_data_1  <= 3'b111;
    @(posedge clk);
    s_valid_2 <= '1;
    s_data_2  <= 8'b10101010;

    // @(posedge clk);
    // s_valid_1 <= '0;
    // s_valid_2 <= '1;
    repeat (12) @(posedge clk);

    $finish();
  end

  initial begin
    $monitor("%0t\t", $time(), "s_valid_1=%d\t", s_valid_1, "s_ready_1=%d\t", s_ready_1,
             "s_data_1=%b\t", s_data_1, "s_valid_2=%d\t", s_valid_2, "s_ready_2=%d\t", s_ready_2,
             "s_data_2=%b\t", s_data_2, "m_valid=%d\t", m_valid, "m_ready=%d\t", m_ready,
             "m_data=%b\t", m_data);
  end

  initial begin  // <12>
    repeat (100000) @(posedge clk);
    $stop();
  end

`ifdef __ICARUS__  // <13>
  initial begin
    $dumpfile("merge_parallel_tb.vcd");
    $dumpvars(0, merge_parallel_tb);
  end
`endif

endmodule
