`timescale 1ns / 1ps  // <1>

module serial_to_parallel_tb;

  localparam int OutWidth = 11;  // <2>

  logic clk;
  logic aresetn;

  logic s_valid;
  logic s_ready;
  logic s_data;

  logic m_valid;
  logic m_ready;
  logic [OutWidth-1:0] m_data;

  serial_to_parallel #(
      .OUT_WIDTH(OutWidth)
  ) DUT (
      .clk(clk),
      .aresetn(aresetn),
      .s_valid(s_valid),
      .s_ready(s_ready),
      .s_data(s_data),
      .m_valid(m_valid),
      .m_ready(m_ready),
      .m_data(m_data)
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

  localparam int NOfIterations = 1000;

  initial begin  // <2>
    wait (~aresetn);
    s_valid <= '0;
    s_data  <= '0;
    wait (aresetn);

    repeat (NOfIterations) begin  // <3>
      @(posedge clk);
      s_valid <= '1;
      s_data  <= $urandom();  // <4>
      do begin
        @(posedge clk);
      end while (~s_ready);  // <5>
      s_valid <= '0;  // <6>
    end
    $finish();  // <7>
  end

  initial begin  // <8>
    wait (~aresetn);
    m_ready <= $urandom();
    wait (aresetn);

    forever begin
      @(posedge clk);
      m_ready <= $urandom();
    end
  end

  logic [OutWidth-1:0] parallel_out = '0;
  int counter = 0;

  initial begin  // <9>
    wait (aresetn);
    forever begin
      @(posedge clk);
      if (s_valid & s_ready) begin
        parallel_out <= {s_data, parallel_out[OutWidth-1:1]};

        if (counter == OutWidth) begin
          counter <= 1;
        end else begin
          counter <= counter + 1;
        end
      end
    end
  end

  initial begin  //<10>
    wait (aresetn);
    forever begin
      @(posedge clk);
      if (counter == OutWidth) begin
        if (~(m_valid)) begin
          $error("%0t Incorrect m_valid. Expected: 1, actual: %d", $time(), m_valid);
        end
      end
      if (m_data !== parallel_out) begin
        $error("%0t Incorrect m_data. Expected: %d, actual: %d", $time(), parallel_out, m_data);
      end
    end
  end

  // initial begin  // <11>
  //   $monitor("%0t\t", $time(), "aresetn=%d\t", aresetn, "s_valid=%d\t", s_valid, "s_ready=%d\t",
  //            s_ready, "s_data=%d\t", s_data, "m_valid=%d\t", m_valid, "m_ready=%d\t", m_ready,
  //            "m_data=%b\t", m_data, "parallel_out=%b", parallel_out);
  // end

  initial begin  // <12>
    repeat (100000) @(posedge clk);
    $stop();
  end

`ifdef __ICARUS__  // <13>
  initial begin
    $dumpfile("serial_to_parallel_tb.vcd");
    $dumpvars(0, serial_to_parallel_tb);
  end
`endif

endmodule
