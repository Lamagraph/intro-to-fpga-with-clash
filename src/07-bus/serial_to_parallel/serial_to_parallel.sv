module serial_to_parallel #(
    parameter int OUT_WIDTH = 8  // <1>
) (
    input logic clk,
    input logic aresetn, // <2>

    // Interface inspired by AXI Stream
    // Input from master
    input  logic s_valid,
    output logic s_ready,
    input  logic s_data,

    // Output to slave
    output logic m_valid,
    input logic m_ready,
    output logic [OUT_WIDTH-1:0] m_data
);

  logic [OUT_WIDTH-1:0] data_ff;
  logic [$clog2(OUT_WIDTH)-1:0] counter;  // <3>
  logic valid_ff;

  always_ff @(posedge clk or negedge aresetn) begin  // <4>
    if (~aresetn) begin
      data_ff <= '0;
    end else if (s_ready & s_valid) begin  // <5>
      data_ff <= {s_data, data_ff[OUT_WIDTH-1:1]};  // <6>
    end
  end

  always_ff @(posedge clk or negedge aresetn) begin  // <7>
    if (~aresetn) begin
      counter  <= '0;
      valid_ff <= 1'b0;
    end else if (s_ready & s_valid) begin
      counter <= counter + 1;

      if (counter == (OUT_WIDTH - 1)) begin
        valid_ff <= 1'b1;
        counter  <= '0;
      end else begin
        valid_ff <= 1'b0;
      end

    end
  end

  always_comb begin  // <8>
    m_valid = valid_ff;
    s_ready = ~(m_valid & ~m_ready);  // <9>
    m_data  = data_ff;
  end

endmodule
