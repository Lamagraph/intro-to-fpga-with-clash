module rgb_to_grayscale_stream #(
    parameter int AXI_ADDR_WIDTH = 29,
    parameter int AXI_LEN_WIDTH  = 20,
    parameter int AXI_DATA_WIDTH = 256
) (
    input clk,
    input rst_n,

    input [AXI_ADDR_WIDTH-1:0] cfg_read_addr,
    input [ AXI_LEN_WIDTH-1:0] cfg_len,

    output logic                      m_axis_read_desc_valid,
    input                             m_axis_read_desc_ready,
    output       [AXI_ADDR_WIDTH-1:0] m_axis_read_desc_addr,
    output       [ AXI_LEN_WIDTH-1:0] m_axis_read_desc_len,

    output                      s_axis_rx_tready,
    input                       s_axis_rx_tvalid,
    input  [AXI_DATA_WIDTH-1:0] s_axis_rx_tdata,
    input                       s_axis_rx_tlast,

    output logic       tx_valid,
    output logic [7:0] tx_data,
    output logic       tx_last,
    input              tx_ready,

    input        run,
    output logic done
);

  assign m_axis_read_desc_addr = cfg_read_addr;  // <1>
  assign m_axis_read_desc_len  = cfg_len;

  localparam int COUNT_PIXELS = AXI_DATA_WIDTH / 32;  // <2>
  localparam int WIDTH_PTR = $clog2(COUNT_PIXELS);
  localparam [WIDTH_PTR-1:0] INDEX_PIXEL_LAST = COUNT_PIXELS - 1;

  logic [COUNT_PIXELS*8-1:0] pixels;

  genvar i;
  generate
    for (i = 0; i < COUNT_PIXELS; i = i + 1) begin : gen_pixels
      wire [ 7:0] r = s_axis_rx_tdata[i*32+0+:8];
      wire [ 7:0] g = s_axis_rx_tdata[i*32+8+:8];
      wire [ 7:0] b = s_axis_rx_tdata[i*32+16+:8];

      wire [15:0] grayscale = (r * 8'd77) + (g * 8'd150) + (b * 8'd29);  // <3>
      assign pixels[i*8+:8] = grayscale[15:8];
    end
  endgenerate

  logic [1:0] state;  // <4>
  localparam IDLE = 2'd0;
  localparam ISSUE_CMD = 2'd1;
  localparam WAIT_DATA = 2'd2;
  localparam DONE_STATE = 2'd3;

  logic                 busy;
  logic [WIDTH_PTR-1:0] pixel_ptr;
  logic                 tx_last_reg;

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      state                  <= IDLE;
      m_axis_read_desc_valid <= 1'b0;
      done                   <= 1'b0;
      busy                   <= 1'b0;
      tx_valid               <= 1'b0;
      tx_last                <= 1'b0;
    end else begin
      if (m_axis_read_desc_valid && m_axis_read_desc_ready) begin
        m_axis_read_desc_valid <= 1'b0;
      end

      case (state)
        IDLE: begin
          done <= 1'b0;
          if (run) begin
            m_axis_read_desc_valid <= 1'b1;
            state                  <= ISSUE_CMD;
          end
        end

        ISSUE_CMD: begin
          if (!m_axis_read_desc_valid) begin
            state <= WAIT_DATA;
          end
        end

        WAIT_DATA: begin
          if (s_axis_rx_tvalid && s_axis_rx_tready) begin  // <5>
            busy        <= 1'b1;
            pixel_ptr   <= '0;
            tx_valid    <= 1'b0;
            tx_last     <= 1'b0;
            tx_last_reg <= s_axis_rx_tlast;
          end else if (tx_ready && busy) begin  // <6>
            tx_data   <= pixels[int'(pixel_ptr)*8+:8];
            tx_valid  <= 1'b1;
            pixel_ptr <= pixel_ptr + 1;

            if (pixel_ptr == INDEX_PIXEL_LAST) begin  // <7>
              busy <= 1'b0;

              if (tx_last_reg) begin
                tx_last <= 1'b1;
                done <= 1'b1;
                state <= DONE_STATE;
              end
            end
          end
        end

        DONE_STATE: begin
          tx_valid <= 1'b0;
          if (!run) begin
            done  <= 1'b0;
            state <= IDLE;
          end
        end

        default: state <= IDLE;
      endcase
    end
  end

  assign s_axis_rx_tready = (state == WAIT_DATA) && !busy && (!tx_valid || tx_ready);  // <8>

endmodule
