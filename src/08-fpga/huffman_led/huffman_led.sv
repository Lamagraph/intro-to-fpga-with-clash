module huffman_led (
  // inputs
  input  logic [0:0]   btn_0_i,
  input  logic [0:0]   btn_1_i,
  // outputs
  output logic [2:0]   number_o,
  // clock and reset
  input  logic         clk_i,
  input  logic         rst_ni
);

`ifdef USE_ENUM_STATE
  typedef enum logic [4:0] {
    Start = 5'd1,
    Zero = 5'd2,
    ZeroZero = 5'd4,
    One = 5'd8,
    OneOne = 5'd16
  } state_t;
`else
  localparam logic [4:0] Start = 5'd1;
  localparam logic [4:0] Zero = 5'd2;
  localparam logic [4:0] ZeroZero = 5'd4;
  localparam logic [4:0] One = 5'd8;
  localparam logic [4:0] OneOne = 5'd16;
  typedef logic [4:0] state_t;
`endif  // USE_ENUM_STATE

  state_t state_d, state_q;
  logic [2:0] number_o_d, number_o_q;

  assign number_o = number_o_q;

  always_ff @(posedge clk_i, negedge rst_ni) begin
`ifdef FORMAL
    // SV assertions
    default clocking
      formal_clock @(posedge clk_i);
    endclocking
    default disable iff (!rst_ni);
`endif  // FORMAL
    if (!rst_ni) begin
      state_q <= Start;
      number_o_q <= '0;
    end else begin
      state_q <= state_d;
      number_o_q <= number_o_d;
    end
  end

  always_comb begin
    // default values
    state_d = state_q;
    number_o_d = number_o_q;
    unique case (state_q)
      Start: begin
        if (btn_0_i) begin
          state_d = Zero;
        end else if (btn_1_i) begin
          state_d = One;
        end
      end
      Zero: begin
        if (btn_0_i) begin
          state_d = ZeroZero;
        end else if (btn_1_i) begin
          state_d = Start;
          number_o_d = 3'd3;
        end
      end
      ZeroZero: begin
        if (btn_0_i) begin
          state_d = Start;
          number_o_d = 3'd1;
        end else if (btn_1_i) begin
          state_d = Start;
          number_o_d = 3'd2;
        end
      end
      One: begin
        if (btn_0_i) begin
          state_d = Start;
          number_o_d = 3'd4;
        end else if (btn_1_i) begin
          state_d = OneOne;
        end
      end
      OneOne: begin
        if (btn_0_i) begin
          state_d = Start;
          number_o_d = 3'd5;
        end else if (btn_1_i) begin
          state_d = Start;
          number_o_d = 3'd6;
        end
      end
      default: begin
        state_d = Start;
      end
    endcase
  end
endmodule
