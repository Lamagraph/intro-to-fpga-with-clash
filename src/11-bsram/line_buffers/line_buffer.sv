`timescale 1ns / 1ps // <1>
module line_buffer #(
  parameter int IMAGE_WIDTH = 640,
  parameter int PIXEL_WIDTH = 8
)(
  input  logic clk,
  input  logic rst,
  input  logic [PIXEL_WIDTH - 1:0] pixel_in,
  output logic [PIXEL_WIDTH - 1:0] pixel_out,
  output logic ready_to_write // <2>
);

logic wre;
logic[1:0] state;
logic[9:0] address;
logic ce = 1;
logic oce = 1;


parameter int FILLING = 0; // <3>
parameter int WRITE = 1;
parameter int READ = 2;

logic [PIXEL_WIDTH-1:0] din;


Gowin_SP sp( // <4>
  .dout(pixel_out),
  .clk(clk),
  .oce(oce),
  .ce(ce),
  .reset(rst),
  .wre(wre),
  .ad(address),
  .din(din)
);



always_ff @(posedge clk) begin
  if (rst) begin
    address <= 0;
    state <= FILLING;
    wre <= 1;
    din <= pixel_in;
    ready_to_write <= 1;
  end
  unique case(state)
    FILLING: // <5>
    begin
      din <= pixel_in;
      if (address == IMAGE_WIDTH - 1) begin
        state <= READ;
        address <= 10'd0;
        wre <= 0;
        ready_to_write <= 0;
      end
      else address <= address + 10'd1;
    end
    READ: // <6>
    begin
      state <= WRITE;
      wre <= 1;
      ready_to_write <= 1;
    end
    WRITE: // <7>
    begin
      state <= READ;
      wre <= 0;
      ready_to_write <= 0;
      din <= pixel_in;
      if (address == IMAGE_WIDTH - 1) begin
        address <= 10'd0;
      end
      else address <= address + 10'd1;
    end
  endcase
end

endmodule
