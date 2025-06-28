module mux_2to1_logic (
    input logic in0, in1,
    input logic sel,
    output logic out
);

  assign out = (in1 & sel) | (in0 & !sel);


endmodule

// module simple_if_tb;

// logic clk, b, a;
// simple_if dut(
//   .clk(clk),
//   .a(a),
//   .b(b));
//   initial begin
//     clk <= '0;
//     forever begin
//       #5;
//       clk <= ~clk;
//     end
//   end
//   initial begin
//     $monitor("clk=%d, a=%d, b=%d", clk, a, b);
//     @(posedge clk) a = 0;
//     @(posedge clk) a = 0;
//     @(posedge clk) a = 1;
//     @(posedge clk) begin
//       a = 1;
//       $finish();
//     end
//   end
// endmodule
