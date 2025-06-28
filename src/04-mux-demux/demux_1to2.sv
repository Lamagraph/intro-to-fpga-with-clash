module demux_1to2 (
    input  logic in,
    input  logic sel,
    output logic out0, out1
);
  assign out0 = !sel & in;
  assign out1 = sel & in;
endmodule
