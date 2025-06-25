module mux_2to1 (
    input  logic in0, in1,
    input  logic sel,
    output logic out
);
  assign out = sel ? in1 : in0;
endmodule
