module mux_2to1_logic (
    input logic in0, in1,
    input logic sel,
    output logic out
);

  assign out = (in1 & sel) | (in0 & !sel);


endmodule
