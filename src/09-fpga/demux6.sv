module demux6 (
    input  logic [2:0] number_i,
    output logic [5:0] select_o
);
  always_comb begin
    unique case (number_i)
      'd1: select_o = 6'b000001;
      'd2: select_o = 6'b000010;
      'd3: select_o = 6'b000100;
      'd4: select_o = 6'b001000;
      'd5: select_o = 6'b010000;
      'd6: select_o = 6'b100000;
      default: select_o = '0;
    endcase
  end
endmodule
