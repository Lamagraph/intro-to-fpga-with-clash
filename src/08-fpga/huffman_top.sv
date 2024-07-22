module huffman_top (  // <1>
    input logic clk_i,

    // inputs
    input logic btn_0_i,   // <2>
    input logic btn_1_i,
    input logic rst_btn_i,

    // outputs
    output logic [5:0] led_o  // <3>
);

  logic btn_0_db, btn_1_db;

  debouncer btn_0_debouncer (  // <4>
      .clk_i(clk_i),
      .rst_ni(rst_btn_i),
      .btn_i(btn_0_i),
      .btn_db_o(btn_0_db)
  );

  debouncer inv_btn_1_debouncer (
      .clk_i(clk_i),
      .rst_ni(rst_btn_i),
      .btn_i(btn_1_i),
      .btn_db_o(btn_1_db)
  );

  logic btn_0, btn_1;

  re_detector btn_0_re_detector (  // <5>
      .clk_i (clk_i),
      .rst_ni(rst_btn_i),
      .in_i  (btn_0_db),
      .out_o (btn_0)
  );

  re_detector btn_1_re_detector (
      .clk_i (clk_i),
      .rst_ni(rst_btn_i),
      .in_i  (btn_1_db),
      .out_o (btn_1)
  );

  logic [2:0] number;

  huffman_led led_rsm (  // <6>
      .btn_0_i(btn_0),
      .btn_1_i(btn_1),
      .number_o(number),
      .clk_i(clk_i),
      .rst_ni(rst_btn_i)
  );

  logic [5:0] inv_select;

  demux6 demux_led (  // <7>
      .number_i(number),
      .select_o(inv_select)
  );

  assign led_o = ~inv_select;

endmodule
