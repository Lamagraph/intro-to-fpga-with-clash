
module float_mult_byte_tb;

  localparam int FloatsCount = 6;
  localparam int BytesCount = 3;


  const shortreal Floats[FloatsCount] = '{12.5, 17.2, 200, 0.24804688, 0.33, 0.999999};
  const byte unsigned Bytes[BytesCount] = '{2, 10, 15};

  shortreal float_num;
  byte unsigned byte_num;

  shortreal mult_res;

  byte unsigned byte_out;
  byte unsigned expected;

  float_mult_byte dut (
      .float_in ($shortrealtobits(float_num)),
      .uint8_in (byte_num),
      .uint8_out(byte_out)
  );

  initial begin
    for (int i = 0; i < FloatsCount; i++) begin
      for (int j = 0; j < BytesCount; j++) begin

        float_num = Floats[i];
        byte_num  = Bytes[j];
        mult_res  = byte_num * float_num;

        if (mult_res > 255) expected = 255;
        else if (mult_res <= 0) expected = 0;
        else expected = $rtoi(mult_res);

        #10;
        assert (expected === byte_out) $display("%f * %0d = %0d", float_num, byte_num, byte_out);
        else begin
          $display("%f * %0d: actual = %0d, expected = %0d", float_num, byte_num, byte_out,
                   expected);
          $fatal;
        end
      end
    end
  end
endmodule
