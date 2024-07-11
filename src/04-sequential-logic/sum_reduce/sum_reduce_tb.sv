module sum_reduce_tb;
  initial begin  // <1>
    $dumpfile("sum_reduce_tb.vcd");
    $dumpvars(0, sum_reduce_tb);
  end

  localparam int CountOfBits = 4;
  localparam int ClkPeriod = 10;

  logic clk, rst;  // <2>

  initial begin  // <3>
    rst <= '1;
    #(ClkPeriod / 2);
    rst <= '0;
  end

  initial begin  // <4>
    clk <= '0;
    forever begin  //<5>
      #(ClkPeriod / 2) clk = ~clk;
    end
  end

  logic [CountOfBits-1:0] num, sum;

  sum_reduce #(
      .COUNT_OF_BITS(CountOfBits)
  ) DUT (  // <6>
      .clk(clk),
      .rst(rst),
      .num(num),
      .sum(sum)
  );

  initial begin
    $monitor("clk=%d, rst=%d, num=%d, sum=%d", clk, rst, num, sum);  // <7>

    wait (!rst) num = 1;  // <8>
    @(posedge clk) num = 2;  // <9>
    @(posedge clk) num = 3;
    @(posedge clk) begin
      num = 0;
      $finish();
    end
  end
endmodule
