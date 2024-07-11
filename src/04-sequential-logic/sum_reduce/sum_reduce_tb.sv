module sum_reduce_tb;
  initial begin
    $dumpfile("sum_reduce_tb.vcd");
    $dumpvars(0, sum_reduce_tb);
  end

  localparam int CountOfBits = 4;

  logic clk, rst;

  initial begin
    rst <= '1;
    @(posedge clk) rst <= '0;
  end

  initial begin
    clk = '0;
    forever
    #10 begin
      clk = ~clk;
    end
  end

  logic [CountOfBits-1:0] num, sum;

  sum_reduce #(
      .COUNT_OF_BITS(CountOfBits)
  ) DUT (
      .clk(clk),
      .rst(rst),
      .num(num),
      .sum(sum)
  );

  initial begin
    $monitor("%d %d", num, sum);

    wait (!rst);
    num = 1;
    @(posedge clk) num = 2;
    @(posedge clk) num = 3;
    @(posedge clk) num = 0;
    $finish();
  end
endmodule
