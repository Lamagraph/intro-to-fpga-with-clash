module sum_reduce_tb;
  localparam int CountOfBits = 4;

  logic clk, rst;

  initial begin
    rst <= '1;
    @(posedge clk);
    rst <= '0;
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
    @(posedge clk);
    @(posedge clk) num = 1;
    @(posedge clk) num = 2;
    @(posedge clk) num = 3;
    @(posedge clk) $finish();
  end
endmodule
