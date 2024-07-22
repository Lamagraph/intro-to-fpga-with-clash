//Copyright (C)2014-2024 GOWIN Semiconductor Corporation.
//All rights reserved.
//File Title: Timing Constraints file
//Tool Version: V1.9.9
//Created Time: 2024-07-20 16:49:45
create_clock -name clk -period 10 [get_ports {clk_i}] // <1>
