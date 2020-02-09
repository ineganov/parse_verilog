// Copyright 1986-2017 Xilinx, Inc. All Rights Reserved.
// --------------------------------------------------------------------------------
// Tool Version: Vivado v.2017.2 (lin64) Build 1909853 Thu Jun 15 18:39:10 MDT 2017
// Date        : Sun Apr 21 23:00:27 2019
// Host        : debian running 64-bit Debian GNU/Linux 8.2 (jessie)
// Command     : write_verilog -force syn.v
// Design      : toplevel
// Purpose     : This is a Verilog netlist of the current design or from a specific cell of the design. The output is an
//               IEEE 1364-2001 compliant Verilog HDL file that contains netlist information obtained from the input
//               design files.
// Device      : xc7a15tcpg236-1
// --------------------------------------------------------------------------------
//`timescale 1 ps / 1 ps

//(* STRUCTURAL_NETLIST = "yes" *)
module toplevel
   (xa,
    xb,
    ya,
    yb);
  input [1:0]xa;
  input [1:0]xb;
  output ya;
  output yb;

  wire [1:0]xa;
  wire [1:0]xa_IBUF;
  wire [1:0]xb;
  wire [1:0]xb_IBUF;
  wire ya;
  wire ya_OBUF;
  wire yb;
  wire yb_OBUF;

  IBUF \xa_IBUF[0]_inst 
       (.I(xa[0]),
        .O(xa_IBUF[0]));
  IBUF \xa_IBUF[1]_inst 
       (.I(xa[1]),
        .O(xa_IBUF[1]));
  IBUF \xb_IBUF[0]_inst 
       (.I(xb[0]),
        .O(xb_IBUF[0]));
  IBUF \xb_IBUF[1]_inst 
       (.I(xb[1]),
        .O(xb_IBUF[1]));
  OBUF ya_OBUF_inst
       (.I(ya_OBUF),
        .O(ya));
  LUT2 #(
    .INIT(4'h8)) 
    ya_OBUF_inst_i_1
       (.I0(xa_IBUF[0]),
        .I1(xa_IBUF[1]),
        .O(ya_OBUF));
  OBUF yb_OBUF_inst
       (.I(yb_OBUF),
        .O(yb));
  LUT2 #(
    .INIT(4'h7)) 
    yb_OBUF_inst_i_1
       (.I0(xb_IBUF[1]),
        .I1(xb_IBUF[0]),
        .O(yb_OBUF));
endmodule
