module toplevel
   (clk,
    reset,
    leds);
  input clk;
  input reset;
  output [15:0]leds;

  wire \<const0> ;
  wire \<const1> ;
  wire clk;
  wire clk_IBUF;
  wire clk_IBUF_BUFG;
  wire \cnt[0]_i_2_n_0 ;
  wire \cnt[0]_i_3_n_0 ;
  wire \cnt[0]_i_4_n_0 ;
  wire \cnt[0]_i_5_n_0 ;
  wire \cnt[12]_i_2_n_0 ;
  wire \cnt[12]_i_3_n_0 ;
  wire \cnt[12]_i_4_n_0 ;
  wire \cnt[12]_i_5_n_0 ;
  wire \cnt[16]_i_2_n_0 ;
  wire \cnt[16]_i_3_n_0 ;
  wire \cnt[16]_i_4_n_0 ;
  wire \cnt[16]_i_5_n_0 ;
  wire \cnt[20]_i_2_n_0 ;
  wire \cnt[20]_i_3_n_0 ;
  wire \cnt[4]_i_2_n_0 ;
  wire \cnt[4]_i_3_n_0 ;
  wire \cnt[4]_i_4_n_0 ;
  wire \cnt[4]_i_5_n_0 ;
  wire \cnt[8]_i_2_n_0 ;
  wire \cnt[8]_i_3_n_0 ;
  wire \cnt[8]_i_4_n_0 ;
  wire \cnt[8]_i_5_n_0 ;
  wire \cnt_reg[0]_i_1_n_0 ;
  wire \cnt_reg[0]_i_1_n_1 ;
  wire \cnt_reg[0]_i_1_n_2 ;
  wire \cnt_reg[0]_i_1_n_3 ;
  wire \cnt_reg[0]_i_1_n_4 ;
  wire \cnt_reg[0]_i_1_n_5 ;
  wire \cnt_reg[0]_i_1_n_6 ;
  wire \cnt_reg[0]_i_1_n_7 ;
  wire \cnt_reg[12]_i_1_n_0 ;
  wire \cnt_reg[12]_i_1_n_1 ;
  wire \cnt_reg[12]_i_1_n_2 ;
  wire \cnt_reg[12]_i_1_n_3 ;
  wire \cnt_reg[12]_i_1_n_4 ;
  wire \cnt_reg[12]_i_1_n_5 ;
  wire \cnt_reg[12]_i_1_n_6 ;
  wire \cnt_reg[12]_i_1_n_7 ;
  wire \cnt_reg[16]_i_1_n_0 ;
  wire \cnt_reg[16]_i_1_n_1 ;
  wire \cnt_reg[16]_i_1_n_2 ;
  wire \cnt_reg[16]_i_1_n_3 ;
  wire \cnt_reg[16]_i_1_n_4 ;
  wire \cnt_reg[16]_i_1_n_5 ;
  wire \cnt_reg[16]_i_1_n_6 ;
  wire \cnt_reg[16]_i_1_n_7 ;
  wire \cnt_reg[20]_i_1_n_3 ;
  wire \cnt_reg[20]_i_1_n_6 ;
  wire \cnt_reg[20]_i_1_n_7 ;
  wire \cnt_reg[4]_i_1_n_0 ;
  wire \cnt_reg[4]_i_1_n_1 ;
  wire \cnt_reg[4]_i_1_n_2 ;
  wire \cnt_reg[4]_i_1_n_3 ;
  wire \cnt_reg[4]_i_1_n_4 ;
  wire \cnt_reg[4]_i_1_n_5 ;
  wire \cnt_reg[4]_i_1_n_6 ;
  wire \cnt_reg[4]_i_1_n_7 ;
  wire \cnt_reg[8]_i_1_n_0 ;
  wire \cnt_reg[8]_i_1_n_1 ;
  wire \cnt_reg[8]_i_1_n_2 ;
  wire \cnt_reg[8]_i_1_n_3 ;
  wire \cnt_reg[8]_i_1_n_4 ;
  wire \cnt_reg[8]_i_1_n_5 ;
  wire \cnt_reg[8]_i_1_n_6 ;
  wire \cnt_reg[8]_i_1_n_7 ;
  wire \cnt_reg_n_0_[0] ;
  wire \cnt_reg_n_0_[10] ;
  wire \cnt_reg_n_0_[11] ;
  wire \cnt_reg_n_0_[12] ;
  wire \cnt_reg_n_0_[13] ;
  wire \cnt_reg_n_0_[14] ;
  wire \cnt_reg_n_0_[15] ;
  wire \cnt_reg_n_0_[16] ;
  wire \cnt_reg_n_0_[17] ;
  wire \cnt_reg_n_0_[18] ;
  wire \cnt_reg_n_0_[19] ;
  wire \cnt_reg_n_0_[1] ;
  wire \cnt_reg_n_0_[20] ;
  wire \cnt_reg_n_0_[2] ;
  wire \cnt_reg_n_0_[3] ;
  wire \cnt_reg_n_0_[4] ;
  wire \cnt_reg_n_0_[5] ;
  wire \cnt_reg_n_0_[6] ;
  wire \cnt_reg_n_0_[7] ;
  wire \cnt_reg_n_0_[8] ;
  wire \cnt_reg_n_0_[9] ;
  wire [15:0]leds;
  wire \leds[10]_i_1_n_0 ;
  wire \leds[11]_i_1_n_0 ;
  wire \leds[12]_i_1_n_0 ;
  wire \leds[13]_i_1_n_0 ;
  wire \leds[14]_i_1_n_0 ;
  wire \leds[15]_i_1_n_0 ;
  wire \leds[1]_i_1_n_0 ;
  wire \leds[2]_i_1_n_0 ;
  wire \leds[3]_i_1_n_0 ;
  wire \leds[4]_i_1_n_0 ;
  wire \leds[5]_i_1_n_0 ;
  wire \leds[6]_i_1_n_0 ;
  wire \leds[7]_i_1_n_0 ;
  wire \leds[8]_i_1_n_0 ;
  wire \leds[9]_i_1_n_0 ;
  wire [15:0]leds_OBUF;
  wire reset;
  wire reset_IBUF;
  wire upd;

  GND GND
       (.G(\<const0> ));
  VCC VCC
       (.P(\<const1> ));
  BUFG clk_IBUF_BUFG_inst
       (.I(clk_IBUF),
        .O(clk_IBUF_BUFG));
  IBUF clk_IBUF_inst
       (.I(clk),
        .O(clk_IBUF));
  LUT1 #(
    .INIT(2'h2)) 
    \cnt[0]_i_2 
       (.I0(\cnt_reg_n_0_[3] ),
        .O(\cnt[0]_i_2_n_0 ));
  LUT1 #(
    .INIT(2'h2)) 
    \cnt[0]_i_3 
       (.I0(\cnt_reg_n_0_[2] ),
        .O(\cnt[0]_i_3_n_0 ));
  LUT1 #(
    .INIT(2'h2)) 
    \cnt[0]_i_4 
       (.I0(\cnt_reg_n_0_[1] ),
        .O(\cnt[0]_i_4_n_0 ));
  LUT1 #(
    .INIT(2'h1)) 
    \cnt[0]_i_5 
       (.I0(\cnt_reg_n_0_[0] ),
        .O(\cnt[0]_i_5_n_0 ));
  LUT1 #(
    .INIT(2'h2)) 
    \cnt[12]_i_2 
       (.I0(\cnt_reg_n_0_[15] ),
        .O(\cnt[12]_i_2_n_0 ));
  LUT1 #(
    .INIT(2'h2)) 
    \cnt[12]_i_3 
       (.I0(\cnt_reg_n_0_[14] ),
        .O(\cnt[12]_i_3_n_0 ));
  LUT1 #(
    .INIT(2'h2)) 
    \cnt[12]_i_4 
       (.I0(\cnt_reg_n_0_[13] ),
        .O(\cnt[12]_i_4_n_0 ));
  LUT1 #(
    .INIT(2'h2)) 
    \cnt[12]_i_5 
       (.I0(\cnt_reg_n_0_[12] ),
        .O(\cnt[12]_i_5_n_0 ));
  LUT1 #(
    .INIT(2'h2)) 
    \cnt[16]_i_2 
       (.I0(\cnt_reg_n_0_[19] ),
        .O(\cnt[16]_i_2_n_0 ));
  LUT1 #(
    .INIT(2'h2)) 
    \cnt[16]_i_3 
       (.I0(\cnt_reg_n_0_[18] ),
        .O(\cnt[16]_i_3_n_0 ));
  LUT1 #(
    .INIT(2'h2)) 
    \cnt[16]_i_4 
       (.I0(\cnt_reg_n_0_[17] ),
        .O(\cnt[16]_i_4_n_0 ));
  LUT1 #(
    .INIT(2'h2)) 
    \cnt[16]_i_5 
       (.I0(\cnt_reg_n_0_[16] ),
        .O(\cnt[16]_i_5_n_0 ));
  LUT1 #(
    .INIT(2'h2)) 
    \cnt[20]_i_2 
       (.I0(upd),
        .O(\cnt[20]_i_2_n_0 ));
  LUT1 #(
    .INIT(2'h2)) 
    \cnt[20]_i_3 
       (.I0(\cnt_reg_n_0_[20] ),
        .O(\cnt[20]_i_3_n_0 ));
  LUT1 #(
    .INIT(2'h2)) 
    \cnt[4]_i_2 
       (.I0(\cnt_reg_n_0_[7] ),
        .O(\cnt[4]_i_2_n_0 ));
  LUT1 #(
    .INIT(2'h2)) 
    \cnt[4]_i_3 
       (.I0(\cnt_reg_n_0_[6] ),
        .O(\cnt[4]_i_3_n_0 ));
  LUT1 #(
    .INIT(2'h2)) 
    \cnt[4]_i_4 
       (.I0(\cnt_reg_n_0_[5] ),
        .O(\cnt[4]_i_4_n_0 ));
  LUT1 #(
    .INIT(2'h2)) 
    \cnt[4]_i_5 
       (.I0(\cnt_reg_n_0_[4] ),
        .O(\cnt[4]_i_5_n_0 ));
  LUT1 #(
    .INIT(2'h2)) 
    \cnt[8]_i_2 
       (.I0(\cnt_reg_n_0_[11] ),
        .O(\cnt[8]_i_2_n_0 ));
  LUT1 #(
    .INIT(2'h2)) 
    \cnt[8]_i_3 
       (.I0(\cnt_reg_n_0_[10] ),
        .O(\cnt[8]_i_3_n_0 ));
  LUT1 #(
    .INIT(2'h2)) 
    \cnt[8]_i_4 
       (.I0(\cnt_reg_n_0_[9] ),
        .O(\cnt[8]_i_4_n_0 ));
  LUT1 #(
    .INIT(2'h2)) 
    \cnt[8]_i_5 
       (.I0(\cnt_reg_n_0_[8] ),
        .O(\cnt[8]_i_5_n_0 ));
  FDRE #(
    .INIT(1'b0)) 
    \cnt_reg[0] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .D(\cnt_reg[0]_i_1_n_7 ),
        .Q(\cnt_reg_n_0_[0] ),
        .R(reset_IBUF));
  CARRY4 \cnt_reg[0]_i_1 
       (.CI(\<const0> ),
        .CO({\cnt_reg[0]_i_1_n_0 ,\cnt_reg[0]_i_1_n_1 ,\cnt_reg[0]_i_1_n_2 ,\cnt_reg[0]_i_1_n_3 }),
        .CYINIT(\<const0> ),
        .DI({\<const0> ,\<const0> ,\<const0> ,\<const1> }),
        .O({\cnt_reg[0]_i_1_n_4 ,\cnt_reg[0]_i_1_n_5 ,\cnt_reg[0]_i_1_n_6 ,\cnt_reg[0]_i_1_n_7 }),
        .S({\cnt[0]_i_2_n_0 ,\cnt[0]_i_3_n_0 ,\cnt[0]_i_4_n_0 ,\cnt[0]_i_5_n_0 }));
  FDRE #(
    .INIT(1'b0)) 
    \cnt_reg[10] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .D(\cnt_reg[8]_i_1_n_5 ),
        .Q(\cnt_reg_n_0_[10] ),
        .R(reset_IBUF));
  FDRE #(
    .INIT(1'b0)) 
    \cnt_reg[11] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .D(\cnt_reg[8]_i_1_n_4 ),
        .Q(\cnt_reg_n_0_[11] ),
        .R(reset_IBUF));
  FDRE #(
    .INIT(1'b0)) 
    \cnt_reg[12] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .D(\cnt_reg[12]_i_1_n_7 ),
        .Q(\cnt_reg_n_0_[12] ),
        .R(reset_IBUF));
  CARRY4 \cnt_reg[12]_i_1 
       (.CI(\cnt_reg[8]_i_1_n_0 ),
        .CO({\cnt_reg[12]_i_1_n_0 ,\cnt_reg[12]_i_1_n_1 ,\cnt_reg[12]_i_1_n_2 ,\cnt_reg[12]_i_1_n_3 }),
        .CYINIT(\<const0> ),
        .DI({\<const0> ,\<const0> ,\<const0> ,\<const0> }),
        .O({\cnt_reg[12]_i_1_n_4 ,\cnt_reg[12]_i_1_n_5 ,\cnt_reg[12]_i_1_n_6 ,\cnt_reg[12]_i_1_n_7 }),
        .S({\cnt[12]_i_2_n_0 ,\cnt[12]_i_3_n_0 ,\cnt[12]_i_4_n_0 ,\cnt[12]_i_5_n_0 }));
  FDRE #(
    .INIT(1'b0)) 
    \cnt_reg[13] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .D(\cnt_reg[12]_i_1_n_6 ),
        .Q(\cnt_reg_n_0_[13] ),
        .R(reset_IBUF));
  FDRE #(
    .INIT(1'b0)) 
    \cnt_reg[14] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .D(\cnt_reg[12]_i_1_n_5 ),
        .Q(\cnt_reg_n_0_[14] ),
        .R(reset_IBUF));
  FDRE #(
    .INIT(1'b0)) 
    \cnt_reg[15] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .D(\cnt_reg[12]_i_1_n_4 ),
        .Q(\cnt_reg_n_0_[15] ),
        .R(reset_IBUF));
  FDRE #(
    .INIT(1'b0)) 
    \cnt_reg[16] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .D(\cnt_reg[16]_i_1_n_7 ),
        .Q(\cnt_reg_n_0_[16] ),
        .R(reset_IBUF));
  CARRY4 \cnt_reg[16]_i_1 
       (.CI(\cnt_reg[12]_i_1_n_0 ),
        .CO({\cnt_reg[16]_i_1_n_0 ,\cnt_reg[16]_i_1_n_1 ,\cnt_reg[16]_i_1_n_2 ,\cnt_reg[16]_i_1_n_3 }),
        .CYINIT(\<const0> ),
        .DI({\<const0> ,\<const0> ,\<const0> ,\<const0> }),
        .O({\cnt_reg[16]_i_1_n_4 ,\cnt_reg[16]_i_1_n_5 ,\cnt_reg[16]_i_1_n_6 ,\cnt_reg[16]_i_1_n_7 }),
        .S({\cnt[16]_i_2_n_0 ,\cnt[16]_i_3_n_0 ,\cnt[16]_i_4_n_0 ,\cnt[16]_i_5_n_0 }));
  FDRE #(
    .INIT(1'b0)) 
    \cnt_reg[17] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .D(\cnt_reg[16]_i_1_n_6 ),
        .Q(\cnt_reg_n_0_[17] ),
        .R(reset_IBUF));
  FDRE #(
    .INIT(1'b0)) 
    \cnt_reg[18] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .D(\cnt_reg[16]_i_1_n_5 ),
        .Q(\cnt_reg_n_0_[18] ),
        .R(reset_IBUF));
  FDRE #(
    .INIT(1'b0)) 
    \cnt_reg[19] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .D(\cnt_reg[16]_i_1_n_4 ),
        .Q(\cnt_reg_n_0_[19] ),
        .R(reset_IBUF));
  FDRE #(
    .INIT(1'b0)) 
    \cnt_reg[1] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .D(\cnt_reg[0]_i_1_n_6 ),
        .Q(\cnt_reg_n_0_[1] ),
        .R(reset_IBUF));
  FDRE #(
    .INIT(1'b0)) 
    \cnt_reg[20] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .D(\cnt_reg[20]_i_1_n_7 ),
        .Q(\cnt_reg_n_0_[20] ),
        .R(reset_IBUF));
  CARRY4 \cnt_reg[20]_i_1 
       (.CI(\cnt_reg[16]_i_1_n_0 ),
        .CO(\cnt_reg[20]_i_1_n_3 ),
        .CYINIT(\<const0> ),
        .DI({\<const0> ,\<const0> ,\<const0> ,\<const0> }),
        .O({\cnt_reg[20]_i_1_n_6 ,\cnt_reg[20]_i_1_n_7 }),
        .S({\<const0> ,\<const0> ,\cnt[20]_i_2_n_0 ,\cnt[20]_i_3_n_0 }));
  FDRE #(
    .INIT(1'b0)) 
    \cnt_reg[21] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .D(\cnt_reg[20]_i_1_n_6 ),
        .Q(upd),
        .R(reset_IBUF));
  FDRE #(
    .INIT(1'b0)) 
    \cnt_reg[2] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .D(\cnt_reg[0]_i_1_n_5 ),
        .Q(\cnt_reg_n_0_[2] ),
        .R(reset_IBUF));
  FDRE #(
    .INIT(1'b0)) 
    \cnt_reg[3] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .D(\cnt_reg[0]_i_1_n_4 ),
        .Q(\cnt_reg_n_0_[3] ),
        .R(reset_IBUF));
  FDRE #(
    .INIT(1'b0)) 
    \cnt_reg[4] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .D(\cnt_reg[4]_i_1_n_7 ),
        .Q(\cnt_reg_n_0_[4] ),
        .R(reset_IBUF));
  CARRY4 \cnt_reg[4]_i_1 
       (.CI(\cnt_reg[0]_i_1_n_0 ),
        .CO({\cnt_reg[4]_i_1_n_0 ,\cnt_reg[4]_i_1_n_1 ,\cnt_reg[4]_i_1_n_2 ,\cnt_reg[4]_i_1_n_3 }),
        .CYINIT(\<const0> ),
        .DI({\<const0> ,\<const0> ,\<const0> ,\<const0> }),
        .O({\cnt_reg[4]_i_1_n_4 ,\cnt_reg[4]_i_1_n_5 ,\cnt_reg[4]_i_1_n_6 ,\cnt_reg[4]_i_1_n_7 }),
        .S({\cnt[4]_i_2_n_0 ,\cnt[4]_i_3_n_0 ,\cnt[4]_i_4_n_0 ,\cnt[4]_i_5_n_0 }));
  FDRE #(
    .INIT(1'b0)) 
    \cnt_reg[5] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .D(\cnt_reg[4]_i_1_n_6 ),
        .Q(\cnt_reg_n_0_[5] ),
        .R(reset_IBUF));
  FDRE #(
    .INIT(1'b0)) 
    \cnt_reg[6] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .D(\cnt_reg[4]_i_1_n_5 ),
        .Q(\cnt_reg_n_0_[6] ),
        .R(reset_IBUF));
  FDRE #(
    .INIT(1'b0)) 
    \cnt_reg[7] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .D(\cnt_reg[4]_i_1_n_4 ),
        .Q(\cnt_reg_n_0_[7] ),
        .R(reset_IBUF));
  FDRE #(
    .INIT(1'b0)) 
    \cnt_reg[8] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .D(\cnt_reg[8]_i_1_n_7 ),
        .Q(\cnt_reg_n_0_[8] ),
        .R(reset_IBUF));
  CARRY4 \cnt_reg[8]_i_1 
       (.CI(\cnt_reg[4]_i_1_n_0 ),
        .CO({\cnt_reg[8]_i_1_n_0 ,\cnt_reg[8]_i_1_n_1 ,\cnt_reg[8]_i_1_n_2 ,\cnt_reg[8]_i_1_n_3 }),
        .CYINIT(\<const0> ),
        .DI({\<const0> ,\<const0> ,\<const0> ,\<const0> }),
        .O({\cnt_reg[8]_i_1_n_4 ,\cnt_reg[8]_i_1_n_5 ,\cnt_reg[8]_i_1_n_6 ,\cnt_reg[8]_i_1_n_7 }),
        .S({\cnt[8]_i_2_n_0 ,\cnt[8]_i_3_n_0 ,\cnt[8]_i_4_n_0 ,\cnt[8]_i_5_n_0 }));
  FDRE #(
    .INIT(1'b0)) 
    \cnt_reg[9] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .D(\cnt_reg[8]_i_1_n_6 ),
        .Q(\cnt_reg_n_0_[9] ),
        .R(reset_IBUF));
   
  LUT2 #(
    .INIT(4'h2)) 
    \leds[10]_i_1 
       (.I0(leds_OBUF[9]),
        .I1(leds_OBUF[15]),
        .O(\leds[10]_i_1_n_0 ));
   
  LUT2 #(
    .INIT(4'h2)) 
    \leds[11]_i_1 
       (.I0(leds_OBUF[10]),
        .I1(leds_OBUF[15]),
        .O(\leds[11]_i_1_n_0 ));
   
  LUT2 #(
    .INIT(4'h2)) 
    \leds[12]_i_1 
       (.I0(leds_OBUF[11]),
        .I1(leds_OBUF[15]),
        .O(\leds[12]_i_1_n_0 ));
   
  LUT2 #(
    .INIT(4'h2)) 
    \leds[13]_i_1 
       (.I0(leds_OBUF[12]),
        .I1(leds_OBUF[15]),
        .O(\leds[13]_i_1_n_0 ));
   
  LUT2 #(
    .INIT(4'h2)) 
    \leds[14]_i_1 
       (.I0(leds_OBUF[13]),
        .I1(leds_OBUF[15]),
        .O(\leds[14]_i_1_n_0 ));
   
  LUT2 #(
    .INIT(4'h2)) 
    \leds[15]_i_1 
       (.I0(leds_OBUF[14]),
        .I1(leds_OBUF[15]),
        .O(\leds[15]_i_1_n_0 ));
  LUT2 #(
    .INIT(4'h2)) 
    \leds[1]_i_1 
       (.I0(leds_OBUF[0]),
        .I1(leds_OBUF[15]),
        .O(\leds[1]_i_1_n_0 ));
   
  LUT2 #(
    .INIT(4'h2)) 
    \leds[2]_i_1 
       (.I0(leds_OBUF[1]),
        .I1(leds_OBUF[15]),
        .O(\leds[2]_i_1_n_0 ));
   
  LUT2 #(
    .INIT(4'h2)) 
    \leds[3]_i_1 
       (.I0(leds_OBUF[2]),
        .I1(leds_OBUF[15]),
        .O(\leds[3]_i_1_n_0 ));
   
  LUT2 #(
    .INIT(4'h2)) 
    \leds[4]_i_1 
       (.I0(leds_OBUF[3]),
        .I1(leds_OBUF[15]),
        .O(\leds[4]_i_1_n_0 ));
   
  LUT2 #(
    .INIT(4'h2)) 
    \leds[5]_i_1 
       (.I0(leds_OBUF[4]),
        .I1(leds_OBUF[15]),
        .O(\leds[5]_i_1_n_0 ));
   
  LUT2 #(
    .INIT(4'h2)) 
    \leds[6]_i_1 
       (.I0(leds_OBUF[5]),
        .I1(leds_OBUF[15]),
        .O(\leds[6]_i_1_n_0 ));
   
  LUT2 #(
    .INIT(4'h2)) 
    \leds[7]_i_1 
       (.I0(leds_OBUF[6]),
        .I1(leds_OBUF[15]),
        .O(\leds[7]_i_1_n_0 ));
   
  LUT2 #(
    .INIT(4'h2)) 
    \leds[8]_i_1 
       (.I0(leds_OBUF[7]),
        .I1(leds_OBUF[15]),
        .O(\leds[8]_i_1_n_0 ));
   
  LUT2 #(
    .INIT(4'h2)) 
    \leds[9]_i_1 
       (.I0(leds_OBUF[8]),
        .I1(leds_OBUF[15]),
        .O(\leds[9]_i_1_n_0 ));
  OBUF \leds_OBUF[0]_inst 
       (.I(leds_OBUF[0]),
        .O(leds[0]));
  OBUF \leds_OBUF[10]_inst 
       (.I(leds_OBUF[10]),
        .O(leds[10]));
  OBUF \leds_OBUF[11]_inst 
       (.I(leds_OBUF[11]),
        .O(leds[11]));
  OBUF \leds_OBUF[12]_inst 
       (.I(leds_OBUF[12]),
        .O(leds[12]));
  OBUF \leds_OBUF[13]_inst 
       (.I(leds_OBUF[13]),
        .O(leds[13]));
  OBUF \leds_OBUF[14]_inst 
       (.I(leds_OBUF[14]),
        .O(leds[14]));
  OBUF \leds_OBUF[15]_inst 
       (.I(leds_OBUF[15]),
        .O(leds[15]));
  OBUF \leds_OBUF[1]_inst 
       (.I(leds_OBUF[1]),
        .O(leds[1]));
  OBUF \leds_OBUF[2]_inst 
       (.I(leds_OBUF[2]),
        .O(leds[2]));
  OBUF \leds_OBUF[3]_inst 
       (.I(leds_OBUF[3]),
        .O(leds[3]));
  OBUF \leds_OBUF[4]_inst 
       (.I(leds_OBUF[4]),
        .O(leds[4]));
  OBUF \leds_OBUF[5]_inst 
       (.I(leds_OBUF[5]),
        .O(leds[5]));
  OBUF \leds_OBUF[6]_inst 
       (.I(leds_OBUF[6]),
        .O(leds[6]));
  OBUF \leds_OBUF[7]_inst 
       (.I(leds_OBUF[7]),
        .O(leds[7]));
  OBUF \leds_OBUF[8]_inst 
       (.I(leds_OBUF[8]),
        .O(leds[8]));
  OBUF \leds_OBUF[9]_inst 
       (.I(leds_OBUF[9]),
        .O(leds[9]));
  FDSE \leds_reg[0] 
       (.C(clk_IBUF_BUFG),
        .CE(upd),
        .D(leds_OBUF[15]),
        .Q(leds_OBUF[0]),
        .S(reset_IBUF));
  FDRE \leds_reg[10] 
       (.C(clk_IBUF_BUFG),
        .CE(upd),
        .D(\leds[10]_i_1_n_0 ),
        .Q(leds_OBUF[10]),
        .R(reset_IBUF));
  FDRE \leds_reg[11] 
       (.C(clk_IBUF_BUFG),
        .CE(upd),
        .D(\leds[11]_i_1_n_0 ),
        .Q(leds_OBUF[11]),
        .R(reset_IBUF));
  FDRE \leds_reg[12] 
       (.C(clk_IBUF_BUFG),
        .CE(upd),
        .D(\leds[12]_i_1_n_0 ),
        .Q(leds_OBUF[12]),
        .R(reset_IBUF));
  FDRE \leds_reg[13] 
       (.C(clk_IBUF_BUFG),
        .CE(upd),
        .D(\leds[13]_i_1_n_0 ),
        .Q(leds_OBUF[13]),
        .R(reset_IBUF));
  FDRE \leds_reg[14] 
       (.C(clk_IBUF_BUFG),
        .CE(upd),
        .D(\leds[14]_i_1_n_0 ),
        .Q(leds_OBUF[14]),
        .R(reset_IBUF));
  FDRE \leds_reg[15] 
       (.C(clk_IBUF_BUFG),
        .CE(upd),
        .D(\leds[15]_i_1_n_0 ),
        .Q(leds_OBUF[15]),
        .R(reset_IBUF));
  FDRE \leds_reg[1] 
       (.C(clk_IBUF_BUFG),
        .CE(upd),
        .D(\leds[1]_i_1_n_0 ),
        .Q(leds_OBUF[1]),
        .R(reset_IBUF));
  FDRE \leds_reg[2] 
       (.C(clk_IBUF_BUFG),
        .CE(upd),
        .D(\leds[2]_i_1_n_0 ),
        .Q(leds_OBUF[2]),
        .R(reset_IBUF));
  FDRE \leds_reg[3] 
       (.C(clk_IBUF_BUFG),
        .CE(upd),
        .D(\leds[3]_i_1_n_0 ),
        .Q(leds_OBUF[3]),
        .R(reset_IBUF));
  FDRE \leds_reg[4] 
       (.C(clk_IBUF_BUFG),
        .CE(upd),
        .D(\leds[4]_i_1_n_0 ),
        .Q(leds_OBUF[4]),
        .R(reset_IBUF));
  FDRE \leds_reg[5] 
       (.C(clk_IBUF_BUFG),
        .CE(upd),
        .D(\leds[5]_i_1_n_0 ),
        .Q(leds_OBUF[5]),
        .R(reset_IBUF));
  FDRE \leds_reg[6] 
       (.C(clk_IBUF_BUFG),
        .CE(upd),
        .D(\leds[6]_i_1_n_0 ),
        .Q(leds_OBUF[6]),
        .R(reset_IBUF));
  FDRE \leds_reg[7] 
       (.C(clk_IBUF_BUFG),
        .CE(upd),
        .D(\leds[7]_i_1_n_0 ),
        .Q(leds_OBUF[7]),
        .R(reset_IBUF));
  FDRE \leds_reg[8] 
       (.C(clk_IBUF_BUFG),
        .CE(upd),
        .D(\leds[8]_i_1_n_0 ),
        .Q(leds_OBUF[8]),
        .R(reset_IBUF));
  FDRE \leds_reg[9] 
       (.C(clk_IBUF_BUFG),
        .CE(upd),
        .D(\leds[9]_i_1_n_0 ),
        .Q(leds_OBUF[9]),
        .R(reset_IBUF));
  IBUF reset_IBUF_inst
       (.I(reset),
        .O(reset_IBUF));
endmodule
