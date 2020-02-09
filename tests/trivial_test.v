
// a comment to test

module toplevel ( xa, xb, xc );

input [10:0] xa;
input [10:0] xb;

output ya, yb;

wire [41:42] q;
reg b,c,d;

my_module #( .BLAH(Q)) my_name ( .a (xa ),
                                 .b ( 1 ),
                                 .pq (1'b1),
                                 .er (16'habcd_123),
                                 .tr ('h1234),
                                 .c (ya ) );

//assign ya = &xa;
//assign yb = ~&xb;

endmodule

