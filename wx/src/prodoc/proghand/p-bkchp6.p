/* p-bkchp6.p */

/* The default frame for the procedure block is an
   unnamed frame.  The first DISPLAY statements uses a
   different frame, aaa.  The default-frame for the
   REPEAT block is bbb.  The PROMPT-FOR statement does
   not use the default frame for the block; instead it
   uses frame aaa.  The DISPLAY statement uses the
   default frame for the REPEAT block. */

DISPLAY "Customer Display" WITH FRAME aaa.
REPEAT WITH FRAME bbb:
    PROMPT-FOR customer.cust-num WITH FRAME aaa.
    FIND customer USING cust-num.
    DISPLAY customer with 2 COLUMNS.
END.
