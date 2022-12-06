/* p-frchp8.p */

/* Now the scope of frame bbb is the Procedure block
   because that is the block in which it is
   first referenced:  by the FORM statement. */

FORM WITH FRAME bbb.
DISPLAY "Customer Display" WITH FRAME aaa.
REPEAT WITH FRAME bbb:
    PROMPT-FOR customer.cust-num WITH FRAME aaa.
    FIND customer USING cust-num.
    DISPLAY customer WITH 2 COLUMNS.
END.
