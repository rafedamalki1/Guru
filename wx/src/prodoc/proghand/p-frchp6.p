/* p-frchp6.p */

/* The scope of frame aaa is the procedure block
   because that is the first block in which frame aaa
   is referenced */

DISPLAY "Customer Display" WITH FRAME aaa.
REPEAT:
    PROMPT-FOR customer.cust-num WITH FRAME aaa.
    FIND customer USING cust-num.
    DISPLAY customer WITH 2 COLUMNS.
END.
