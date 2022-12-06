/* p-frchp7.p */

/* The scope of frame aaa is the procedure block
   because that is the first block in which frame aaa
   is referenced; the scope of frame bbb is the REPEAT
   block.  The DISPLAY statement in the REPEAT block
   uses that frame because it is the default frame for
   the REPEAT block.  (The DISPLAY statement could over-
   ride this by explicitly naming a frame: DISPLAY WITH
   FRAME...) */

DISPLAY "Customer Display" WITH FRAME aaa.
REPEAT WITH FRAME bbb:
    PROMPT-FOR customer.cust-num WITH FRAME aaa.
    FIND customer USING cust-num.
    DISPLAY customer WITH 2 COLUMNS.
END.
