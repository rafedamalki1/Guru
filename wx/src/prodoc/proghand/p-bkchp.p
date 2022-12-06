/* p-bkchp.p */

/* This REPEAT block loops through its statements
   until the user presses the END-ERROR (F4) key. */

REPEAT:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num.
    DISPLAY customer WITH 2 COLUMNS.
END.
