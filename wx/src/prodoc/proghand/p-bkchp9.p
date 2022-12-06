/* p-bkchp9.p */

/* The FOR option in the block header explicitly scopes
   the customer record to the DO block. */

DO FOR customer:
    PROMPT-FOR cust-num.
    FIND customer USING cust-num.
    DISPLAY customer WITH 2 COLUMNS.
END.
