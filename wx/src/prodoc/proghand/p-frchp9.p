/* p-frchp9.p */

REPEAT WITH FRAME a:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num.
    DISPLAY name.
END.
FOR EACH customer WITH FRAME a:
    DISPLAY name.
END.
