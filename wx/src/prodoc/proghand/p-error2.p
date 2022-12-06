/* p-error2.p */

REPEAT WITH 1 COLUMN 1 DOWN ON ERROR UNDO, LEAVE:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num.
    DISPLAY name address city state postal-code credit-limit.
END.
