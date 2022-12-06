/* p-io.p */

REPEAT:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num.
    DISPLAY name state credit-limit.
END.
