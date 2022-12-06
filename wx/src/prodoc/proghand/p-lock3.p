/* p-lock3.p */

REPEAT:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num.
    UPDATE name credit-limit sales-rep.
END.
