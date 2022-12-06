/* r-updat3.p */

REPEAT:
     PROMPT-FOR customer.cust-num.
     FIND customer USING cust-num.
     UPDATE name address city customer.state country WITH 1 COLUMN 1 DOWN.
END.
