/* p-error4.p */

REPEAT WITH 1 COLUMN 1 DOWN:
   PROMPT-FOR customer.cust-num.
   FIND customer USING cust-num.
   UPDATE name address city state postal-code credit-limit.
END.
