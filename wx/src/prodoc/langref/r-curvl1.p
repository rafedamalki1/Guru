/* r-curvl1.p */

FIND LAST customer NO-LOCK.

IF customer.cust-num < CURRENT-VALUE(next-cust-num) AND
   customer.cust-num > 1000
THEN DO:
   CURRENT-VALUE(next-cust-num) = customer.cust-num.
   MESSAGE "The value of next-cust-num has been changed to"
           customer.cust-num VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.
ELSE MESSAGE "The value of next-cust-num remains"
             CURRENT-VALUE(next-cust-num)
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
