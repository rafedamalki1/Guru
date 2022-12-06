/* r-delval.p */

DEFINE VARIABLE ans AS LOGICAL.

REPEAT WITH 1 DOWN:
    PROMPT-FOR customer.cust-num.
    FIND customer USING customer.cust-num.
    DISPLAY name.
    ans = no.
    DISPLAY "Do you want to delete this customer ?" WITH FRAME f-query.
    UPDATE ans WITH FRAME f-query NO-LABELS.
    IF ans THEN
       DELETE customer VALIDATE(NOT(CAN-FIND(order OF customer)),
       "This customer has outstanding orders and cannot be deleted").
END.
