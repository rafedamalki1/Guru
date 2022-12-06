/* p-frchp3.p */

DEFINE VARIABLE answer AS LOGICAL LABEL
    "Do you want to delete this customer?".

REPEAT:
    PROMPT-FOR customer.cust-num WITH FRAME a.
    FIND customer USING cust-num.
    DISPLAY name.
    SET answer WITH FRAME a.
    IF answer THEN DELETE customer.
END.
