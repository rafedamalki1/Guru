/* p-lock4.p */

DEFINE VARIABLE answer AS LOGICAL.

REPEAT:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num.
    UPDATE name credit-limit sales-rep.
    SET answer LABEL "Update OK?".
    IF NOT answer THEN UNDO, NEXT.
END.
