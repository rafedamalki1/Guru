/* r-delet2.p */

DEFINE VARIABLE del AS LOGICAL FORMAT "y/n".

REPEAT:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num.
    DISPLAY name.
    del = no.
    UPDATE del LABEL "Enter ""y"" to confirm delete".
    IF del THEN DELETE customer.
END.
