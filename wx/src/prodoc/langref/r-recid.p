/* r-recid.p */

DEFINE VARIABLE response AS LOGICAL.
DEFINE VARIABLE crecid AS RECID.

REPEAT:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num NO-LOCK.
    crecid = RECID(customer).
    DISPLAY name .
    response = YES.
    UPDATE response LABEL "Update credit-limit ?".
    IF response THEN DO:
	FIND customer WHERE RECID(customer) = crecid EXCLUSIVE-LOCK.
	UPDATE credit-limit.
    END.
END.
