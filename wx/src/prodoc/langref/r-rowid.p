/* r-rowid.p */

DEFINE VARIABLE response AS LOGICAL.
DEFINE VARIABLE crowid AS ROWID.

REPEAT:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num NO-LOCK.
    crowid = ROWID(customer).
    DISPLAY name .
    response = YES.
    UPDATE response LABEL "Update credit-limit ?".
    IF response THEN DO:
	FIND customer WHERE ROWID(customer) = crowid EXCLUSIVE-LOCK.
	UPDATE credit-limit.
    END.
END.
