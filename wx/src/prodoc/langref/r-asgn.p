/* r-asgn.p */

REPEAT:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num NO-ERROR.
    IF NOT AVAILABLE customer
    THEN
    DO:
	CREATE customer.
	ASSIGN cust-num.
    END.
    UPDATE customer WITH 2 COLUMNS.
END.
