/* r-prmpt.p */

REPEAT:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num NO-ERROR.
    IF NOT AVAILABLE customer THEN DO:
	MESSAGE "No such customer number." .
	UNDO, RETRY.
    END.
    DISPLAY name phone sales-rep.
END.
