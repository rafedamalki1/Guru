/* r-not.p */

REPEAT:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num NO-ERROR.
    IF NOT AVAILABLE customer
    THEN DO:
	MESSAGE "Customer with cust-num:" INPUT cust-num
		" does not exist. Please try another.".
	UNDO, RETRY.
    END.
    ELSE DO:
	DISPLAY name phone.
    END.
END.
