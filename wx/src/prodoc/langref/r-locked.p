/* r-locked.p */

REPEAT:
    PROMPT-FOR customer.cust-num.
    FIND customer USING customer.cust-num
	NO-ERROR NO-WAIT.
    IF NOT AVAILABLE customer THEN DO:
	IF LOCKED customer
	THEN MESSAGE "Customer record is locked".
	ELSE MESSAGE "Customer record was not found".
	NEXT.
    END.
    DISPLAY cust-num name city state.
END.
