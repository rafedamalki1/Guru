/* r-gopend.p */

REPEAT:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num.
    UPDATE
	name address city st SKIP
	credit-limit balance WITH 1 COLUMN
	EDITING:
	    READKEY.
	    APPLY LASTKEY.
	    IF GO-PENDING AND INPUT balance > INPUT credit-limit THEN DO:
		MESSAGE "The current unpaid balance exceeds the credit limit.".
		NEXT.
	    END.
	END.
END.
