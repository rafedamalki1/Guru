/* p-ftchp3.p */

DEFINE VARIABLE nsalesrep AS CHARACTER FORMAT "x(3)".
DEFINE VARIABLE answer AS LOGICAL
    FORMAT "Change/Keep".

REPEAT:
    answer = false.
    PROMPT-FOR customer.sales-rep
	LABEL "Sales rep's initials" WITH FRAME a.
    FOR EACH customer WHERE customer.sales-rep =
			    input customer.sales-rep:
	DISPLAY name FORMAT "x(30)" WITH 1 DOWN.
	UPDATE answer LABEL
	    "Change the salesrep on this account?(change/keep)"
	    WITH FRAME b SIDE-LABELS NO-BOX.
	IF answer THEN DO:
	    SET nsalesrep
		LABEL "Enter new sales rep's initials"
		WITH FRAME c SIDE-LABELS NO-BOX.
	    customer.sales-rep = nsalesrep.
	END.
    END.
END.
