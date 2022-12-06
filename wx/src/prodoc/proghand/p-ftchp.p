/* p-ftchp.p */

DEFINE VARIABLE nsalesrep AS CHARACTER.
DEFINE VARIABLE answer AS LOGICAL.

REPEAT:
    PROMPT-FOR customer.sales-rep
	LABEL "Sales rep's initials" WITH FRAME a.
    FOR EACH customer WHERE customer.sales-rep =
			    input customer.sales-rep:
	DISPLAY name WITH 1 DOWN.
	SET answer LABEL
	    "Change the sales rep on this account?"
	    WITH FRAME b SIDE-LABELS NO-BOX.
	IF answer THEN DO:
	    SET nsalesrep
		LABEL "Enter new sales rep's initials"
		WITH FRAME c SIDE-LABELS NO-BOX.
	    customer.sales-rep = nsalesrep.
	END.
    END.
END.
