/* r-enter.p */

DEFINE VARIABLE new-max LIKE credit-limit.

FOR EACH customer:
    DISPLAY cust-num name credit-limit LABEL "Current credit limit"
	WITH FRAME a 1 DOWN ROW 1.
    SET new-max LABEL "New credit limit"
	WITH SIDE-LABELS NO-BOX ROW 10 FRAME b.
    IF new-max ENTERED THEN DO:
	IF new-max <> credit-limit THEN DO:
	    DISPLAY "Changing Credit Limit of" name SKIP
		    "from" credit-limit "to"
		    new-max WITH FRAME c ROW 15 NO-LABELS.
	    credit-limit = new-max.
	    NEXT.
	END.
    END.
    DISPLAY "No Change In Credit Limit" WITH FRAME d ROW 15.
END.
