/* r-nenter.p */

DEFINE VARIABLE new-max LIKE credit-limit.

FOR EACH CUSTOMER:
    DISPLAY cust-num name credit-limit LABEL "current max credit"
	WITH FRAME a 1 DOWN ROW 1.
    SET new-max LABEL "new max credit"
	WITH SIDE-LABELS NO-BOX ROW 10 FRAME b.
    IF new-max NOT ENTERED OR new-max = credit-limit THEN DO:
	DISPLAY "No Change In credit-limit" WITH FRAME d ROW 15.
	NEXT.
    END.
    DISPLAY "Changing Credit Limit of" name SKIP
	    "from" credit-limit "to"
	    new-max WITH FRAME c ROW 15 NO-LABELS.
    credit-limit = new-max.
END.
