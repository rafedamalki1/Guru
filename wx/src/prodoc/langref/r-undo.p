/* r-undo.p */

DEFINE VARIABLE ans AS LOGICAL.

REPEAT FOR salesrep WITH ROW 7 1 COLUMN 1 DOWN CENTERED
		    ON ENDKEY UNDO, LEAVE:
    PROMPT-FOR sales-rep.
    FIND salesrep USING sales-rep NO-ERROR.
    IF NOT AVAILABLE salesrep THEN DO:
       ans = yes.
       MESSAGE "Salesrep record does not exist.".
       MESSAGE "Do you want to add a salesrep?" UPDATE ans.
       IF ans THEN DO:
	   CREATE salesrep.
	   ASSIGN sales-rep.
	   UPDATE rep-name region month-quota.
       END.
       ELSE UNDO, RETRY.
    END.
    ELSE DISPLAY salesrep.
END.
