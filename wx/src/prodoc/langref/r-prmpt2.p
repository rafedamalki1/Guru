/* r-promp2.p */

REPEAT:
    PROMPT-FOR salesrep.sales-rep LABEL "Sales rep's initials"
	WITH FRAME namefr ROW 2 SIDE-LABELS.
    FIND salesrep USING sales-rep.
    DISPLAY rep-name region month-quota WITH 1 DOWN NO-HIDE.
END.
