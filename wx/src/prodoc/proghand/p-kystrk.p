/* p-kystrk.p */

REPEAT:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num.
    UPDATE name SKIP address SKIP city SKIP state SKIP sales-rep
	   HELP "Use the space bar to select a sales-rep"
	   WITH SIDE-LABELS
	 EDITING:
	    READKEY.
	    IF FRAME-FIELD <> "sales-rep" THEN DO:
		APPLY LASTKEY.
		IF GO-PENDING
		THEN LEAVE.
		NEXT.
	    END.
	    IF LASTKEY = KEYCODE(" ") THEN DO:
		FIND NEXT salesrep NO-ERROR.
		IF NOT AVAILABLE salesrep
		THEN FIND FIRST salesrep.
		DISPLAY salesrep.sales-rep @ customer.sales-rep.
		NEXT.
	     END.
	     IF LOOKUP(KEYFUNCTION(LASTKEY),
		       "TAB,BACK-TAB,GO,RETURN,END-ERROR") > 0
	     THEN APPLY LASTKEY.
	     ELSE BELL.
	 END.
END.
