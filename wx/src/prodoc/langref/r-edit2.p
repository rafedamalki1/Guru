/* r-edit2.p */

PROMPT-FOR customer.cust-num.
FIND customer USING cust-num.

/* Update customer fields, monitoring each keystroke during the UPDATE */

UPDATE name address city state SKIP
       sales-rep HELP "Use the space bar to select a sales-rep" WITH 2 COLUMNS
    EDITING:

	/* Read a keystroke */
	READKEY.

	/* If the cursor is in any field except sales-rep, execute the
	   last key pressed and go on to the next iteration of this
	   EDITING phrase to check the next key */
	IF FRAME-FIELD <> "sales-rep" THEN DO:
	    APPLY LASTKEY.
	    IF GO-PENDING THEN LEAVE.
	    ELSE NEXT.
	END.

	/* When in the sales-rep field, if the last key pressed was
	   the space bar then cycle through the sales reps */
	IF LASTKEY = KEYCODE(" ") THEN DO:
	    FIND NEXT salesrep NO-ERROR.
	    IF NOT AVAILABLE salesrep THEN FIND FIRST salesrep.
	    DISPLAY salesrep.sales-rep @ customer.sales-rep.
	    NEXT.
	END.

	/* If the user presses any one of a set of keys while in the
	   sales-rep field, immediately execute that key */
	IF LOOKUP(KEYFUNCTION(LASTKEY),
	      "TAB,BACK-TAB,GO,RETURN,END-ERROR") > 0
	THEN APPLY LASTKEY.
	ELSE BELL.
    END.
