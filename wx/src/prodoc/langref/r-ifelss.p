/* r-ifelss.p */

DEFINE VARIABLE ans AS LOGICAL.
DEFINE STREAM due.

OUTPUT STREAM due TO ovrdue.lst.
   DISPLAY STREAM due
	   "Orders shipped but still unpaid as of" TODAY SKIP(2)
	   WITH NO-BOX NO-LABELS CENTERED FRAME hdr PAGE-TOP.

FOR EACH order WITH FRAME oinfo:
    FIND customer OF order NO-LOCK.
    DISPLAY order-num name order-date promise-date ship-date.
    IF ship-date = ? THEN DO:
	IF promise-date = ? THEN DO:
	    MESSAGE "Please update the promise date.".
	    REPEAT WHILE promise-date = ?:
		UPDATE promise-date WITH FRAME oinfo.
	    END.
	END.
	ans = FALSE.
	MESSAGE "Has this order been shipped?" UPDATE ans.
	IF ans THEN REPEAT WHILE ship-date = ?:
	    UPDATE ship-date WITH FRAME oinfo.
	END.
    END.
    ELSE DO:
	ans = TRUE.
	MESSAGE "Has this order been paid?" UPDATE ans.
	IF NOT ans THEN DO:
	    DISPLAY STREAM due order-num TO 14 name AT 18
		    order-date AT 42 ship-date AT 54
	    WITH NO-BOX DOWN FRAME unpaid.
	END.
    END.
END.
OUTPUT STREAM due CLOSE.
