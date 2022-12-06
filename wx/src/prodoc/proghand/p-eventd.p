


/* p-eventd.p */

DEFINE BUTTON btn_1 LABEL "Next".
DEFINE BUTTON btn_2 LABEL "Prev".

FIND FIRST customer.
DISPLAY cust-num name phone SKIP (0.5)
			btn_2 TO 40 SPACE btn_1
			SKIP (0.5)
		WITH FRAME f SIDE-LABELS.

ON CHOOSE OF btn_1 DO:
	FIND NEXT customer NO-ERROR.
	IF NOT AVAILABLE customer THEN DO:
			FIND LAST customer.
			BELL.
	END.
	DISPLAY cust-num name phone WITH FRAME f.
END.

ON CHOOSE OF btn_2 DO:
	FIND PREV customer NO-ERROR.
	IF NOT AVAILABLE customer THEN DO:
			FIND FIRST customer.
			BELL.
	END.
	DISPLAY cust-num name phone WITH FRAME f.
END.

ON LEAVE OF phone DO:
	IF INPUT customer.phone NE customer.phone THEN DO:
		ASSIGN customer.phone.
		BELL.
		MESSAGE "Phone Number changed."
			VIEW-AS ALERT-BOX INFORMATION.
	END.
END.

ENABLE phone btn_2 btn_1 WITH FRAME f.
WAIT-FOR GO OF FRAME f.




