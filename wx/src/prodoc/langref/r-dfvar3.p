/* r-dfvar3.p */

DEFINE NEW GLOBAL SHARED VARIABLE first-time AS LOGICAL INITIAL TRUE.
DEFINE VARIABLE selection AS INTEGER FORMAT "9" LABEL "Selection".

IF first-time THEN DO:
    RUN r-init.p.
    first-time = false.
END.
FORM
    "    MAIN MENU          " SKIP(1)
    "1 - Accounts Payable   " SKIP
    "2 - Accounts Receivable" WITH CENTERED ROW 5 FRAME menu.
REPEAT:
    VIEW FRAME menu.
    UPDATE selection AUTO-RETURN WITH FRAME sel CENTERED ROW 12 SIDE-LABELS.
    IF selection = 1 THEN DO:
	HIDE FRAME menu.
	HIDE FRAME sel.
	RUN apmenu.p.
    END.
    ELSE IF selection = 2 THEN DO:
	HIDE FRAME menu.
	HIDE FRAME sel.
	RUN armenu.p.
    END.
    ELSE DO:
	MESSAGE "Invalid selection. Try again".
	UNDO, RETRY.
    END.
END.
