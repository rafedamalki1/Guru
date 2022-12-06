/* r-inclr.p */

DISPLAY "     Please choose     " SKIP
	" 1  Run order entry    " SKIP
	" 2  Run receivables    " SKIP
	" 3  Exit               "  WITH CENTERED FRAME menu.

REPEAT:
    READKEY.
    IF LASTKEY = KEYCODE("1") THEN RUN ordentry.
    ELSE
    IF LASTKEY = KEYCODE("2") THEN RUN receive.
    ELSE
    IF LASTKEY = KEYCODE("3") THEN QUIT.
    ELSE DO:
	MESSAGE "Sorry, that is not a valid choice".
	INPUT CLEAR.
    END.
END.
