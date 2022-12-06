/* r-keywd.p */

DEFINE VARIABLE formname AS CHARACTER FORMAT "x(20)".

REPEAT ON ERROR UNDO, RETRY:
    UPDATE formname.
    IF KEYWORD(formname) NE ?
    THEN DO:
	MESSAGE formname + " may not be used as a form name".
	UNDO, RETRY.
    END.
    ELSE LEAVE.
END.
