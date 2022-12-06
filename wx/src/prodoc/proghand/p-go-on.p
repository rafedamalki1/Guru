/* p-go-on.p */

DISPLAY "You may update each customer." SKIP
	"After making your changes, press one of:"
	SKIP(1)
	" F1   - Make the changes permanent" SKIP
	" F4   - Undo changes and exit     " SKIP
	" F9   - Undo changes and try again" SKIP
	" F10  - Find next customer        " SKIP
	" F11  - Find previous customer    "
	WITH CENTERED FRAME instr.

FIND FIRST customer.
REPEAT:
    UPDATE cust-num name address st
	GO-ON(F9 F10 F11) WITH 1 DOWN CENTERED.
    IF LASTKEY = KEYCODE("F9")
    THEN UNDO, RETRY.
    ELSE IF LASTKEY = KEYCODE("F10")
    THEN FIND NEXT customer.
    ELSE IF LASTKEY = KEYCODE("F11")
    THEN FIND PREV customer.
END.
