/* r-lastky.p */

DISPLAY "You may update each customer.  After making your changes,"
	SKIP "Press one of:" SKIP(1)
	KBLABEL("GO") "Make the changes permanent"  SKIP
	KBLABEL("END-ERROR")  "Undo changes and exit"       SKIP
	"F9" SPACE(7) "Undo changes and try again"  SKIP
	"F10" SPACE(6) "Find next customer"          SKIP
	"F12" SPACE(6) "Find previous customer"
	 WITH CENTERED FRAME instr.

FIND FIRST customer.
REPEAT:
    UPDATE cust-num name address city state
	    GO-ON(F9 F10 F12) WITH 1 DOWN.
    IF LASTKEY = KEYCODE("F9")
    THEN UNDO, RETRY.
    ELSE IF LASTKEY = KEYCODE("F10")
    THEN FIND NEXT customer.
    ELSE IF LASTKEY = KEYCODE("F12")
    THEN FIND PREV customer.
END.
