/* r-quit.p - QUIT */

DEFINE VARIABLE ans AS INTEGER FORMAT "9".
DEFINE VARIABLE proc AS CHARACTER EXTENT 4.

/* Create the procedures newcust.p, chgcust.p, delcust.p, and prncust.p. */

proc[1] = "newcust.p".
proc[2] = "chgcust.p".
proc[3] = "delcust.p".
proc[4] = "prncust.p".

REPEAT:
    FORM
	"    CUSTOMER MAINTENANCE    " SKIP(2)
	"1 - Create New Customer     " SKIP(1)
	"2 - Change Existing Customer" SKIP(1)
	"3 - Delete Customer         " SKIP(1)
	"4 - Print Customer List     " SKIP(1)
	"5 - Exit From PROGRESS      " SKIP(2)
	"SELECTION" ans WITH NO-BOX NO-LABELS CENTERED FRAME cusmaint1.
    UPDATE ans WITH FRAME cusmaint1.
    IF ans = 5 THEN QUIT.
    ELSE IF ans < 1 OR ans > 4 THEN DO:
	     MESSAGE "Invalid Choice. Please try again.".
	     UNDO, RETRY.
    END.
    HIDE FRAME cusmaint1.
    RUN VALUE(proc[ans]).
END.
