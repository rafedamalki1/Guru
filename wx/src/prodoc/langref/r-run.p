/* r-run.p */

DEFINE VARIABLE selection AS CHARACTER
    LABEL "Enter Program Choice" FORMAT "x(1)".
DEFINE VARIABLE programs AS CHARACTER FORMAT "x(15)" EXTENT 5.

/* Create the procedures custrpt.p, custedit.p, ordrpt.p, and ordedit.p. */

programs[1] = "custrpt.p".
programs[2] = "custedit.p".
programs[3] = "ordrpt.p".
programs[4] = "ordedit.p".
programs[5] = "r-exit.p".

REPEAT:
    FORM HEADER TODAY "MASTER MENU" AT 35 STRING(TIME,"hh:mm") to 79.
    FORM SKIP(3)
	 "1 - Customer Listing"  AT 30
	 "2 - Customer Update" AT 30
	 "3 - Order Listing"  AT 30
	 "4 - Order Update"   AT 30
	 "5 - Quit System"    AT 30
	 selection COLON 28 AUTO-RETURN WITH SIDE-LABELS NO-BOX 1 DOWN.

    UPDATE selection
	VALIDATE(INDEX("12345",selection) NE 0,"Not a valid choice").
    HIDE ALL.
    RUN VALUE(programs[INDEX("12345",selection)]).
END.
