/* r-down.p */

DEFINE VARIABLE laststate AS CHARACTER.

FOR EACH customer BY state:
    IF state <> laststate THEN DO:
	IF laststate <> "" THEN DOWN 1.
	laststate = state.
    END.
    DISPLAY cust-num name city state.
END.
