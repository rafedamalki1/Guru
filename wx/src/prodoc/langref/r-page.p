/* r-page.p */

DEFINE VARIABLE laststate AS CHARACTER.

OUTPUT TO PRINTER.
FOR EACH customer BY state:
    IF state <> laststate THEN DO:
	IF laststate <> "" THEN PAGE.
	laststate = state.
    END.
    DISPLAY cust-num name address city state.
END.
