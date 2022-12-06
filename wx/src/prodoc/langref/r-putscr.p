/* r-putscr.p */

DEFINE VARIABLE paid-owed AS DECIMAL.
DEFINE VARIABLE bal-label AS CHARACTER FORMAT "x(20)".

FOR EACH customer:
    paid-owed = balance.
    IF  paid-owed < 0  /* Customer has a credit */
    THEN DO:
	paid-owed = - paid-owed.
	bal-label = "Customer Credit     ".
    END.
    ELSE
	bal-label = "Unpaid balance      ".
    DISPLAY cust-num name
	    paid-owed LABEL "                      " WITH 1 DOWN.
    IF balance < 0
    THEN
	PUT SCREEN COLOR MESSAGES ROW 2 COLUMN 34 bal-label.
    ELSE
	PUT SCREEN ROW 2 COLUMN 34 bal-label.
END.
