


/* r-fill.p */

DEFINE VARIABLE percentg AS INTEGER FORMAT ">>9".
DEFINE VARIABLE fillchar AS CHARACTER FORMAT "x".

fillchar =  "*".

FOR EACH customer:
	ACCUMULATE balance (TOTAL).
END.

DISPLAY "Percentage of Outstanding Balance" WITH CENTERED NO-BOX.

FOR EACH customer WHERE balance > 0:
	percentg = balance / (ACCUM TOTAL balance) * 100.
	FORM SKIP  name percentg LABEL "%" bar AS CHAR
		LABEL "  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17"
		FORMAT "x(50)" WITH NO-BOX NO-UNDERLINE USE-TEXT.
	COLOR DISPLAY BRIGHT-RED bar.
	DISPLAY name percentg FILL(fillchar,percentg * 3) @ bar.
END.





