/* r-index.p */

DEFINE VARIABLE x AS CHARACTER FORMAT "9"
    LABEL "Enter a digit between 1 and 5".
DEFINE VARIABLE show AS CHARACTER FORMAT "x(5)" EXTENT 5 LABEL "Literal".

show[1] = "One".
show[2] = "Two".
show[3] = "Three".
show[4] = "Four".
show[5] = "Five".

REPEAT:
    SET x AUTO-RETURN.
    IF INDEX("12345",x) = 0 THEN DO:
	MESSAGE "x must be 1,2,3,4, or 5. Try again.".
	UNDO, RETRY.
    END.
    ELSE DISPLAY show[INTEGER(x)].
END.
