/* r-sqrt.p */

DEFINE VARIABLE num AS INTEGER FORMAT ">,>>9"
    LABEL "Enter a number between 1 and 9,999".

REPEAT WITH SIDE-LABELS CENTERED
	    TITLE "SQUARE ROOT GENERATOR" COLUMN 20 1 DOWN.
    DISPLAY SKIP(2).
    SET num SKIP(2).
    DISPLAY "The square root of " + string(num) + " is" FORMAT "x(27)"
	    SQRT(num) FORMAT ">>>.9999".
END.
