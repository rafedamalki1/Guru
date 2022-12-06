/* r-log.p */

DEFINE VARIABLE base AS DECIMAL
    FORMAT ">>>,>>>.9999".
DEFINE VARIABLE number AS DECIMAL.

REPEAT:
    UPDATE base
	VALIDATE(base > 1, "Base must be greater than 1").

    REPEAT:
	UPDATE number
	    VALIDATE(number > 0, "Number must be positive").
	DISPLAY number LOG(number, base)
	    LABEL "LOG(NUMBER, BASE)".
    END.
END.
