/* r-do.p */

    FOR EACH customer:
	DISPLAY name credit-limit.
	PAUSE 3.
	IF credit-limit > 80000 THEN DO:
	     credit-limit = 80000.
	     DISPLAY name credit-limit.
	END.
    END.
