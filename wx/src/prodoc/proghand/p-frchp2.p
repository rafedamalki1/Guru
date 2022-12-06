/* p-frchp2.p */

REPEAT:
    FIND NEXT customer.
    IF max-credit > 500
    THEN DO:
	DISPLAY name WITH NO-LABEL NO-UNDERLINE.
	DISPLAY "Extend special offer" @ msg-area AS CHARACTER.
    END.
    ELSE DO:
	DISPLAY name WITH NO-LABEL NO-UNDERLINE.
	DISPLAY "Recheck credit" @ msg-area.
    END.
END.
