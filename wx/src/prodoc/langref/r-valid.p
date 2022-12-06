/* r-valid.p */

REPEAT FOR item:
    PROMPT-FOR item-num.
    FIND item USING item-num NO-ERROR.
    IF NOT AVAILABLE item THEN DO:
	CREATE item.
	ASSIGN item-num.
	UPDATE item-name price.
	VALIDATE item.
    END.
    ELSE DISPLAY item-name price.
END.
