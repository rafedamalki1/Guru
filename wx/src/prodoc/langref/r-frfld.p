/* r-frfld.p */

FOR EACH customer:
    UPDATE cust-num name address address2 city state postal-code
	   WITH 1 DOWN 1 COLUMN CENTERED EDITING:
	DISPLAY "You are editing field:" FRAME-FIELD SKIP
		"Of file:" FRAME-FILE SKIP
		"Its value is:" FRAME-VALUE FORMAT "x(20)"
		WITH FRAME a ROW 15 NO-LABELS CENTERED.
	READKEY.
	APPLY LASTKEY.
    END. /* Editing */
END.
