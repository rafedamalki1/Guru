/* r-nprmpt.p */

FOR EACH customer:
    UPDATE customer WITH 2 COLUMNS.
    IF contact EQ " "
    THEN DO:
	MESSAGE "You must enter a contact".
	NEXT-PROMPT contact.
	UNDO, RETRY.
    END.
END.
