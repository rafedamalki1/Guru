/* r-linec.p */

OUTPUT TO PRINTER.
FOR EACH customer BREAK BY state:
    DISPLAY cust-num name address city state.
    IF LAST-OF(state) THEN DO:
	IF LINE-COUNTER + 4 > PAGE-SIZE
	THEN PAGE.
	ELSE DOWN 1.
    END.
END.
