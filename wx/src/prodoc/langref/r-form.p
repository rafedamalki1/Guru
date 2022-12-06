/* r-form.p */

REPEAT FOR customer:

    FORM name    COLON 10        phone     COLON 50
	 address COLON 10        sales-rep COLON 50  SKIP
	 city    COLON 10 NO-LABEL state NO-LABEL postal-code NO-LABEL
	 WITH SIDE-LABELS 1 DOWN CENTERED.

    PROMPT-FOR cust-num WITH FRAME cnum SIDE-LABELS CENTERED.
    FIND customer USING cust-num.
    UPDATE name address city state postal-code phone sales-rep.

END.
