/* r-new.p */

REPEAT:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num NO-ERROR.
    IF NOT AVAILABLE customer
    THEN DO WITH FRAME newcus:
	MESSAGE "Creating new customer".
	CREATE customer.
	ASSIGN cust-num.
	UPDATE name address city st country.
    END.

    CREATE order.
    order.cust-num = customer.cust-num.
    IF NEW customer THEN DO:
	UPDATE order.order-num promise-date.
	order.terms = "COD".
	DISPLAY order.terms.
    END.
    ELSE UPDATE order.order-num promise-date order.terms.
END.
