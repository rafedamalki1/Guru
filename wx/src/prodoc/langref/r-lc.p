/* r-lc.p */

REPEAT:
    PROMPT-FOR Customer.Cust-num.
    FIND Customer USING Cust-num.
    DISPLAY Name.
    UPDATE sales-rep.
    sales-rep =
	CAPS(SUBSTRING(sales-rep, 1, 1) ) +
	LC(SUBSTRING(Sales-rep, 2) ).
    DISPLAY Sales-rep.
END.
