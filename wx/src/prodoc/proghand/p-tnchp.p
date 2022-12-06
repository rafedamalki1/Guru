/* p-tnchp.p */

DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE j AS INTEGER.

cusloop:
REPEAT:
    i = i + 1.
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num.
    DISPLAY name.
    FOR EACH order OF customer:
	j = j + 1.
	DISPLAY order-num.
	UPDATE odate.
	IF odate > TODAY THEN UNDO cusloop, RETRY cusloop.
    END.
END.
