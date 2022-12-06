/* r-ambig.p */

DEFINE VARIABLE cname LIKE customer.name LABEL "Cust Name".

REPEAT:
    SET cname.
    FIND customer WHERE name = cname NO-ERROR.
    IF AVAILABLE customer
	THEN DISPLAY cust-num address city st postal-code.
    ELSE IF AMBIGUOUS customer
	THEN MESSAGE "There is more than one customer with that name".
    ELSE MESSAGE "Cannot find customer with that name".
END.
