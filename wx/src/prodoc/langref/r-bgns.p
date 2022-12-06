/* r-bgns.p */

DEFINE VARIABLE cname LIKE customer.name LABEL "Name".

REPEAT:
    SET cname WITH SIDE-LABELS.
    FOR EACH customer WHERE name BEGINS cname:
	DISPLAY name address city state postal-code.
    END.
END.
