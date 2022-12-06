/* r-bgns2.p */

DEFINE VARIABLE cname LIKE customer.name LABEL "Name".

REPEAT:
    SET cname WITH SIDE-LABELS.
    /* create MATCHES pattern */
    cname = cname + "*".
    FOR EACH customer WHERE name MATCHES cname:
	DISPLAY name address city st postal-code.
    END.
END.
