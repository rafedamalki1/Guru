/* r-find2.p */

DEFINE VARIABLE start-name LIKE customer.name.

REPEAT:
    SET start-name.
    FIND FIRST customer WHERE name >= start-name.
    REPEAT:
	DISPLAY name.
	FIND NEXT customer USE-INDEX name.
    END.
END.
