/*r-imprt.p */
INPUT FROM customer.d.

DISABLE TRIGGERS FOR LOAD OF customer.

REPEAT:
    CREATE customer.
    IMPORT customer.
END.

INPUT CLOSE.
