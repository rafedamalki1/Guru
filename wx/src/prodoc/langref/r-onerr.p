/* r-onerr.p */

REPEAT ON ERROR UNDO, NEXT:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num.
    DISPLAY name address city state country.
END.
