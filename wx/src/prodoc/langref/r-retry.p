/* r-retry.p */

REPEAT:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num.
    
    IF NOT RETRY
    THEN DISPLAY name address city state country.
    ELSE DISPLAY country.
    
    SET name address city state country.
    IF country = "" THEN UNDO, RETRY.
END.
