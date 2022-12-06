/* r-decml.p */

DEFINE VARIABLE new-max AS CHARACTER FORMAT "x(10)".

REPEAT:
    PROMPT-FOR customer.cust-num WITH FRAME credit.
    FIND customer USING cust-num.
    DISPLAY cust-num name credit-limit WITH FRAME credit DOWN.
    DISPLAY "Enter one of:" SKIP(1)
	    "a = 5000" SKIP
	    "b = 2000" SKIP
	    "RETURN = 1000"
	    "A dollar value"
	    WITH FRAME vals COLUMN 60.
    SET new-max WITH FRAME credit.
    IF new-max = "a" THEN credit-limit = 5000.
    ELSE IF new-max = "b" THEN credit-limit = 2000.
    ELSE IF new-max > "0" AND new-max < "999,999.99" THEN
	    credit-limit = DECIMAL(new-max).
    ELSE credit-limit = 1000.
    DISPLAY credit-limit WITH FRAME credit.
END.
