/* r-caps.p */

REPEAT:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num.
    UPDATE name address city state.
    customer.state = CAPS(customer.state).
    DISPLAY customer.state.
END.
