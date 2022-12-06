/* r-match.p */

FOR EACH customer WHERE address MATCHES("*St"):
    DISPLAY name address city state country.
END.
