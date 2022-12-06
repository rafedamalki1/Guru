/* r-aggreg.p */

FOR EACH customer BREAK BY country:
    DISPLAY name country balance (SUB-TOTAL BY country) .
END.
