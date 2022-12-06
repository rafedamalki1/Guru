/* r-in.p */

INPUT FROM VALUE(SEARCH("r-in.dat")).

REPEAT:
    PROMPT-FOR cust.cust-num credit-limit.
    FIND customer USING INPUT cust-num.
    ASSIGN credit-limit.
END.
INPUT CLOSE.
