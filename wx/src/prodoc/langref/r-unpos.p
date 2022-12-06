/* r-unpos.p */

DEFINE VARIABLE old-max LIKE credit-limit LABEL "Old Limit".

FOR EACH customer:
    old-max =+ credit-limit.
    credit-limit =+(credit-limit + 100).
    DISPLAY name old-max credit-limit.
END.
