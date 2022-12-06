/* r-fact.p */

DEFINE SHARED VARIABLE nfact AS INTEGER.
DEFINE VARIABLE i AS INTEGER.

IF nfact < 0
THEN RETURN "The value is negative.".

IF nfact > 12
THEN RETURN "The calculated value won't fit in an integer.".

i = nfact.
nfact = nfact - 1.

IF nfact <= 1 THEN DO:
    nfact = i.
    RETURN.
END.


RUN r-fact.p.

nfact = nfact * i.

RETURN.
