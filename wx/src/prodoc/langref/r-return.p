/* r-return.p */

DEFINE NEW SHARED VARIABLE nfact AS INTEGER LABEL "N Factorial"
                       FORMAT ">,>>>,>>>,>>9".
DEFINE VARIABLE n AS INTEGER FORMAT "->9" LABEL "N".

REPEAT:
    SET n SPACE(5).
    nfact = n.
    RUN r-fact.p.
    IF RETURN-VALUE <> ""
    THEN DO:
       BELL.
       MESSAGE RETURN-VALUE.
    END.
    ELSE DISPLAY nfact.
END.
