/* r-ithru2.p */

DEFINE VARIABLE fn AS CHARACTER FORMAT "x(14)".

INPUT THROUGH ls NO-ECHO.

REPEAT:
    SET fn WITH NO-BOX NO-LABELS FRAME indata.
    DISPLAY fn.
END.

INPUT CLOSE.