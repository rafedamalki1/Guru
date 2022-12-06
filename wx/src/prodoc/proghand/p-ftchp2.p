/* p-ftchp2.p */

DEFINE VARIABLE test AS CHAR FORMAT "x(10)".
REPEAT:
    SET test FORMAT "x(20)".
    DISPLAY test WITH FRAME aaa.
    DISPLAY test FORMAT "X(20)" WITH FRAME bbb.
END.
