/* r-uneg.p */

DEFINE VARIABLE x AS DECIMAL LABEL "X".
DEFINE VARIABLE abs-x AS DECIMAL LABEL "ABS(X)".

REPEAT:
   SET x.
   IF x < 0
   THEN abs-x =- x.
   ELSE abs-x =x.
   DISPLAY abs-x.
END.
