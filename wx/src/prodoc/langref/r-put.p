/* r-put.p */

DEFINE STREAM s1.

OUTPUT STREAM s1 TO cus.dat.

FOR EACH customer:
    PUT STREAM s1 name "/".
END.

OUTPUT STREAM s1 CLOSE.
