/* r-runpr1.p */

DEFINE VARIABLE new-param AS CHARACTER FORMAT "x(20)".
DEFINE VARIABLE out-param AS DECIMAL FORMAT "$->>>,>>>,>>9.99".
DEFINE VARIABLE in-param AS INTEGER INIT 20.


RUN r-param1.p (OUTPUT out-param, 10, OUTPUT new-param, in-param).
DISPLAY out-param LABEL "Updated YTD Sales"   SKIP
     new-param LABEL "Status" WITH SIDE-LABELS.
