/* p-exprc3.p */

DEFINE INPUT PARAMETER db-procs AS HANDLE.
DEFINE VARIABLE t-i-cust-num LIKE customer.cust-num.

REPEAT:
   UPDATE t-i-cust-num.
   RUN getcust IN db-procs (INPUT t-i-cust-num).
END.

