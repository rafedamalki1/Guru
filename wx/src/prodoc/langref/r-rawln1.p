/* You must connect to a non-PROGRESS database to run this procedure */

DEFINE VARIABLE r1 AS RAW.

FIND customer WHERE cust-num = 29.
r1 = RAW(name).
LENGTH(r1) = 2.
