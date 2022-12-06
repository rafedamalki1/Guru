/* You must connect to a non-PROGRESS database to run this procedure */

DEFINE VARIABLE r1 AS RAW.

FIND FIRST customer.
r1 = RAW(name, 8, 4).
