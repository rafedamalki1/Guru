/* You must connect to a non-PROGRESS database to run this procedure */

DEFINE VARIABLE r1 AS RAW.

FIND FIRST customer.
DISPLAY name.

r1 = RAW(name).
PUT-BYTE(r1, 17) = 115.
PUT-BYTE(r1, 18) = 0.
RAW(name) = r1.

DISPLAY name.
