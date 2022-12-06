/* You must connect to a non-PROGRESS database to run this procedure */

DEFINE VARIABLE r1 AS RAW.

FIND customer WHERE cust-num = 26.
DISPLAY name.
r1 = RAW(name).
PUT-BYTE(r1,1) = ASC('B').
PUT-BYTE(r1,2) = ASC('i').
PUT-BYTE(r1,3) = ASC('l').
PUT-BYTE(r1,4) = ASC('l').


RAW(name) = r1.
DISPLAY name.

