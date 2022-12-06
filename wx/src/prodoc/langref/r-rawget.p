/* You must connect to a non-PROGRESS database to run this procedure */

DEFINE VARIABLE i AS INTEGER.

FOR EACH Customer:
   i = GET-BYTE(RAW(name), 1).
   IF i = 83
   THEN DISPLAY Name.
END.
