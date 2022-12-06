/* p-impun2.p */

DEFINE VARIABLE file-line AS CHARACTER.

INPUT FROM p-datfl3.d.

REPEAT:
   CREATE customer.
   IMPORT UNFORMATTED file-line.
   ASSIGN customer.cust-num = INTEGER(SUBSTRING(file-line, 1, 2))
          customer.name = TRIM(SUBSTRING(file-line, 4, 17))
          sales-rep = SUBSTRING(file-line, 22, 3).
END.

INPUT CLOSE.
