/* p-impun1.p */

INPUT FROM p-datfl2.d.

REPEAT:
   CREATE customer.
   IMPORT customer.cust-num.
   IMPORT UNFORMATTED customer.name.
   IMPORT customer.sales-rep.
END.

INPUT CLOSE.
