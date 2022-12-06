/* p-imprt2.p */

INPUT FROM p-datfl7.d.

REPEAT:
   CREATE customer.
   IMPORT DELIMITER "," cust-num name sales-rep.
END.

OUTPUT CLOSE.
