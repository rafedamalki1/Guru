/* p-impun3.p */

INPUT FROM p-datfl4.d.

REPEAT:
   CREATE customer.
   IMPORT DELIMITER "," cust-num name sales-rep.
END.

INPUT CLOSE.
