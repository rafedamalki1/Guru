/* p-exprt2.p */

OUTPUT TO p-datfl7.d.

FOR EACH customer:
   EXPORT DELIMITER "," cust-num name sales-rep.
END.

OUTPUT CLOSE.
