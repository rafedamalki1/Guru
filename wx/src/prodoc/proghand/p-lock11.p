/* p-lock11.p */

DEFINE BUTTON upd-button LABEL "Update".
DEFINE VARIABLE current-cust AS RECID.

FORM
  customer.cust-num name SKIP
  upd-button 
  WITH FRAME main-frame.

ON CHOOSE OF upd-button
   DO:
      RUN p-lock10.p (INPUT current-cust). 
   END.
 
DO FOR customer:
   FIND FIRST customer NO-LOCK.
   current-cust = RECID(customer).
   DISPLAY cust-num name WITH FRAME main-frame.
END.

ENABLE upd-button next-button WITH FRAME main-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
