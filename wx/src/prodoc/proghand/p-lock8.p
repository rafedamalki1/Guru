/* p-lock8.p */

DEFINE BUTTON upd-button LABEL "Update". 
DEFINE VARIABLE current-cust AS RECID.

FORM
  customer.cust-num name SKIP
  upd-button 
  WITH FRAME main-frame.

ON CHOOSE OF upd-button
   DO:
      DO TRANSACTION:
        FIND customer WHERE RECID(customer) = current-cust EXCLUSIVE-LOCK.
        UPDATE customer.name WITH FRAME main-frame.
        RELEASE customer.
      END.  /* TRANSACTION */
    END.
 
FIND FIRST customer NO-LOCK.

current-cust = RECID(customer).

DISPLAY cust-num name WITH FRAME main-frame.
ENABLE upd-button WITH FRAME main-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
