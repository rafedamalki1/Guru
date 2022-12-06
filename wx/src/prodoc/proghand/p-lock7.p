/* p-lock7.p */

DEFINE BUTTON upd-cust LABEL "Update Customer". 
DEFINE BUTTON exit-app LABEL "Exit".

DEFINE VARIABLE changes-made AS LOGICAL.
DEFINE VARIABLE curr-cust AS ROWID.
DEFINE QUERY seq-cust FOR customer.
DEFINE BROWSE brow-cust QUERY seq-cust DISPLAY Cust-num Name WITH 5 DOWN.
   
FORM
   upd-cust exit-app SKIP(1)
   brow-cust
   WITH FRAME main-frame.

OPEN QUERY seq-cust FOR EACH customer.

ON VALUE-CHANGED OF brow-cust
   DO:
      curr-cust = ROWID(customer).
   END.
  
ON CHOOSE OF upd-cust
   DO: /* TRANSACTION */
      FIND customer WHERE ROWID(customer) = curr-cust EXCLUSIVE-LOCK.
      UPDATE customer WITH FRAME cust-frame VIEW-AS DIALOG-BOX
         TITLE "Customer Update".
      DISPLAY Cust-num Name WITH BROWSE brow-cust.
      RELEASE customer.
   END.

ENABLE ALL WITH FRAME main-frame.
PAUSE 0 BEFORE-HIDE.

WAIT-FOR CHOOSE OF exit-app OR WINDOW-CLOSE OF DEFAULT-WINDOW.
