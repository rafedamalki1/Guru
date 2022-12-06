/* r-rels.p */

DEFINE BUTTON upd-cust LABEL "Update Customer".  
DEFINE BUTTON exit-app LABEL "Exit".

DEFINE VARIABLE methRtn AS LOGICAL.
DEFINE VARIABLE curr-cust AS ROWID.
DEFINE QUERY seq-cust FOR customer.
DEFINE BROWSE brow-cust QUERY seq-cust DISPLAY Cust-num Name WITH 10 DOWN.
   
FORM
   upd-cust exit-app SKIP(1)
   brow-cust
   WITH FRAME main-frame.

FORM
   customer EXCEPT comments
   WITH FRAME curr-frame COLUMN 40.

OPEN QUERY seq-cust FOR EACH customer.

ON VALUE-CHANGED OF brow-cust
   DO:
      DISPLAY customer EXCEPT comments WITH FRAME curr-frame SIDE-LABELS.
      curr-cust = ROWID(customer).
   END.
  
ON CHOOSE OF upd-cust
   DO: /* TRANSACTION */
      FIND customer WHERE ROWID(customer) = curr-cust EXCLUSIVE-LOCK.
      UPDATE customer WITH FRAME cust-frame VIEW-AS DIALOG-BOX
         TITLE "Customer Update".
      methRtn = brow-cust:REFRESH().
      DISPLAY customer EXCEPT comments WITH FRAME curr-frame SIDE-LABELS.
      RELEASE customer.
   END.
 
ENABLE ALL WITH FRAME main-frame.
APPLY "VALUE-CHANGED" TO brow-cust.
PAUSE 0 BEFORE-HIDE.

WAIT-FOR CHOOSE OF exit-app OR WINDOW-CLOSE OF DEFAULT-WINDOW.
