/* p-txn13.p */

DEFINE BUTTON upd-cust LABEL "Update Customer".
DEFINE BUTTON del-cust LABEL "Delete Customer".
DEFINE BUTTON exit-app LABEL "Exit".
DEFINE BUTTON tran-undo LABEL "Undo Transaction".
DEFINE BUTTON tran-com LABEL "Commit Transaction".

DEFINE VARIABLE changes-made AS LOGICAL.
DEFINE VARIABLE curr-cust AS ROWID.
DEFINE VARIABLE exit-chosen AS LOGICAL.
DEFINE VARIABLE undo-chosen AS LOGICAL.

DEFINE QUERY seq-cust FOR Customer.
DEFINE BROWSE brow-cust QUERY seq-cust DISPLAY Cust-num Name WITH 10 DOWN.
   
FORM 
   upd-cust del-cust exit-app SKIP(1)
   brow-cust SKIP(1)
   tran-com tran-undo
   WITH FRAME main-frame.

OPEN QUERY seq-cust FOR EACH Customer.

ON VALUE-CHANGED OF brow-cust
   DO:
      curr-cust = ROWID(Customer).
   END.
   
ON CHOOSE OF upd-cust
   DO:
      FIND Customer WHERE ROWID(Customer) = curr-cust EXCLUSIVE-LOCK.
      UPDATE Customer WITH FRAME cust-frame VIEW-AS DIALOG-BOX
         TITLE "Customer Update".
      changes-made = brow-cust:REFRESH().
   END.
   
ON CHOOSE OF del-cust
   DO: 
       MESSAGE "Delete" Customer.name + "?" VIEW-AS ALERT-BOX
           QUESTION BUTTONS OK-CANCEL UPDATE kill-it AS LOGICAL.
           
       IF kill-it
       THEN DO:
          FIND Customer WHERE ROWID(Customer) = curr-cust EXCLUSIVE-LOCK.
          DELETE Customer.
          changes-made = brow-cust:REFRESH().
       END.
   END.   

ON CHOOSE OF tran-undo
  DO:
     undo-chosen = TRUE. 
  END.

ON CHOOSE OF exit-app
   DO:
      DEFINE BUTTON exit-commit LABEL "Commit" AUTO-GO.
      DEFINE BUTTON exit-undo 	LABEL "Undo" AUTO-GO.
      DEFINE BUTTON exit-cancel LABEL "Cancel" AUTO-ENDKEY.
      
      FORM
         "Do you want to commit or undo your changes?" SKIP
         exit-commit exit-undo exit-cancel
         WITH FRAME exit-frame VIEW-AS DIALOG-BOX TITLE "Exit".
      
      ON CHOOSE OF exit-undo
         DO:
            undo-chosen = TRUE.
         END.
         
      exit-chosen = TRUE.
      
      /* If changes have been made during the current transaction,
         then ask the user to either commit or undo them (or cancel
         the Exit operation).					    */
      IF changes-made
      THEN UPDATE exit-commit exit-undo exit-cancel WITH FRAME exit-frame.
   END.
  
ENABLE ALL WITH FRAME main-frame.
PAUSE 0 BEFORE-HIDE.

exit-chosen = FALSE. 

trans-loop:
DO WHILE NOT exit-chosen TRANSACTION ON ENDKEY UNDO, LEAVE
                    ON ERROR UNDO, LEAVE:
                    
   changes-made = brow-cust:REFRESH().
                          
   ASSIGN changes-made = FALSE
          undo-chosen = FALSE.
   
   WAIT-FOR CHOOSE OF tran-com, tran-undo, exit-app.

   /* If the user chose UNDO (either from the main frame
      or from the Exit dialog), then undo the current
      transaction and either start a new one or exit.    */
   IF undo-chosen 
   THEN DO:
      IF exit-chosen
      THEN UNDO trans-loop, LEAVE trans-loop.
      ELSE UNDO trans-loop, RETRY trans-loop.      
   END.
   
   /* Make sure we don't hold any locks after committing a transaction. */
   IF AVAILABLE(Customer)
   THEN RELEASE Customer.
   
END.
