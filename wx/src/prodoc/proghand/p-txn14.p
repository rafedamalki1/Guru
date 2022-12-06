/* p-txn14.p */

DEFINE BUTTON exit-app LABEL "Exit".
DEFINE BUTTON com-cust LABEL "Save Changes".
DEFINE BUTTON rev-cust LABEL "Revert to Saved".

DEFINE VARIABLE curr-cust AS ROWID.
DEFINE VARIABLE exit-chosen AS LOGICAL.
DEFINE VARIABLE rev-chosen AS LOGICAL.
DEFINE VARIABLE temp-hand AS WIDGET-HANDLE.

DEFINE BUFFER this-cust FOR Customer.
DEFINE QUERY seq-cust FOR this-cust SCROLLING.
DEFINE BROWSE brow-cust QUERY seq-cust DISPLAY Cust-num Name WITH 4 DOWN.
   
FORM
   exit-app SKIP
   brow-cust
   WITH FRAME main-frame.

FORM
   Customer.Cust-num Customer.Name Customer.Address Customer.Address2 
   Customer.City Customer.State Customer.Postal-Code Customer.Country
   Customer.Phone Customer.Contact Customer.Sales-rep 
   Customer.Credit-Limit Customer.Balance Customer.Terms
   Customer.Discount Customer.Comments
   SKIP
   com-cust AT 15 rev-cust AT 45
   WITH FRAME curr-frame SIDE-LABELS.

OPEN QUERY seq-cust FOR EACH this-cust. 
FIND FIRST Customer NO-LOCK.

ON ITERATION-CHANGED OF brow-cust
   DO:
      IF AVAILABLE(Customer)
      THEN DO:
         /* Determine whether any updates were made to previous record. */
         temp-hand = FRAME curr-frame:CURRENT-ITERATION.
         temp-hand = temp-hand:FIRST-CHILD.
         
         search-widgets: 
         DO WHILE temp-hand <> ?: 
            IF CAN-QUERY(temp-hand, "MODIFIED")
            THEN IF temp-hand:MODIFIED
                 THEN LEAVE search-widgets.
            temp-hand = temp-hand:NEXT-SIBLING.
         END.
         
         /* If a modification was made, assign the record. */
         IF temp-hand <> ?
         THEN DO:
            DO WITH FRAME curr-frame TRANSACTION: 
               ASSIGN Customer.
            END.
            MESSAGE "Customer record updated.".
         END.
      END.
      
      /* Set curr-cust to the ROWID of the current query
         record. Find that record with NO-LOCK.		  */ 
      curr-cust = ROWID(this-cust).   
      FIND Customer WHERE ROWID(Customer) = curr-cust NO-LOCK.
      
      /* Display the current record. We have to disable the update
         fields and them re-enable them after the DISPLAY. Otherwise,
         the DISPLAY sets all the MODIFIED attributes to TRUE.*/
      DISABLE ALL WITH FRAME curr-frame.
      DISPLAY Customer WITH FRAME curr-frame. 
      ENABLE ALL WITH FRAME curr-frame. 
   END.

ON CHOOSE OF exit-app /* Exit application. */
   DO:
      /* Set a flag so we'll know why we exited the WAIT-FOR. */
      exit-chosen = TRUE.
   END.
   
ON CHOOSE OF com-cust /* Commit changes to Customer record. */
   DO:
      /* Commit any changes made to the Customer record. */
      DO WITH FRAME curr-frame TRANSACTION:
         ASSIGN Customer.
         RELEASE Customer.
      END.
      
      MESSAGE "Customer record updated.".
      
      /* Release the exclusive lock. */ 
      CLOSE QUERY seq-cust.
      OPEN QUERY seq-cust FOR EACH this-cust.
      REPOSITION seq-cust TO ROWID curr-cust.
              
      /* Restore the MODIFIED attribute to FALSE. */
      DISABLE ALL WITH FRAME curr-frame.
      ENABLE ALL WITH FRAME curr-frame. 
   END.

ON CHOOSE OF rev-cust /* Undo pending changes to Customer record. */
   DO:   
      rev-chosen = TRUE.
       
      /* Undo any changes to the Customer screen buffer
         and reset the MODIFIED attributes to FALSE.	*/
      DISABLE ALL WITH FRAME curr-frame.
      IF AVAILABLE(Customer)
      THEN DISPLAY Customer WITH FRAME curr-frame.
      ENABLE ALL WITH FRAME curr-frame.     
  END.

ON ENTRY OF FRAME curr-frame
   DO:
      /* Upgrade from NO-LOCK to EXCLUSIVE-LOCK. */     
      FIND Customer WHERE ROWID(Customer) = curr-cust EXCLUSIVE-LOCK
                 NO-WAIT NO-ERROR.
                 
      /* If we didn't get the exclusive lock, then give a
         message and return focus to previous frame.  		*/         
      IF LOCKED(Customer)
      THEN DO:
          MESSAGE "Record is currently locked by another user."
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      END.
      
      IF NOT AVAILABLE(Customer)
      THEN DO:
          APPLY "CHOOSE" TO rev-cust IN FRAME curr-frame.
          RETURN NO-APPLY.
      END. 

      /* Pick up any updates that may have occured. */
      DISPLAY Customer WITH FRAME curr-frame. 
   END.
   
ENABLE ALL WITH FRAME main-frame.
ENABLE ALL WITH FRAME curr-frame.
APPLY "ITERATION-CHANGED" TO brow-cust.
PAUSE 0 BEFORE-HIDE.

main-loop:
DO WHILE TRUE ON ENDKEY UNDO, RETURN ON ERROR UNDO, RETRY:

   ASSIGN rev-chosen = FALSE
          exit-chosen = FALSE.
          
   WAIT-FOR CHOOSE OF exit-app OR WINDOW-CLOSE OF DEFAULT-WINDOW OR
         CHOOSE OF com-cust, rev-cust FOCUS brow-cust IN FRAME main-frame.

   IF AVAILABLE(Customer)
   THEN RELEASE Customer. 
   
   IF exit-chosen
   THEN LEAVE main-loop.
   
   IF rev-chosen
   THEN UNDO main-loop, RETRY main-loop.     
END.
