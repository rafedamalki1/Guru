/* r-altbox.p */

DEFINE VARIABLE cust-list AS CHARACTER VIEW-AS SELECTION-LIST
                           SINGLE SIZE 50 BY 10 LABEL "Customers".
DEFINE VARIABLE ok-status AS LOGICAL.                           

FORM
  cust-list
  WITH FRAME sel-frame.

ON DEFAULT-ACTION OF cust-list
   DO:
      MESSAGE "You have chosen to delete" cust-list:SCREEN-VALUE + "."
              SKIP(1)
              "Do you really want to delete this customer?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
                    TITLE "" UPDATE choice AS LOGICAL.
      CASE choice:
         WHEN TRUE THEN /* Yes */
         DO:
            FIND customer WHERE name = cust-list:SCREEN-VALUE
                 EXCLUSIVE-LOCK.
            DELETE customer.
         END.
         WHEN FALSE THEN /* No */
         DO:
             MESSAGE "Deletion canceled."
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
             RETURN NO-APPLY.
         END.
         OTHERWISE /* Cancel */
             STOP.
         END CASE.
   END.
   
FOR EACH customer BY name:
   ok-status = cust-list:ADD-LAST(customer.name).
END. 

ENABLE cust-list WITH FRAME sel-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.                       


