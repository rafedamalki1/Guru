/* p-diagbx.p */

DEFINE QUERY custq FOR customer.
DEFINE BROWSE custb QUERY custq DISPLAY cust-num name WITH 10 DOWN.
DEFINE BUTTON update-cust LABEL "Update Customer".
DEFINE BUTTON ok-button LABEL "OK" AUTO-GO SIZE 8 BY 1.
DEFINE BUTTON cancel-button LABEL "Cancel" AUTO-ENDKEY SIZE 8 BY 1.
DEFINE VARIABLE curr-rec AS ROWID.

FORM
  custb SKIP(1)
  update-cust AT 3 cancel-button AT 25
  SKIP(1)
  WITH FRAME main-frame CENTERED.
  
FORM
  country name address address2 city state postal-code contact
  phone sales-rep credit-limit balance terms discount comments SKIP(1)
  ok-button AT 15 cancel-button AT 50
  WITH FRAME upd-frame TITLE "Customer Update" VIEW-AS DIALOG-BOX.

FRAME upd-frame:HIDDEN = TRUE.
ENABLE ok-button cancel-button WITH FRAME upd-frame. 
 
ON CHOOSE OF update-cust OR MOUSE-SELECT-DBLCLICK OF custb
   DO: /* Transaction */
      curr-rec = ROWID(customer).
      FIND customer WHERE ROWID(customer) = curr-rec EXCLUSIVE-LOCK.
      UPDATE customer EXCEPT cust-num WITH FRAME upd-frame.
   END.
   
OPEN QUERY custq FOR EACH customer.

ENABLE ALL WITH FRAME main-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
