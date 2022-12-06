/* r-brownr.p */

DEFINE VARIABLE curr-rec AS RECID.
DEFINE VARIABLE status-ok AS LOGICAL.
DEFINE VARIABLE threshold LIKE customer.credit-limit INITIAL 25000.

DEFINE BUTTON no-orders-custs LABEL "No Orders".
DEFINE BUTTON hi-cred-custs LABEL "High Credit".

DEFINE QUERY qry FOR customer.
DEFINE BROWSE brws QUERY qry DISPLAY cust-num name country credit-limit
   WITH 10 DOWN MULTIPLE.

FORM
   brws SKIP(1)
   no-orders-custs hi-cred-custs
   WITH FRAME brws-frame.

FORM
   threshold
   WITH FRAME thresh-frame VIEW-AS DIALOG-BOX
        TITLE "Set Threshold" SIDE-LABELS.

ON CHOOSE OF no-orders-custs DO:
   /* Select those customers with no orders. */
   
   status-ok = brws:DESELECT-ROWS().
   HIDE MESSAGE.
 
   FOR EACH customer NO-LOCK WHERE NOT CAN-FIND(FIRST order OF customer):
   
      /* Position query to this record and then select row in browse. */
      curr-rec = RECID(customer).
      REPOSITION qry TO RECID curr-rec.
      status-ok = brws:SELECT-FOCUSED-ROW().
      IF NOT status-ok
      THEN MESSAGE "Could not select row.".
   END. 

   /* Report number of selected rows. */

   MESSAGE brws:NUM-SELECTED-ROWS "of" NUM-RESULTS("qry")
           "rows have been selected.".
           
   /* Position to first selected row. */
   IF brws:NUM-SELECTED-ROWS > 0
   THEN status-ok = brws:SCROLL-TO-SELECTED-ROW(1).
END.

ON CHOOSE OF hi-cred-custs DO:
   /* Select customers with high credit limits. */
   
   status-ok = brws:DESELECT-ROWS().
   HIDE MESSAGE.
   
   /* Get credit-limit threshold value. */
   UPDATE threshold WITH FRAME thresh-frame.
   
   FOR EACH customer NO-LOCK WHERE customer.credit-limit >= threshold:
   
      /* Position query to this record and then select row in browse. */
      curr-rec = RECID(customer).
      REPOSITION qry TO RECID curr-rec.
      status-ok = brws:SELECT-FOCUSED-ROW().
      IF NOT status-ok
      THEN MESSAGE "Could not select row.".
   END.

   /* Report number of selected rows. */
   MESSAGE brws:NUM-SELECTED-ROWS "of" NUM-RESULTS("qry")
           "rows have been selected.".
           
   /* Position to the first selected row. */
   IF brws:NUM-SELECTED-ROWS > 0
   THEN status-ok = brws:SCROLL-TO-SELECTED-ROW(1).
END.

OPEN QUERY qry PRESELECT EACH customer.

ENABLE ALL WITH FRAME brws-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
