/* p-br13.p */

DEFINE QUERY q1 FOR customer.
DEFINE BROWSE b1 QUERY q1 DISPLAY cust-num name city
    state postal-code phone WITH 10 DOWN WIDTH 70 SEPARATORS.
DEFINE BUTTON b-phone LABEL "Show Phone Nos.".          
DEFINE VARIABLE method-return AS LOGICAL.
          
DEFINE FRAME f1
   b1 SKIP(.5)
   b-phone
        WITH SIDE-LABELS ROW 2 CENTERED NO-BOX.

ON CHOOSE OF b-phone 
DO:
    method-return = b1:MOVE-COLUMN(6, 3).  /* Move the phone column up. */
    DISABLE b-phone WITH FRAME f1.  
END.

OPEN QUERY q1 FOR EACH customer.
ENABLE b1 b-phone WITH FRAME f1.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
