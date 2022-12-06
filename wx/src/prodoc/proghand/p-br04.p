/* p-br04.p */

DEFINE VARIABLE credit-left AS DECIMAL LABEL "Credit Left".
DEFINE QUERY q1 FOR customer SCROLLING.
DEFINE BROWSE b1 QUERY q1 DISPLAY cust-num name credit-limit balance 
    (credit-limit - balance) @ credit-left
    ENABLE credit-limit WITH 10 DOWN SEPARATORS 
    TITLE "Update Credit Limits".
    
DEFINE FRAME f1
    b1
        WITH SIDE-LABELS ROW 2 CENTERED NO-BOX. 
        
ON LEAVE OF credit-limit IN BROWSE b1 DO:
    DISPLAY (INTEGER(customer.credit-limit:SCREEN-VALUE) - balance) @ credit-left 
        WITH BROWSE b1.
END.        
        
OPEN QUERY q1 FOR EACH customer NO-LOCK.
ENABLE ALL WITH FRAME f1.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.


