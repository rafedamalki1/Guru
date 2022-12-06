/* p-br05.p */

DEFINE VARIABLE credit-left AS DECIMAL LABEL "Credit Left".
DEFINE QUERY q1 FOR customer SCROLLING.
DEFINE BROWSE b1 QUERY q1 DISPLAY cust-num WIDTH 10 name WIDTH 23 
    credit-limit WIDTH 15 balance WIDTH 15  
    (credit-limit - balance) @ credit-left WIDTH 15  
    ENABLE credit-limit WITH 10 DOWN WIDTH 70 SEPARATORS 
    TITLE "Update Credit Limits".
    
DEFINE FRAME f1
    b1
        WITH SIDE-LABELS ROW 2 CENTERED NO-BOX.
        
OPEN QUERY q1 FOR EACH customer NO-LOCK.
ENABLE ALL WITH FRAME f1.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.

