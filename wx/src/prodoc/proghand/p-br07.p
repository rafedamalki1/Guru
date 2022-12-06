/* p-br07.p */

DEFINE VARIABLE credit-left AS DECIMAL LABEL "Credit Left".
DEFINE QUERY q1 FOR customer SCROLLING.
DEFINE BROWSE b1 QUERY q1 DISPLAY cust-num name credit-limit balance 
    (credit-limit - balance) @ credit-left
    ENABLE credit-limit WITH 10 DOWN SEPARATORS 
    TITLE "Update Credit Limits".
    
DEFINE BUTTON b-ok LABEL "OK" SIZE 20 BY 1.
    
DEFINE FRAME f1
    b1 skip(.5)
    customer.comments VIEW-AS EDITOR INNER-LINES 3 
        INNER-CHARS 62
        WITH SIDE-LABELS ROW 2 CENTERED NO-BOX.
        
DEFINE FRAME f2
    customer.comments NO-LABEL VIEW-AS EDITOR INNER-LINES 3 
        INNER-CHARS 62 SKIP(.5)
    b-ok to 42 SKIP(.5)
        WITH SIDE-LABELS ROW 2 CENTERED
        VIEW-AS DIALOG-BOX TITLE "Comments". 
        
ON VALUE-CHANGED OF b1
DO:
    DISPLAY customer.comments WITH FRAME f1.
END.

ON DEFAULT-ACTION OF b1
DO:
    ASSIGN customer.comments:READ-ONLY IN FRAME f2 = TRUE.       
    DISPLAY customer.comments WITH FRAME f2.
    ENABLE ALL WITH frame f2.
    WAIT-FOR CHOOSE OF b-ok.
    HIDE FRAME f2.
END.
              
ASSIGN customer.comments:READ-ONLY IN FRAME f1 = TRUE.       
OPEN QUERY q1 FOR EACH customer NO-LOCK.
ENABLE ALL WITH FRAME f1.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.