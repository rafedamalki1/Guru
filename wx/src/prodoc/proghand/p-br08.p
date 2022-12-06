/* p-br08.p */

DEFINE QUERY q1 FOR customer SCROLLING.
DEFINE BROWSE b1 QUERY q1 DISPLAY cust-num name credit-limit 
    ENABLE credit-limit WITH 10 DOWN SEPARATORS NO-ASSIGN
    TITLE "Update Credit Limits".
    
DEFINE FRAME f1
    b1
        WITH SIDE-LABELS ROW 2 CENTERED NO-BOX.
        
ON ROW-LEAVE OF BROWSE b1 DO:
    IF INTEGER(customer.credit-limit:SCREEN-VALUE IN BROWSE b1) > 100000 
    THEN DO:
        MESSAGE "Credit limits over $100,000 need a manager's approval." 
            VIEW-AS ALERT-BOX ERROR BUTTONS OK TITLE "Invalid Credit Limit".
       DISPLAY credit-limit WITH BROWSE b1.
       RETURN NO-APPLY.
    END.
    
    DO TRANSACTION:
        GET CURRENT q1 EXCLUSIVE-LOCK NO-WAIT.
        IF CURRENT-CHANGED(customer) THEN DO:
            MESSAGE "The record changed while you were working."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK TITLE "New Data".  
            DISPLAY credit-limit WITH BROWSE b1.
            RETURN NO-APPLY.
        END.  
        ELSE ASSIGN INPUT BROWSE b1 credit-limit. 
    END. /* TRANSACTION */
    GET CURRENT q1 NO-LOCK.  
END.

OPEN QUERY q1 FOR EACH customer NO-LOCK.
ENABLE ALL WITH FRAME f1.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.