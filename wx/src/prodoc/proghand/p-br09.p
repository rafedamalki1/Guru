/* p-br09.p */

DEFINE QUERY q1 FOR item SCROLLING.
DEFINE BROWSE b1 QUERY q1 DISPLAY item-num item-name price
    ENABLE item-name price WITH 10 DOWN SEPARATORS 
    TITLE "Update Inventory". 
    
DEFINE VARIABLE method-status AS LOGICAL. 
DEFINE VARIABLE temp-rowid AS ROWID.
DEFINE BUTTON b-new LABEL "Add New Record" SIZE 45 BY 1.5.    
    
DEFINE FRAME f1
    b1 skip
    b-new
        WITH SIDE-LABELS ROW 2 CENTERED NO-BOX.
        
ON CHOOSE OF b-new DO:
    method-status = b1:INSERT-ROW("AFTER").
END.         
    
ON ROW-LEAVE OF BROWSE b1 DO:
    IF b1:NEW-ROW IN FRAME f1 THEN DO:
        DO ON ERROR UNDO, RETURN NO-APPLY:
            CREATE item.
            ASSIGN item-num = NEXT-VALUE(next-item-num)
                   INPUT BROWSE b1 item-name price.
            method-status = b1:CREATE-RESULT-LIST-ENTRY().
        END.  
    END.
END.

OPEN QUERY q1 FOR EACH item NO-LOCK.
ENABLE ALL WITH FRAME f1.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.