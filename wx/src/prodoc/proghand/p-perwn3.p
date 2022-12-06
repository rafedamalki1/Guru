/* r-perwn3.p */

DEFINE PARAMETER BUFFER orderbuf FOR order.
DEFINE INPUT PARAMETER appdata AS CHARACTER.
DEFINE INPUT PARAMETER wparent AS WIDGET-HANDLE.

DEFINE QUERY ordlineq FOR order-line, item.
DEFINE BROWSE ordlineb QUERY ordlineq
    DISPLAY 
        order-line.line-num order-line.item-num item.item-name 
        order-line.price order-line.qty order-line.extended-price 
        order-line.discount order-line.backorder
    WITH 5 DOWN.
DEFINE BUTTON bClose LABEL "Close Order Lines".
DEFINE BUTTON bUpdate LABEL "Update Order Line".
DEFINE VARIABLE whand AS WIDGET-HANDLE.
DEFINE FRAME OrdlineFrame SKIP(.5)
    orderbuf.order-num COLON 12 VIEW-AS TEXT SKIP(.5)
    ordlineb SKIP
    bClose bUpdate
WITH SIDE-LABELS SIZE-CHARS 98.8 by 8.5.
    
ON CHOOSE OF bUpdate IN FRAME OrdlineFrame DO:
    IF ordlineb:NUM-SELECTED-ROWS >= 1 THEN DO:
        RUN update-order-line.
    END.
    ELSE DO:
        BELL.
        MESSAGE "Select an order line to update ...".
    END.
END.  

IF THIS-PROCEDURE:PERSISTENT THEN DO:
    THIS-PROCEDURE:PRIVATE-DATA = appdata + "," 
                                  + STRING(orderbuf.order-num).
    CREATE WIDGET-POOL.
END.

CREATE WINDOW whand
    ASSIGN
        TITLE = "Order Lines for Order ..."
        PARENT = wparent
        RESIZE = FALSE
        SCROLL-BARS = FALSE
        HEIGHT-CHARS = FRAME OrdlineFrame:HEIGHT-CHARS
        WIDTH-CHARS = FRAME OrdlineFrame:WIDTH-CHARS.
THIS-PROCEDURE:CURRENT-WINDOW = whand.

OPEN QUERY ordlineq PRESELECT EACH order-line 
    WHERE order-line.order-num = orderbuf.order-num, EACH item
    WHERE item.item-num = order-line.item-num
    BY order-line.line-num.
DISPLAY orderbuf.order-num WITH FRAME OrdlineFrame.
ENABLE ALL WITH FRAME OrdlineFrame.

IF THIS-PROCEDURE:PERSISTENT THEN DO:
    ON CHOOSE OF bClose IN FRAME OrdlineFrame DO:
        RUN destroy-query.
    END.
END.
ELSE DO: 
    WAIT-FOR CHOOSE OF bClose IN FRAME OrdlineFrame.
END.

PROCEDURE destroy-query:
    MESSAGE "Exiting order lines for order" 
            STRING(orderbuf.order-num) "...".
    DELETE PROCEDURE THIS-PROCEDURE NO-ERROR.
    DELETE WIDGET-POOL.
END.

PROCEDURE update-order-line:
    DEFINE VARIABLE rid AS ROWID.
    DEFINE VARIABLE choice AS LOGICAL.
    DEFINE BUTTON bSave LABEL "Save Changes".
    DEFINE BUTTON bCancel LABEL "Cancel".
    
    DEFINE FRAME UpdateFrame SKIP(.5)
        orderbuf.order-num COLON 12 VIEW-AS TEXT 
            order-line.line-num VIEW-AS TEXT SKIP(1) SPACE(1)
        order-line.qty order-line.discount order-line.backorder SKIP(1)
        bSave bCancel        
    WITH TITLE "Update Order Line" SIDE-LABELS VIEW-AS DIALOG-BOX.
    
    ON CHOOSE OF bSave IN FRAME UpdateFrame 
       OR GO OF FRAME UpdateFrame DO:
        MESSAGE "Are you sure you want to save your changes?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE choice AS LOGICAL.
        CASE choice:
            WHEN TRUE THEN DO TRANSACTION ON ERROR UNDO, RETRY:
                rid = ROWID(order-line).
                FIND order-line WHERE ROWID(order-line) 
                    = rid EXCLUSIVE-LOCK.
                ASSIGN
                    order-line.qty
                    order-line.discount
                    order-line.backorder
                    order-line.extended-price = INPUT order-line.qty 
                        * order-line.price
                        * (1 - (INPUT order-line.discount * 0.01))
                .
                DISPLAY 
                    order-line.qty order-line.discount 
                    order-line.backorder order-line.extended-price
                WITH BROWSE ordlineb.
                APPLY "WINDOW-CLOSE" TO FRAME UpdateFrame.
            END.
            WHEN FALSE THEN DO:
               MESSAGE "Changes not saved."
                   VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
               RETURN NO-APPLY.
            END.
        END CASE.
    END.  
    
    ON CHOOSE OF bCancel DO:
        MESSAGE "Are you sure you want to cancel your current updates?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE choice AS LOGICAL.
        CASE choice:
            WHEN TRUE THEN DO:
               MESSAGE "Current updates cancelled."
                   VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
               APPLY "WINDOW-CLOSE" TO FRAME UpdateFrame.
            END.
            WHEN FALSE THEN DO:
               MESSAGE "Current update is continuing ..."
                   VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            END.
        END CASE.
    END.    
    
    DISPLAY 
        orderbuf.order-num order-line.line-num order-line.qty 
        order-line.discount order-line.backorder
    WITH FRAME UpdateFrame IN WINDOW ACTIVE-WINDOW.
    ENABLE
        order-line.qty order-line.discount order-line.backorder
        bSave bCancel
    WITH FRAME UpdateFrame IN WINDOW ACTIVE-WINDOW.
    
    WAIT-FOR WINDOW-CLOSE OF FRAME UpdateFrame.
END PROCEDURE.


     



