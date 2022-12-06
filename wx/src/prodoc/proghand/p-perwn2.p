/* r-perwn2.p */

DEFINE PARAMETER BUFFER custbuf FOR customer.
DEFINE INPUT PARAMETER appdata AS CHARACTER.
DEFINE INPUT PARAMETER wparent AS WIDGET-HANDLE.

DEFINE QUERY orderq FOR order.
DEFINE BROWSE orderb QUERY orderq
    DISPLAY 
        order.order-num order.order-date 
        order.ship-date order.promise-date order.carrier
    WITH 5 DOWN.
DEFINE BUTTON bClose LABEL "Close Orders".
DEFINE BUTTON bOpenlines LABEL "Open Order Lines".
DEFINE BUTTON bUpdate LABEL "Update Order".
DEFINE VARIABLE whand AS WIDGET-HANDLE.
DEFINE VARIABLE lorder-redundant AS LOGICAL.
DEFINE FRAME OrderFrame SKIP(.5)
    custbuf.name COLON 11 VIEW-AS TEXT SKIP
    custbuf.cust-num COLON 11 VIEW-AS TEXT SKIP(.5) 
    orderb SKIP
    bClose bOpenlines bUpdate
WITH SIDE-LABELS SIZE-CHARS 65.8 by 9.
    
ON CHOOSE OF bOpenlines IN FRAME OrderFrame DO:
    IF orderb:NUM-SELECTED-ROWS >= 1 THEN DO:
        RUN check-redundant(OUTPUT lorder-redundant).
        IF NOT lorder-redundant THEN DO:
            MESSAGE "Opening order lines for order" 
                    STRING(order.order-num) + ".".
            RUN p-perwn3.p PERSISTENT 
                (BUFFER order, INPUT THIS-PROCEDURE:PRIVATE-DATA,
                 INPUT whand) NO-ERROR.
        END.
        ELSE DO:
            BELL.
            MESSAGE "Order lines already open for order" 
                    STRING(order.order-num) + ".".
        END.
    END.
    ELSE DO:
        BELL.
        MESSAGE "Select an order to open order lines ...".
    END.
END.  

ON CHOOSE OF bUpdate IN FRAME OrderFrame DO:
    IF orderb:NUM-SELECTED-ROWS >= 1 THEN DO:
        RUN update-order.
    END.
    ELSE DO:
        BELL.
        MESSAGE "Select an order to update ...".
    END.
END.  

IF THIS-PROCEDURE:PERSISTENT THEN DO:
    THIS-PROCEDURE:PRIVATE-DATA = appdata + "," + custbuf.name.
    CREATE WIDGET-POOL.
END.

CREATE WINDOW whand
    ASSIGN
        TITLE = "Orders for Customer ..."
        PARENT = wparent
        RESIZE = FALSE
        SCROLL-BARS = FALSE
        HEIGHT-CHARS = FRAME OrderFrame:HEIGHT-CHARS
        WIDTH-CHARS = FRAME OrderFrame:WIDTH-CHARS.
THIS-PROCEDURE:CURRENT-WINDOW = whand.

OPEN QUERY orderq PRESELECT EACH order
    WHERE order.cust-num = custbuf.cust-num BY order-num.
DISPLAY custbuf.cust-num custbuf.name WITH FRAME OrderFrame.
ENABLE ALL WITH FRAME OrderFrame.

IF THIS-PROCEDURE:PERSISTENT THEN DO:
    ON CHOOSE OF bClose IN FRAME OrderFrame DO:
        RUN destroy-query.
    END.
END.
ELSE DO: 
    WAIT-FOR CHOOSE OF bClose IN FRAME OrderFrame.
END.

PROCEDURE destroy-query:
    DEFINE VARIABLE phand AS HANDLE.
    DEFINE VARIABLE nhand AS HANDLE.

    MESSAGE "Exiting orders for" custbuf.name "...".
    phand = SESSION:FIRST-PROCEDURE.
    DO WHILE VALID-HANDLE(phand):
        nhand = phand:NEXT-SIBLING.
        IF LOOKUP(custbuf.name, phand:PRIVATE-DATA) > 0 AND
           phand <> THIS-PROCEDURE THEN
                RUN destroy-query IN phand NO-ERROR.
        phand = nhand.
    END.
    DELETE PROCEDURE THIS-PROCEDURE NO-ERROR.
    DELETE WIDGET-POOL.
END.

PROCEDURE check-redundant:
    DEFINE OUTPUT PARAMETER lorder-redundant AS LOGICAL INITIAL FALSE.
    
    DEFINE VARIABLE phand AS HANDLE.
    DEFINE VARIABLE nhand AS HANDLE.

    phand = SESSION:FIRST-PROCEDURE.
    DO WHILE VALID-HANDLE(phand):
        nhand = phand:NEXT-SIBLING.
        IF LOOKUP(appdata, phand:PRIVATE-DATA) > 0 AND
           LOOKUP(STRING(order.order-num), 
                           phand:PRIVATE-DATA) > 0 THEN DO:
                lorder-redundant = TRUE.
                RETURN.
        END.
        phand = nhand.
    END.
END PROCEDURE.

PROCEDURE update-order:
    DEFINE VARIABLE rid AS ROWID.
    DEFINE VARIABLE choice AS LOGICAL.
    DEFINE BUTTON bSave LABEL "Save Changes".
    DEFINE BUTTON bCancel LABEL "Cancel".
    
    DEFINE FRAME UpdateFrame SKIP(.5)
        order.order-num COLON 12 VIEW-AS TEXT 
            order.sales-rep VIEW-AS TEXT "For..." VIEW-AS TEXT
            custbuf.name VIEW-AS TEXT SKIP
        custbuf.cust-num COLON 48 VIEW-AS TEXT SKIP(1) SPACE(1)
        order.order-date order.ship-date order.promise-date SKIP SPACE(1)
        order.carrier order.instructions SKIP SPACE(1)
        order.PO order.terms SKIP(1)
        bSave bCancel        
    WITH TITLE "Update Order" SIDE-LABELS VIEW-AS DIALOG-BOX.
    
    ON CHOOSE OF bSave IN FRAME UpdateFrame
       OR GO OF FRAME UpdateFrame DO:
        MESSAGE "Are you sure you want to save your changes?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE choice AS LOGICAL.
        CASE choice:
            WHEN TRUE THEN DO TRANSACTION ON ERROR UNDO, RETRY:
                rid = ROWID(order).
                FIND order WHERE ROWID(order) = rid EXCLUSIVE-LOCK.
                ASSIGN
                    order.order-date
                    order.ship-date
                    order.promise-date
                    order.carrier
                    order.instructions
                    order.PO
                    order.terms
                .
                DISPLAY 
                    order.order-num order.order-date 
                    order.ship-date order.promise-date order.carrier
                WITH BROWSE orderb.
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
        order.order-num order.sales-rep custbuf.name custbuf.cust-num
        order.order-date order.ship-date order.promise-date
        order.carrier order.instructions order.PO order.terms
    WITH FRAME UpdateFrame IN WINDOW ACTIVE-WINDOW.
    ENABLE
        order.order-date order.ship-date order.promise-date
        order.carrier order.instructions order.PO order.terms
        bSave bCancel
    WITH FRAME UpdateFrame IN WINDOW ACTIVE-WINDOW.
    
    WAIT-FOR WINDOW-CLOSE OF FRAME UpdateFrame.
END PROCEDURE.



     



