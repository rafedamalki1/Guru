/* p-perwn1.p */

DEFINE QUERY custq FOR customer.
DEFINE BROWSE custb QUERY custq
    DISPLAY name cust-num balance credit-limit phone sales-rep 
WITH 10 DOWN.

DEFINE BUTTON bExit LABEL "Exit".
DEFINE BUTTON bOpenorders LABEL "Open Orders".
DEFINE VARIABLE whand AS WIDGET-HANDLE.
DEFINE VARIABLE lcust-redundant AS LOGICAL.

DEFINE FRAME CustFrame 
    custb SKIP bExit bOpenorders
WITH SIZE-CHARS 96.5 by 11.
    
ON CHOOSE OF bExit IN FRAME CustFrame DO:
    RUN exit-proc.
END.        

ON CHOOSE OF bOpenorders IN FRAME CustFrame DO:
    IF custb:NUM-SELECTED-ROWS >= 1 THEN DO:
        RUN check-redundant(OUTPUT lcust-redundant).
        IF NOT lcust-redundant THEN DO:
            MESSAGE "Opening orders for" customer.name + ".".
            RUN p-perwn2.p PERSISTENT 
                (BUFFER customer, INPUT whand:TITLE, 
                 INPUT whand) NO-ERROR.
        END.
        ELSE DO:
            BELL.
            MESSAGE "Orders already open for" customer.name + ".".
        END.
    END.
    ELSE DO:
        BELL.
        MESSAGE "Select a customer to open orders ...".
    END.
END.  

CREATE WINDOW whand
    ASSIGN
        TITLE = "Customer Order Maintenance"
        SCROLL-BARS = FALSE
        RESIZE = FALSE
        HEIGHT-CHARS = FRAME CustFrame:HEIGHT-CHARS
        WIDTH-CHARS = FRAME CustFrame:WIDTH-CHARS.
CURRENT-WINDOW = whand.

OPEN QUERY custq PRESELECT EACH customer BY name.
PAUSE 0 BEFORE-HIDE.
ENABLE ALL WITH FRAME CustFrame.

WAIT-FOR CHOOSE OF bExit IN FRAME CustFrame.

PROCEDURE exit-proc:
    DEFINE VARIABLE phand AS HANDLE.
    DEFINE VARIABLE nhand AS HANDLE.

    MESSAGE "Exiting application ...".
    phand = SESSION:FIRST-PROCEDURE.
    DO WHILE VALID-HANDLE(phand):
        nhand = phand:NEXT-SIBLING.
        IF LOOKUP(whand:TITLE, phand:PRIVATE-DATA) > 0 THEN
            RUN destroy-query IN phand NO-ERROR.
        phand = nhand.
    END.
    DELETE WIDGET whand.
END PROCEDURE.

PROCEDURE check-redundant:
    DEFINE OUTPUT PARAMETER lcust-redundant AS LOGICAL INITIAL FALSE.
    
    DEFINE VARIABLE phand AS HANDLE.
    DEFINE VARIABLE nhand AS HANDLE.

    phand = SESSION:FIRST-PROCEDURE.
    DO WHILE VALID-HANDLE(phand):
        nhand = phand:NEXT-SIBLING.
        IF LOOKUP(whand:TITLE, phand:PRIVATE-DATA)   > 0 AND
           LOOKUP(customer.name, phand:PRIVATE-DATA) > 0 THEN DO:
                lcust-redundant = TRUE.
                RETURN.
        END.
        phand = nhand.
    END.
END PROCEDURE.

