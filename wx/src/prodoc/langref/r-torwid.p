/* r-torwid.p */

DEFINE QUERY custq FOR customer 
    FIELDS (cust-num name balance credit-limit).
DEFINE BUFFER cust2 FOR customer.

DEFINE TEMP-TABLE rowtab FIELD rowchar AS CHARACTER
    INDEX rowi IS UNIQUE PRIMARY rowchar ASCENDING.

DEFINE BROWSE custb QUERY custq
    DISPLAY cust-num name balance credit-limit
WITH 10 DOWN MULTIPLE.
DEFINE VARIABLE hcustb AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE irow AS INTEGER NO-UNDO.
DEFINE BUTTON bstore LABEL "Store Selections".
DEFINE BUTTON bdisplay LABEL "Display Call Selections".
DEFINE BUTTON bclear LABEL "Clear Storage".

DEFINE FRAME brs-frame custb SKIP bstore bdisplay bclear.
DEFINE FRAME dsp-frame
    cust2.cust-num cust2.name cust2.phone
WITH 5 DOWN SCROLL 1.

ON CHOOSE OF bstore DO:
    DO irow = 1 TO custb:NUM-SELECTED-ROWS:
        IF custb:FETCH-SELECTED-ROW(irow) AND 
           NOT CAN-FIND(rowtab WHERE STRING(ROWID(customer)) = rowchar)
        THEN DO:
            CREATE rowtab NO-ERROR.
            ASSIGN rowchar = STRING(ROWID(customer)) NO-ERROR.
        END.
    END.
END.

ON CHOOSE OF bdisplay DO:
    CLEAR FRAME dsp-frame ALL.
    FOR EACH rowtab WITH FRAME dsp-frame:
        FIND cust2 WHERE ROWID(cust2) = TO-ROWID(rowchar).
        DISPLAY cust2.cust-num cust2.name cust2.phone.
        DOWN WITH FRAME dsp-frame.
    END.
END.

ON CHOOSE OF bclear DO:
    IF custb:DESELECT-ROWS() THEN FOR EACH rowtab:
        DELETE rowtab.
    END.
    FRAME dsp-frame:VISIBLE = FALSE.
END.

OPEN QUERY custq PRESELECT EACH customer.
ENABLE ALL WITH FRAME brs-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
