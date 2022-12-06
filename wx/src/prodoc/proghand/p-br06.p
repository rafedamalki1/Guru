/* p-br06.p */

DEFINE QUERY q1 FOR customer SCROLLING. 
DEFINE VARIABLE credit-left AS DECIMAL LABEL "Credit Left".
DEFINE BROWSE b1 QUERY q1 DISPLAY cust-num name credit-limit balance 
    (credit-limit - balance) @ credit-left ENABLE name credit-limit 
    WITH 10 DOWN SEPARATORS TITLE "Update Credit Limits".
DEFINE VARIABLE method-return AS LOGICAL.
DEFINE VARIABLE wh AS WIDGET-HANDLE.
DEFINE VARIABLE olabel AS CHARACTER.
DEFINE FRAME f1
    b1
        WITH SIDE-LABELS ROW 2 CENTERED NO-BOX. 
ON END-SEARCH OF b1 IN FRAME f1
DO:
    wh:LABEL = olabel.
END.
ON START-SEARCH OF b1 IN FRAME f1
DO: 
    wh = b1:CURRENT-COLUMN.     
    IF wh:LABEL = "name" THEN DO: 
        olabel = wh:label.
        wh:label = "SEARCHING...".     
        MESSAGE "Incremental Search?" VIEW-AS ALERT-BOX QUESTION 
            BUTTONS YES-NO-CANCEL TITLE "Search Mode" 
            UPDATE answ AS LOGICAL.
        CASE answ:
            WHEN TRUE THEN DO:               
                RUN inc-src.
                RETURN. 
            END.
            WHEN FALSE THEN DO:
                MESSAGE "Search mode defaults to the first character."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                RETURN NO-APPLY.
            END.
            OTHERWISE APPLY "END-SEARCH" TO SELF.
        END CASE.   
    END.
    ELSE 
    DO:
      olabel = wh:LABEL.
      wh:LABEL = "SEARCHING...".
    END.
END.
OPEN QUERY q1 FOR EACH customer NO-LOCK by customer.name. 
ENABLE ALL WITH FRAME f1.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.

PROCEDURE inc-src: 

DEFINE VARIABLE curr-record AS RECID.
DEFINE VARIABLE initial-str AS CHARACTER.

APPLY "START-SEARCH" TO BROWSE b1.

ON ANY-PRINTABLE OF b1 IN FRAME f1
DO:    
    ASSIGN curr-record = RECID(customer)
           initial-str = initial-str + LAST-EVENT:LABEL.
       
    match-string:
    DO ON ERROR UNDO match-string, LEAVE match-string: 
        IF RETRY THEN 
            initial-str = LAST-EVENT:LABEL.               
            message "initial-str: "initial-str.
       
        FIND NEXT customer WHERE customer.name BEGINS initial-str 
            USE-INDEX name NO-LOCK NO-ERROR.
                 
        IF AVAILABLE customer THEN 
            curr-record = RECID(customer).
        ELSE IF RETRY THEN 
             DO:
                initial-str = "".
                BELL.
             END.
             ELSE 
                UNDO match-string, RETRY match-string.         
    END.
 
    REPOSITION q1 TO RECID curr-record.
END. /* of anyprintable */

    message "hello".
    ON VALUE-CHANGED OF b1 IN FRAME f1
    DO: 
        message "vchg".
        initial-str = "".
    END.
     
    WAIT-FOR "END-SEARCH" OF BROWSE b1.
    APPLY "ENTRY" TO name IN BROWSE b1.
