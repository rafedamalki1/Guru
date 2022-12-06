/* p-schcs1.p */

DEFINE VARIABLE filen AS CHARACTER FORMAT "x(8)" 
    LABEL "Schema Cache Name".
DEFINE VARIABLE icnt AS INTEGER.
    
DEFINE VARIABLE db-table AS CHARACTER LABEL "Select Tables"
       VIEW-AS SELECTION-LIST
       MULTIPLE NO-DRAG SIZE 32 BY 7
       LIST-ITEMS "customer", "invoice", "item", "local-default",
                  "order", "order-line", "ref-call", "salesrep",
                  "state".
            
DEFINE BUTTON bsave LABEL "Save to File".
DEFINE BUTTON bcancel LABEL "Cancel".

DEFINE FRAME SchemaFrame 
    SPACE(1) 
    db-table 
    VALIDATE(db-table <> "" AND db-table <> ?, "You must select a table.")
    filen 
    VALIDATE(filen <> "" AND filen <> ?, "You must enter filename.")
    SKIP(1) 
    SPACE(20) bsave bcancel
WITH TITLE "Save Schema Cache File" SIDE-LABELS SIZE 80 by 11.
     
ON CHOOSE OF bcancel IN FRAME SchemaFrame QUIT.

ON CHOOSE OF bsave IN FRAME SchemaFrame DO:
    ASSIGN filen db-table.
    IF NOT filen:VALIDATE() THEN RETURN NO-APPLY.
    IF NOT db-table:VALIDATE() THEN RETURN NO-APPLY.
    DO WHILE NOT CONNECTED("sports"):
        BELL.
        PAUSE MESSAGE "When ready to connect the sports database, press <RETURN>".
        CONNECT sports -1 NO-ERROR.
        IF NOT CONNECTED("sports") THEN
            DO icnt = 1 to ERROR-STATUS:NUM-MESSAGES:
                MESSAGE ERROR-STATUS:GET-MESSAGE(icnt).
            END.
        ELSE
            MESSAGE "Sports database connected.".
    END.
    RUN p-schcs2.p (INPUT db-table, INPUT filen).
    DISCONNECT sports NO-ERROR.
END.

ENABLE ALL WITH FRAME SchemaFrame.
WAIT-FOR CHOOSE OF bcancel IN FRAME SchemaFrame.

