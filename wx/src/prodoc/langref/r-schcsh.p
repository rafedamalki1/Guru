/* r-schcsh.p */

DEFINE VARIABLE db-name AS CHARACTER FORMAT "x(12)" INITIAL ?.
DEFINE VARIABLE icnt AS INTEGER.

DO WHILE db-name <> "":
    SET db-name LABEL "Database Name" 
        WITH FRAME A SIDE-LABELS TITLE "Save Cache" VIEW-AS DIALOG-BOX.
    IF db-name <> "" THEN 
        CONNECT VALUE(db-name) -1 NO-ERROR.
    ELSE
        LEAVE.
    IF NOT ERROR-STATUS:ERROR THEN DO:
        SAVE CACHE COMPLETE VALUE(db-name) to VALUE(db-name + ".csh")
            NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN
            MESSAGE "Saved schema cache for" 
                    db-name "in" db-name + ".csh.".
        ELSE DO:
            BELL.
            DO icnt = 1 TO ERROR-STATUS:NUM-MESSAGES:
                MESSAGE ERROR-STATUS:GET-MESSAGE(icnt)
                    VIEW-AS ALERT-BOX.
            END.
        END.
    END.
    ELSE DO:
        BELL.
        DO icnt = 1 TO ERROR-STATUS:NUM-MESSAGES:
            MESSAGE ERROR-STATUS:GET-MESSAGE(icnt)
                VIEW-AS ALERT-BOX.
        END.
    END.
    DISCONNECT VALUE(db-name) NO-ERROR.
END.
