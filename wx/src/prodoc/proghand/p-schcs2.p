/* p-schcs2.p */

DEFINE INPUT PARAMETER db-table AS CHARACTER.
DEFINE INPUT PARAMETER filen AS CHARACTER.

DEFINE VARIABLE itab AS INTEGER.

Table-Add: DO itab = 1 to NUM-ENTRIES(db-table):
    CASE ENTRY(itab, db-table):
        WHEN "customer"         THEN FIND FIRST customer NO-ERROR.
        WHEN "invoice"          THEN FIND FIRST invoice NO-ERROR.
        WHEN "item"             THEN FIND FIRST item NO-ERROR.
        WHEN "local-default"    THEN FIND FIRST local-default NO-ERROR.
        WHEN "order"            THEN FIND FIRST order NO-ERROR.
        WHEN "order-line"       THEN FIND FIRST order-line NO-ERROR.
        WHEN "ref-call"         THEN FIND FIRST ref-call NO-ERROR.
        WHEN "salesrep"         THEN FIND FIRST salesrep NO-ERROR.
        WHEN "state"            THEN FIND FIRST state NO-ERROR.
        OTHERWISE               LEAVE Table-Add.
    END CASE.
END.

SAVE CACHE CURRENT sports to VALUE(filen + ".csh") NO-ERROR.
IF NOT ERROR-STATUS:ERROR THEN
    MESSAGE "Saved partial schema cache for the sports database" 
            "in" filen + ".csh.".
ELSE DO:
    BELL.
    DO itab = 1 TO ERROR-STATUS:NUM-MESSAGES:
        MESSAGE ERROR-STATUS:GET-MESSAGE(itab) VIEW-AS ALERT-BOX.
    END.
END.
