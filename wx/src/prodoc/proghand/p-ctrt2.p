/* p-ctrt2.p */

DEFINE VARIABLE proci AS INTEGER.
DEFINE VARIABLE whand AS WIDGET-HANDLE.
DEFINE VARIABLE proc-name AS CHARACTER EXTENT 3
    INIT ["p-join2.p", "p-foftab.p", "p-wow1.p"].

CREATE WINDOW whand. CURRENT-WINDOW = whand.
MESSAGE "These are STATIC procedure executions." VIEW-AS ALERT-BOX.
RUN p-join2.p.
DELETE WIDGET whand.

CREATE WINDOW whand. CURRENT-WINDOW = whand.
RUN p-foftab.p.
DELETE WIDGET whand.

CREATE WINDOW whand. CURRENT-WINDOW = whand.
RUN p-wow1.p.
DELETE WIDGET whand.

DO proci = 1 TO 3:
    CREATE WINDOW whand. CURRENT-WINDOW = whand.
    IF proci = 1 THEN
        MESSAGE "These are DYNAMIC procedure executions."
            VIEW-AS ALERT-BOX.
    RUN VALUE(proc-name[proci]).
    DELETE WIDGET whand.
END.
    
