/* r-uschp.p */

DEFINE VARIABLE line AS CHARACTER FORMAT "x(30)".

IF OPSYS = "unix" THEN UNIX SILENT VALUE(SEARCH("quoter"))
			new-item >new-item.q.
ELSE DOS quoter new-item >new-item.q.
INPUT FROM new-item.q NO-ECHO.

REPEAT:
    CREATE item.
    SET line.
    item-num = INTEGER(SUBSTR(line,1,6)).
    item-name = SUBSTR(line,7,24).
END.

INPUT CLOSE.
