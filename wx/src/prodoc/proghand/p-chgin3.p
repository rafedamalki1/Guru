


/* p-chgin3.p */

DEFINE VARIABLE data AS CHARACTER FORMAT "x(78)".

OS-COMMAND SILENT quoter p-datfl3.d > p-datfl3.q.
    
INPUT FROM p-datfl3.q NO-ECHO.

REPEAT:
    CREATE customer.
    SET data WITH NO-BOX NO-LABELS NO-ATTR-SPACE WIDTH 80.
    cust-num = INTEGER(SUBSTRING(data,1,2)).
    name = SUBSTRING(data,4,17).
    sales-rep = SUBSTRING(data,22,3).
END.

INPUT CLOSE.



