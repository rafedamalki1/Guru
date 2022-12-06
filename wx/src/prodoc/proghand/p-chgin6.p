/* p-chgin6.p */

DEFINE VARIABLE data AS CHARACTER.
 
    
INPUT FROM p-datfl3.d NO-ECHO.

REPEAT:
    CREATE customer.
    IMPORT UNFORMATTED data.
    cust-num = INTEGER(SUBSTRING(data,1,2)).
    name = SUBSTRING(data,4,17).
    sales-rep = SUBSTRING(data,22,3).
END.

INPUT CLOSE.
