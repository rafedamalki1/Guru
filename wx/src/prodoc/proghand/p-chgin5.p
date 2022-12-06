


/* p-chgin5.p */

OS-COMMAND SILENT quoter -c 1-2,4-20,22-24 p-datfl5.d >p-datfl5.q.

INPUT FROM p-datfl5.q NO-ECHO.

REPEAT:
    CREATE customer.
    SET cust-num name sales-rep.
END.

INPUT CLOSE.


