


/* p-chgin4.p */

OS-COMMAND SILENT quoter -d , p-datfl4.d >p-datfl4.q.

INPUT FROM p-datfl4.q NO-ECHO.

REPEAT:
    CREATE customer.
    SET cust-num name sales-rep.
END.

INPUT CLOSE.



