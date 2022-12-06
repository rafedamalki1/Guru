/* p-rasscp.p */

/* The scope of the customer record is the Procedure
   block since that is the outermost block in which the
   customer record is referenced. */

REPEAT:
    INSERT customer.
END.

DISPLAY customer WITH 2 COLUMNS.
