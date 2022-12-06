/* p-txn11a.p */

DEFINE SHARED VARIABLE cust-num-var AS ROWID.

HIDE ALL.
FIND customer WHERE ROWID(customer) = cust-num-var.
FOR EACH order OF customer:
    UPDATE order WITH 2 COLUMNS.
    FOR EACH order-line OF order:
	UPDATE order-line.
    END.
END.
