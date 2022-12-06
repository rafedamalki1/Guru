/* p-txn3.p */

REPEAT:
    INSERT order WITH 2 COLUMNS.
    FIND customer OF order.
    REPEAT:
	CREATE order-line.
	order-line.order-num = order.order-num.
	DISPLAY order-line.order-num.
	UPDATE line-num order-line.item-num qty price.
    END.
END.

FOR EACH salesrep:
    DISPLAY sales-rep rep-name.
    UPDATE region.
END.
