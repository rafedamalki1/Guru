/* p-txn10.p */

REPEAT:
    DO TRANSACTION:
	INSERT order WITH 2 COLUMNS.
	FIND customer OF order.
    END.
    REPEAT TRANSACTION:
	CREATE order-line.
	order-line.order-num = order.order-num.
	DISPLAY order-line.order-num.
	UPDATE line-num order-line.item-num qty price.
	FIND item OF order-line.
    END.
END.
