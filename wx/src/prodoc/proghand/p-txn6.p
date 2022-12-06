/* p-txn6.p */

ON F9 ERROR.

REPEAT:
    INSERT order WITH 2 COLUMNS.
    FIND customer OF order.
    REPEAT:
	CREATE order-line.
	order-line.order-num = order.order-num.
	DISPLAY order-line.order-num.
	UPDATE line-num order-line.item-num qty.
	FIND item OF order-line.
	order-line.price = item.price.
	UPDATE order-line.price.
    END.
END.
