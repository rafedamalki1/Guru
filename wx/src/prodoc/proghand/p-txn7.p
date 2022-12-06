/* p-txn7.p */

ON F9 ERROR.

o-block:
REPEAT:
    INSERT order WITH 2 COLUMNS.
    FIND customer OF order.
    o-l-block:
    REPEAT ON ERROR UNDO o-block, RETRY o-block:
	CREATE order-line.
	order-line.order-num = order.order-num.
	DISPLAY line-num order-line.item-num qty.
	SET line-num order-line.item-num qty.
	FIND item OF order-line.
	order-line.price = item.price.
	UPDATE order-line.price.
    END.
END.
