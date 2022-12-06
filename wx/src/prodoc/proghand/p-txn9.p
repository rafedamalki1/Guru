/* p-txn9.p */

ON F9 ENDKEY.

o-block:
REPEAT:
    INSERT order WITH 2 COLUMNS.
    FIND customer OF order.
    o-l-block:
    REPEAT ON ENDKEY UNDO o-block, LEAVE o-block:
	CREATE order-line.
	order-line.order-num = order.order-num.
	DISPLAY order-line.order-num.
	UPDATE line-num order-line.item-num qty.
	FIND item OF order-line.
	order-line.price = item.price.
	UPDATE order-line.price.
    END.
END.
