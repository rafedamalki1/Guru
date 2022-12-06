/* r-insrt.p */

REPEAT:
    INSERT order WITH 1 COLUMN.
    REPEAT:
	CREATE order-line.
	order-line.order-num = order.order-num.
	UPDATE line-num order-line.item-num qty price.
	/* Verify the item-num by finding an item
	   with that number */
	FIND item OF order-line.
    END.
 END.
