/* p-txn5.p */

DEFINE VARIABLE extension LIKE order-line.price.
DEFINE VARIABLE tot-order LIKE order-line.price.

REPEAT:
    INSERT order WITH 2 COLUMNS.
    tot-order = 0.
    o-l-block:
    REPEAT:
	CREATE order-line.
	order-line.order-num = order.order-num.
	DISPLAY order-line.order-num.
	UPDATE line-num order-line.item-num qty price.
	extension = qty * price.
	tot-order = tot-order + extension.
	IF tot-order > 500 THEN DO:
	    MESSAGE "Order has exceeded $500".
	    MESSAGE "No more order-lines allowed".
	    UNDO o-l-block.
	END.
    END.
END.
