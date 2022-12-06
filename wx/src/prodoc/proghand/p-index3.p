/* p-index3.p */

REPEAT:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num.
    FOR EACH order WHERE order.cust-num = customer.cust-num:
	DISPLAY order-num terms.
	FOR EACH order-line WHERE order-line.order-num = order.order-num:
	    DISPLAY line-num qty.
	    FIND item WHERE item.item-num = order-line.item-num.
	    DISPLAY idesc on-hand alloc.
	END.
    END.
END.
