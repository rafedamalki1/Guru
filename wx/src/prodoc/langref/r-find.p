/* r-find.p */

REPEAT:
    PROMPT-FOR item.item-num.
    FIND item USING item-num.
    DISPLAY item-num item-name.
    REPEAT:
	FIND NEXT order-line OF item.
	FIND order OF order-line.
	FIND customer WHERE customer.cust-num = order.cust-num.
	DISPLAY customer.name order.order-num order-line.qty (TOTAL).
    END.
END.
