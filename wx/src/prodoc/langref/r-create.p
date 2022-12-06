/* r-create.p */

REPEAT:
    CREATE order.
    UPDATE order.order-num
	order.cust-num VALIDATE(CAN-FIND(customer OF order),
				"Customer does not exist")
	cust-num order-date.
    REPEAT:
	CREATE order-line.
	order-line.order-num = order.order-num.
	UPDATE line-num
	       order-line.item-num VALIDATE(CAN-FIND(item OF order-line),
					    "Item does not exist")
	       qty
	       price.
    END.
END.
