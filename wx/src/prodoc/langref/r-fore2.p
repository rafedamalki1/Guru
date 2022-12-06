/* r-fore2.p */

FOR EACH customer, EACH order OF customer,
     EACH order-line OF order, item OF order-line
     BY promise-date BY customer.cust-num BY line-num:

     DISPLAY promise-date customer.cust-num
	  order.order-num line-num item.item-num item-name.
END.
