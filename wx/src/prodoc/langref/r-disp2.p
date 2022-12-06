/* r-disp2.p */

FOR EACH order, customer OF order:
     DISPLAY order-num customer.name ship-date promise-date.
     FOR EACH order-line OF order, item OF order-line:
	  DISPLAY line-num item-name qty order-line.price
	    qty * order-line.price (TOTAL) LABEL "Order-value".
     END.
END.
