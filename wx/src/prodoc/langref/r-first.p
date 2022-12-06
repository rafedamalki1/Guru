/* r-first.p */

DEFINE VARIABLE order-value AS DECIMAL LABEL "Order-value".

FOR EACH order:
     DISPLAY order-num.
     FOR EACH order-line OF order BREAK BY qty * price:
	  IF FIRST(qty * price) THEN order-value = 0.
	  order-value = order-value + qty * price.
	  DISPLAY line-num item-num qty * price LABEL "Extended-price".
     END.
     DISPLAY order-value.
END.
