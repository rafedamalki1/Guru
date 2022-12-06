REPEAT PRESELECT EACH order, customer OF order, EACH order-line OF order
            BY order.order-date BY order.cust-num BY order-line.item-num:
   FIND NEXT order-line.
   DISPLAY order.order-date order.cust-num customer.name
           order-line.item-num.
END.
