DEFINE QUERY cust-order FOR customer, order.

OPEN QUERY cust-order FOR EACH customer, EACH order OF customer.

GET FIRST cust-order.

DO WHILE AVAILABLE(order):
   DISPLAY customer.cust-num customer.name WITH FRAME cust-info.
   DISPLAY order WITH SIDE-LABELS FRAME order-info.
   PAUSE.
   
   GET NEXT cust-order.
END.
