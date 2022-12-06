/* at.p */

DEFINE FRAME order-info
  order.cust-num   AT X 50 Y 14
  customer.name    AT X-OF order.cust-num + 100 Y 14 
  order.order-num  AT X-OF order.cust-num + 225 Y 14
  order.order-date AT X-OF order.cust-num + 320 Y 14
  WITH TITLE "Order Information" NO-LABELS.
 
FOR EACH order NO-LOCK
  BREAK BY order.cust-num WITH FRAME order-info:
  IF FIRST-OF(order.cust-num) THEN
  DO: 
    FIND customer OF order NO-LOCK.
    DISPLAY order.cust-num customer.name.
  END.

  DISPLAY order.order-num order.order-date.

END.
