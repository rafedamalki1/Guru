/* at.p */

DEFINE FRAME order-info
  order.cust-num   AT ROW 2 COLUMN 8
  customer.name    AT ROW 2 COLUMN 18
  order.order-num  AT ROW 2 COLUMN 50
  order.order-date AT ROW 2 COLUMN 65
  WITH TITLE "Order Information".
 
FOR EACH order NO-LOCK BREAK BY order.cust-num WITH FRAME order-info:
  IF FIRST-OF(order.cust-num) THEN
  DO: 
    FIND customer OF order NO-LOCK.
    DISPLAY order.cust-num customer.name.
  END.

  DISPLAY order.order-num order.order-date.
 
END.
