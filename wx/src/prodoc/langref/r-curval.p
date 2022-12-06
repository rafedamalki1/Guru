/* r-curval.p */

DEFINE VARIABLE cur-cust LIKE customer.cust-num NO-UNDO.

cur-cust = CURRENT-VALUE(next-cust-num).
IF CAN-FIND(FIRST order WHERE order.cust-num = cur-cust) THEN
  FOR EACH order WHERE order.cust-num = cur-cust,
      EACH order-line OF order NO-LOCK
      BREAK BY order.order-num:
  
      IF FIRST-OF(order.order-num) THEN
        DISPLAY order.order-num order.order-date order.cust-num
          WITH FRAME order-info CENTERED ROW 2 1 COL.
   
      DISPLAY order-line.
  END.
ELSE
DO:
  FIND FIRST customer WHERE customer.cust-num = cur-cust NO-LOCK NO-ERROR.
  
  IF AVAILABLE customer THEN   
     MESSAGE "No Orders Exist for Customer " + customer.name +
          ", " + string(customer.cust-num)
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "No Orders".
  ELSE MESSAGE "Customer number" cur-cust "does not exist."
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "No Customer".
END.
