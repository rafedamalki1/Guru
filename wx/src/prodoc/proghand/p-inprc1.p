/* p-inprc1.p */

DEFINE BUFFER cust_buf FOR customer.
DEFINE BUFFER ord_buf FOR order.
DEFINE VARIABLE t-i-item-num LIKE item.item-num.

RUN p-exprc3.p (INPUT THIS-PROCEDURE).

PROCEDURE getcust:
   DEFINE INPUT PARAMETER t-i-cust-num LIKE customer.cust-num.
   FIND FIRST cust_buf WHERE cust_buf.cust-num = t-i-cust-num NO-ERROR.
   IF NOT AVAILABLE cust_buf
   THEN DO:
      MESSAGE "Customer not found.".
      RETURN ERROR.
   END.
   ELSE DO:
      FIND FIRST ord_buf OF cust_buf NO-ERROR.
      IF AVAILABLE ord_buf
      THEN RUN getord.
      ELSE MESSAGE "No orders for customer.".
   END.
END PROCEDURE. /* getcust */

PROCEDURE getord:
   FOR EACH order-line OF ord_buf:
      DISPLAY order-line WITH FRAME foo.
      t-i-item-num = order-line.item-num.
      RUN getitem.
   END.
END PROCEDURE. /* getord */

PROCEDURE getitem:
   FIND FIRST item WHERE item.item-num = t-i-item-num.
   DISPLAY cust_buf.cust-num cust_buf.name item.item-num item.item-name
      WITH FRAME foo1.
END PROCEDURE. /* getitem */
