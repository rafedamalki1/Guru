/* p-mulbrw.p */

DEFINE QUERY orders FOR order.
DEFINE BROWSE ord QUERY orders DISPLAY order.order-num order.cust-num
    WITH 8 DOWN.

DEFINE QUERY items FOR order-line, item.
DEFINE BROWSE itm QUERY items
    DISPLAY order-line.order-num order-line.line-num item.item-name
     WITH 2 DOWN.

FORM
  itm
  WITH FRAME itm-frame.

OPEN QUERY orders FOR EACH order.

ON "ITERATION-CHANGED" OF BROWSE ord
   DO:
       ENABLE itm WITH FRAME itm-frame COLUMN 30.
       OPEN QUERY items FOR EACH order-line OF order,
            EACH item OF order-line.
   END.

ENABLE ord WITH FRAME ord-frame.
APPLY "ITERATION-CHANGED" TO BROWSE ord.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
