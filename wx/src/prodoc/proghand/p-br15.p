/* p-br15.p */

DEFINE QUERY orders FOR order.
DEFINE BROWSE ord QUERY orders DISPLAY order.cust-num order.order-num 
    WITH 10 DOWN SEPARATORS.

DEFINE QUERY items FOR order-line, item.
DEFINE BROWSE itm QUERY items
     DISPLAY order-line.line-num item.item-name
     ENABLE item-name
     WITH 10 DOWN SEPARATORS.

DEFINE FRAME f1
  ord space(4) itm
  WITH NO-BOX SIDE-LABELS ROW 2 CENTERED.

ON "VALUE-CHANGED" OF BROWSE ord
   DO:
       ENABLE itm WITH FRAME f1.
       OPEN QUERY items FOR EACH order-line OF order,
            EACH item OF order-line.
   END.

OPEN QUERY orders FOR EACH order.
ENABLE ord WITH FRAME f1.
APPLY "VALUE-CHANGED" TO BROWSE ord.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
