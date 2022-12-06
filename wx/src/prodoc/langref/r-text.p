/* r-text.p */

DEFINE VARIABLE s-com AS CHARACTER FORMAT "x(40)" EXTENT 5.

FORM "Shipped   :" order.ship-date AT 13 SKIP
     "Misc Info :" order.instructions AT 13 SKIP(1)
     "Order Comments :" s-com AT 1
WITH FRAME o-com CENTERED NO-LABELS TITLE "Shipping Information".

FOR EACH customer, EACH order OF customer:
    DISPLAY cust.cust-num cust.name order.order-num order.order-date
           order.promise-date WITH FRAME order-hdr CENTERED.
    UPDATE ship-date instructions TEXT(s-com) WITH FRAME o-com.
    s-com = "".
END.
