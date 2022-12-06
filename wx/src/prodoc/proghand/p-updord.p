/* p-updord.p */

DEFINE SHARED FRAME cust-frame.
DEFINE SHARED VARIABLE csz AS CHARACTER FORMAT "x(22)".
DEFINE SHARED BUFFER xcust FOR customer.

FOR EACH order OF xcust:
    {p-dicust.i}      /* include file for layout of shared frame */

    DISPLAY order.order-num WITH FRAME cust-frame.
    UPDATE order.order-date order.ship-date
           order.promise-date WITH FRAME cust-frame.
END.
