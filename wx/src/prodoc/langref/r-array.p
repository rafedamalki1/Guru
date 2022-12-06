/* r-array.p */

DEFINE VARIABLE month-order  AS INTEGER EXTENT 12 LABEL "Monthly Orders".
DEFINE VARIABLE rpt-year     AS INTEGER FORMAT "9999".
DEFINE VARIABLE total-orders AS INTEGER LABEL "Total Orders".

SET rpt-year.

FOR EACH order WHERE YEAR(order-date) = rpt-year:
   month-order[MONTH(order-date)] = month-order[MONTH(order-date)] + 1.
   total-orders = total-orders + 1.
END.

DISPLAY total-orders month-order
   WITH TITLE "Orders for " + STRING(rpt-year).
