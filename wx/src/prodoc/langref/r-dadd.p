/* r-dadd.p - DATE ADDITION */

DISPLAY "ORDERS SCHEDULED TO SHIP MORE THAN ONE WEEK LATE".

FOR EACH order WHERE ship-date = ?:
  IF TODAY > (promise-date + 7)
  THEN
  DO:
    FIND customer OF order.
    DISPLAY order.order-num order.cust-num customer.name promise-date
    customer.terms.
  END.
END.
