/* p-dicust.i */

   FORM
     xcust.cust-num   COLON 10 LABEL "Cust #"
     xcust.name    COLON 10           xcust.phone        COLON 50
     xcust.address COLON 10           xcust.sales-rep    COLON 50
     csz     NO-LABEL COLON 12        xcust.credit-limit COLON 50
     SKIP (2)
     order.order-num    COLON 10      order.order-date   COLON 30
     order.ship-date    COLON 30
     order.promise-date COLON 30

   WITH SIDE-LABELS 1 DOWN CENTERED ROW 5
        TITLE " CUSTOMER/ORDER FORM " FRAME cust-frame.
