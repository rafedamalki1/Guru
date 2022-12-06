 /*r-shrfrm.i */

 FORM

  xcust.name         COLON 10    xcust.phone        COLON 55
  xcust.address      COLON 10    xcust.sales-rep    COLON 55
  csz NO-LABEL       COLON 10    xcust.credit-limit COLON 55
  SKIP(2)
  order.order-num    COLON 10    order.order-date   COLON 30
  order.ship-date    COLON 30
  order.promise-date COLON 30


  WITH SIDE-LABELS 1 DOWN CENTERED ROW 5
  TITLE   "Customer/Order Form" FRAME cust-frame.
