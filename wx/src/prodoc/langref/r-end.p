/* r-end.p */

FOR EACH customer:
  DISPLAY customer.cust-num name phone.
  FOR EACH order OF customer:
    DISPLAY order WITH 2 COLUMNS.
  END.
END.
