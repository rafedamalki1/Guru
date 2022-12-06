TRIGGER PROCEDURE FOR ASSIGN OF customer.cust-num 
       OLD VALUE oldcust.

FOR EACH order WHERE order.cust-num = oldcust:
   order.cust-num = customer.cust-num.
END.

