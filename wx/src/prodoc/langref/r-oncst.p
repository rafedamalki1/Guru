ON WRITE OF customer NEW new-cust OLD old-cust
  DO:
     IF new-cust.city <> old-cust.city AND
        new-cust.postal-code = old-cust.postal-code
     THEN DO:
         MESSAGE "Must update postal code, too.".
         RETURN ERROR.
     END.
  END.

FOR EACH customer:
   UPDATE customer.
END.
