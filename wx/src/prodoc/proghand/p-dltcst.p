TRIGGER PROCEDURE FOR DELETE OF customer.

FOR EACH invoice OF customer:
   IF invoice.amount > invoice.total-paid + invoice.adjustment THEN DO:
       MESSAGE "Outstanding unpaid invoice exists. Cannot delete.".
       RETURN ERROR.
   END.
END.

FOR EACH order OF customer:
   DELETE order.
END.

