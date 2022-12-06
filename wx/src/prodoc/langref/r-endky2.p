/* r-endky2.p */

customer:
FOR EACH customer:
     DISPLAY cust-num.
     UPDATE credit-limit.
     FOR EACH order OF customer
	   ON ENDKEY UNDO customer, LEAVE customer:
	UPDATE order-num ship-date.
     END.
END.
