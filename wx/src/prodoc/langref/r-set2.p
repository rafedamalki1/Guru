/* r-set2.p */

FOR EACH customer:
     DISPLAY cust-num name credit-limit.
     SET name credit-limit
	 VALIDATE(credit-limit > 0, "Invalid credit limit.")
	 HELP "Enter a positive credit-limit.".
     REPEAT:
	  CREATE order.
	  cust-num = customer.cust-num.
	  SET order-num
	      ship-date VALIDATE(ship-date > today,
	                         "Ship date too early.").
     END.
END.
