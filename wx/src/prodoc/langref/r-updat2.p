/* r-updat2.p */

FOR EACH customer:
     UPDATE customer.name
	  credit-limit VALIDATE(credit-limit < 500000, "Too high")
	  HELP "Enter credit-limit < 500000".
     FOR EACH order OF customer:
	  DISPLAY order-num.
	  UPDATE promise-date ship-date VALIDATE(ship-date > today,
		"Ship date must be later than today").
     END.
END.
