/* r-disp.p */

FOR EACH customer BY state BY name:
     DISPLAY state cust-num name.
     FOR EACH order OF customer:
	  DISPLAY order-num name ship-date promise-date.
	  FOR EACH order-line OF order, item OF order-line:
	       DISPLAY line-num item-name qty order-line.price.
	  END.
     END.
END.
