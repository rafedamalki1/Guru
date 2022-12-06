/*r-frname.p */
FOR EACH customer, EACH order OF customer:
    DISPLAY order-num WITH CENTERED ROW 2 FRAME onum.
    UPDATE
    customer.cust-num AT 5 customer.name AT 30 SKIP
	WITH FRAME custfrm WITH CENTERED 1 DOWN
    EDITING:
	DISPLAY " You are currently editing a frame called "
	   FRAME-NAME WITH FRAME d1 WITH 1 DOWN CENTERED.
	READKEY.
	APPLY LASTKEY.
	IF LASTKEY = KEYCODE("RETURN") THEN
	    MESSAGE " Press the space bar to edit order shipdate".
    END. /* Editing */
    HIDE FRAME custfrm.
    HIDE FRAME d1.
    UPDATE
      ship-date AT 5
       WITH FRAME orderfrm WITH CENTERED 1 DOWN
    EDITING:
      DISPLAY " Now you are editing a frame called"
	FRAME-NAME WITH FRAME d2 WITH 1 DOWN CENTERED.
      READKEY.
      APPLY LASTKEY.
    END.
    HIDE FRAME orderfrm.
    HIDE FRAME d2.
 END.
