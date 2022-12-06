/* cuhelp.p */

FORM customer.cust-num customer.name
WITH FRAME cust-frame 5 DOWN ROW 10 CENTERED
   OVERLAY TITLE " Available Customers ".

REPEAT WHILE FRAME-LINE(cust-frame) <= FRAME-DOWN(cust-frame):
   FIND NEXT customer.
   DISPLAY customer.cust-num customer.name WITH FRAME cust-frame.
   DOWN WITH FRAME cust-frame.
END.

UP 5 WITH FRAME cust-frame.

REPEAT:
   CHOOSE ROW customer.cust-num NO-ERROR WITH FRAME cust-frame.
   FIND customer WHERE customer.cust-num = INPUT customer.cust-num.
   IF KEYFUNCTION(LASTKEY) = "CURSOR-UP" THEN DO:
      FIND PREV customer NO-ERROR.
      IF AVAILABLE customer THEN DO:
	 SCROLL DOWN WITH FRAME cust-frame.
	 DISPLAY customer.cust-num customer.name  WITH FRAME cust-frame.
      END.
   END.
   ELSE
   IF KEYFUNCTION(LASTKEY) = "CURSOR-DOWN" THEN DO:
      FIND NEXT customer NO-ERROR.
      IF AVAILABLE customer THEN DO:
	 SCROLL UP WITH FRAME cust-frame.
	 DISPLAY customer.cust-num customer.name  WITH FRAME cust-frame.
      END.
   END.
   ELSE
   IF KEYFUNCTION(LASTKEY) = "RETURN" THEN DO:
       FRAME-VALUE = FRAME-VALUE.
       HIDE FRAME cust-frame.
       RETURN.
   END.
END.
