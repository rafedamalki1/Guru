/* r-choose2.p */

DEFINE VARIABLE counter AS INTEGER.
DEFINE VARIABLE oldchoice AS CHARACTER.

FORM customer.cust-num customer.name customer.address customer.city 
     WITH FRAME cust-frame SCROLL 1 5 DOWN ATTR-SPACE.

FIND FIRST customer.
REPEAT COUNTER = 1 TO 5:
    DISPLAY cust-num name address city WITH FRAME cust-frame.
    DOWN WITH FRAME cust-frame.
    FIND NEXT customer NO-ERROR.
    IF NOT AVAILABLE customer
    THEN LEAVE.
END.
UP 5 WITH FRAME cust-frame.
oldchoice = "".

REPEAT:
    STATUS DEFAULT "Use UP & DOWN ARROWS then enter C to create, D to delete".
    CHOOSE ROW customer.cust-num NO-ERROR GO-ON(CURSOR-RIGHT)
	WITH FRAME cust-frame.

    /* After choice */
    IF FRAME-VALUE = ""
    THEN NEXT.   /* Force user to press END or move cursor to valid line */
    IF FRAME-VALUE <> oldchoice
    THEN DO:
	oldchoice = FRAME-VALUE.
	FIND customer WHERE cust-num = INTEGER(FRAME-VALUE).
    END.

    /* React to moving cursor off the screen */

    IF LASTKEY = KEYCODE("CURSOR-DOWN")
    THEN DO:
	FIND NEXT customer NO-ERROR.
	IF NOT AVAILABLE customer
	THEN FIND FIRST customer.
	DOWN WITH FRAME cust-frame.
	DISPLAY cust-num name address city WITH FRAME cust-frame.
	NEXT.
    END.

   IF LASTKEY = KEYCODE("CURSOR-UP")
   THEN DO:
	FIND PREV customer NO-ERROR.
	IF NOT AVAILABLE customer
	THEN FIND LAST customer.
	UP WITH FRAME cust-frame.
	DISPLAY cust-num name address city WITH FRAME cust-frame.
	NEXT.
   END.

   /* CHOOSE selected a valid key.  Check which key. */

   IF LASTKEY = KEYCODE("c")
   THEN DO:              /* Open a space in the frame. */
	SCROLL FROM-CURRENT DOWN WITH FRAME cust-frame.
	CREATE customer.
	UPDATE cust-num name address city WITH FRAME cust-frame.
	oldchoice = INPUT cust-num.
	NEXT.
   END.

    IF LASTKEY = KEYCODE("d")
    THEN DO:           /* Delete a customer from the database. */
	DELETE customer.
	REPEAT counter = 1 TO 100 WHILE FRAME-LINE(cust-frame)
	  <= FRAME-DOWN(cust-frame).
	    FIND NEXT customer NO-ERROR.
	    IF AVAILABLE customer
	    THEN DISPLAY cust-num name address city WITH FRAME cust-frame.
	    ELSE CLEAR FRAME cust-frame.
	    DOWN WITH FRAME cust-frame.
	END.
	UP counter - 1 WITH FRAME cust-frame.
	oldchoice = INPUT cust-num.
    END.
END.
STATUS DEFAULT.
