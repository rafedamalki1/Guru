/* p-scroll.p */

DEFINE VARIABLE current_line AS INTEGER.

FORM customer.cust-num customer.name customer.address
     customer.city customer.postal-code
     WITH FRAME cust-frame SCROLL 1 5 DOWN WIDTH 90.

CURRENT-WINDOW:WIDTH-CHARS = 90.

FIND FIRST customer.

REPEAT current_line = 1 TO 5:
    DISPLAY cust-num name address city postal-code
       WITH FRAME cust-frame.
    DOWN WITH FRAME cust-frame.
    FIND NEXT customer NO-ERROR.
    IF NOT AVAILABLE customer
    THEN LEAVE.
END.
UP 5 WITH FRAME cust-frame.

REPEAT:
   STATUS DEFAULT
    "Use up and down arrows. Enter C to create, D to delete".
    CHOOSE ROW customer.cust-num NO-ERROR GO-ON(CURSOR-RIGHT)
        WITH FRAME cust-frame.
    FIND customer WHERE cust-num = INTEGER(INPUT cust-num).
    
    /* React to moving cursor off the screen */
    IF LASTKEY = KEYCODE("CURSOR-DOWN")
    THEN DO:
        FIND NEXT customer NO-ERROR.
        IF NOT AVAILABLE customer
        THEN FIND FIRST customer.
        DOWN WITH FRAME cust-frame.
        DISPLAY cust-num name address city postal-code
          WITH FRAME cust-frame.
        NEXT.
    END.

    IF LASTKEY = KEYCODE("CURSOR-UP")
       THEN DO:
          FIND PREV customer NO-ERROR.
          IF NOT AVAILABLE customer
          THEN FIND LAST customer.
          UP WITH FRAME cust-frame.
          DISPLAY cust-num name address city postal-code
            WITH FRAME cust-frame.
          NEXT.
    END.

/* CHOOSE selected a valid key.  Check which key. */   
    IF KEYLABEL(LASTKEY) = "c"
       THEN DO:              
          /* Open a space in the frame. */
          SCROLL FROM-CURRENT DOWN WITH FRAME cust-frame.
          CREATE customer.
          UPDATE cust-num name address city postal-code
            WITH FRAME cust-frame.
          NEXT.
    END.

IF KEYLABEL(LASTKEY) = "d"
      THEN DO:       
        /* Delete a customer from the database. */
        DELETE customer.
        SCROLL FROM-CURRENT WITH FRAME cust-frame.
        current_line = FRAME-LINE(cust-frame).
        DOWN FRAME-DOWN(cust-frame) - FRAME-LINE(cust-frame) - 1
          WITH FRAME cust-frame.
          /* Place cursor on last active line in frame */
        FIND customer WHERE cust-num = INTEGER(INPUT cust-num).
        FIND NEXT customer NO-ERROR.
        IF NOT AVAILABLE customer
        THEN FIND FIRST customer.
        DOWN WITH FRAME cust-frame.
        DISPLAY cust-num name address city postal-code
           WITH FRAME cust-frame.
        UP FRAME-LINE(cust-frame) - current_line
           WITH FRAME cust-frame.
           /* Move cursor back to where it was at start of block */
    END.
END.
STATUS DEFAULT. 

