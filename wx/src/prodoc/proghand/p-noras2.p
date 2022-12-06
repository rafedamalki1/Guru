/* p-noras2.p */

/* This procedure cannot compile because the customer buffer is
   referenced in a strong-scope block first and therefore cannot
   be raised from the weak-scope block to the procedure block.    */
   
REPEAT FOR customer:
   FIND NEXT customer.
   DISPLAY name.
END.

FOR EACH customer:
   DISPLAY address city state postal-code.
END.

DISPLAY customer.
