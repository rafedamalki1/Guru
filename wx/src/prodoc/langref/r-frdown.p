/* r-frdown.p */

DEFINE VARIABLE ans AS LOGICAL.

REPEAT:
   FIND NEXT customer.
   DISPLAY cust-num name.
   IF FRAME-LINE = FRAME-DOWN
   THEN DO:
       MESSAGE "Do you want to see the next page ?"
       UPDATE ans.
       IF NOT ans
       THEN LEAVE.
   END.
END.
