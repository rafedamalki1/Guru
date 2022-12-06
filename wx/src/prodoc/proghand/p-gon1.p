/* p-gon1.p */

DISPLAY "You may update each customer." SKIP
        "After making your changes, press one of:" SKIP(1)
        KBLABEL("GO") + "  - Make the change permanent" FORMAT "x(40)"
           SKIP
        KBLABEL("END-ERROR") + "  - Undo changes and exit" FORMAT "x(40)"
           SKIP
        "F8  - Undo changes and try again" SKIP
        "F10 - Find next customer" SKIP
        "F12 - Find previous customer"
        WITH CENTERED FRAME ins.
        
FIND FIRST Customer.

upd-loop:
REPEAT:
   UPDATE Cust-num Name Address Address2 City St
        GO-ON(F8 F10 F12) WITH 1 DOWN CENTERED.
        
   CASE LASTKEY:
      WHEN KEYCODE("F8") THEN
         UNDO upd-loop, RETRY upd-loop.
      WHEN KEYCODE("F10") THEN
         FIND NEXT Customer. 
      WHEN KEYCODE("F12") THEN 
         FIND PREV Customer.
   END CASE.
END.
