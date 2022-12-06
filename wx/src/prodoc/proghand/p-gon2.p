/* p-gon2.p */

FORM
   Customer.Cust-num Customer.Name Customer.Address
   Customer.Address2 Customer.City Customer.State
   WITH CENTERED FRAME upd-frame.

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

       
ON F8 ANYWHERE
   DO:
     DISPLAY Customer.Cust-num Name Address City State
        WITH CENTERED FRAME upd-frame. 
   END.
ON F10 ANYWHERE
   DO:
      APPLY "GO" TO FRAME upd-frame.
      FIND NEXT Customer.
      DISPLAY Cust-num Name Address Address2 City State
         WITH CENTERED FRAME upd-frame. 
   END.
   
ON F12 ANYWHERE
   DO:
      APPLY "GO" TO FRAME upd-frame.
      FIND PREV Customer.
      DISPLAY Cust-num Name Address Address2 City State
         WITH CENTERED FRAME upd-frame. 
   END.
   
ON GO OF FRAME upd-frame
   ASSIGN FRAME upd-frame Customer.Cust-num Name Address Address2
          City State.
  
ENABLE ALL WITH FRAME upd-frame.

FIND FIRST Customer.

APPLY "F8" TO FRAME upd-frame.   

WAIT-FOR END-ERROR OF FRAME upd-frame.
