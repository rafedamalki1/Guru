/* p-frref2.p */

DO:
  REPEAT:
     FIND NEXT customer.
     DISPLAY cust-num name WITH FRAME a.
     LEAVE.
  END.
  
  REPEAT:
     FIND NEXT customer.
     DISPLAY cust-num name WITH FRAME b.     
  END.

END.
