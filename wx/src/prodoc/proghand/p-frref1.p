/* p-frref1.p */

DO:
  REPEAT:
     FIND NEXT customer.
     DISPLAY cust-num name WITH FRAME a.
     LEAVE.
  END.
 
  FIND NEXT customer.
  DISPLAY cust-num name WITH FRAME b. 
END.
