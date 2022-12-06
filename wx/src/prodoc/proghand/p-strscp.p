/* p-strscp.p */

DO:
   DO FOR customer:
      FIND FIRST customer.
      DISPLAY cust-num name WITH FRAME a.
   END.

   
   DO FOR customer:
      FIND NEXT customer.
      DISPLAY cust-num name WITH FRAME b. 
   END.

END.
