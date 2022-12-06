/* p-wkscp3.p */

DO:
   FOR EACH customer: 
      DISPLAY cust-num name WITH FRAME a.
   END. 
   
   FIND FIRST customer.
   DISPLAY customer.cust-num name WITH FRAME b. 
END.
