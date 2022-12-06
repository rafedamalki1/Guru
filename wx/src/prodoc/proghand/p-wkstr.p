/* p-wkstr.p */

DO:
   DO FOR customer:
       FIND FIRST customer.
       DISPLAY cust-num name.
   END.
  
   FOR EACH customer:
      DISPLAY cust-num name.
   END.
END.
