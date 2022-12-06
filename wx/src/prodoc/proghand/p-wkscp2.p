/* p-wkscp2.p */

DEFINE VARAIBLE saved-rep LIKE customer.sales-rep.

DO:
   FOR EACH customer: 
      DISPLAY cust-num name WITH FRAME a.
      saved-rep = customer.sales-rep.
      FOR EACH customer:
         IF customer.sales-rep = saved-rep
         THEN DISPLAY cust-name WITH FRAME a.
         DOWN WITH FRAME a.
      END.
   END.

   
 
END.
