/* p-kystkb.p */

DEFINE FRAME cust-frame
     Customer.Cust-num SKIP Customer.name SKIP Customer.address SKIP
     Customer.address2 SKIP Customer.city Customer.state SKIP 
     Customer.sales-rep HELP "To see a list of values, press F6."
     WITH DOWN SIDE-LABELS.

ON F6 OF Customer.Sales-rep
   DO: 
      FOR EACH salesrep:
        DISPLAY Salesrep.Sales-rep
            WITH NO-LABELS 9 DOWN COLUMN 60 ROW 5.
      END.
   END.
  
REPEAT WITH FRAME cust-frame:
   PROMPT-FOR Customer.Cust-num.
   FIND Customer USING Cust-num. 
   UPDATE Name Address Address2 City State Sales-rep.
END.
