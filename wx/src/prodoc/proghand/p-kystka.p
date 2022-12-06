/* p-kystka.p */

DEFINE FRAME cust-frame
     Customer.Cust-num SKIP Customer.name SKIP Customer.address SKIP
     Customer.address2 SKIP Customer.city Customer.state SKIP 
     Customer.sales-rep HELP "To see a list of values, press F6."
     WITH DOWN SIDE-LABELS.


REPEAT WITH FRAME cust-frame:
   PROMPT-FOR Customer.Cust-num.
   FIND Customer USING Cust-num. 
   UPDATE Name Address Address2 City State Customer.Sales-rep 
      EDITING:
         READKEY.
         IF FRAME-FIELD = "sales-rep" AND LASTKEY = KEYCODE("F6")
         THEN DO:
              FOR EACH Salesrep:
                 DISPLAY Salesrep.Sales-rep
                    WITH NO-LABELS 9 DOWN COLUMN 60 ROW 5.
              END.
         END.
         ELSE DO:
              APPLY LASTKEY.
         END.
      END.
END.
