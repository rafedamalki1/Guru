/* p-kystk4.p */

DEFINE FRAME cust-frame
   Customer.Cust-num SKIP Customer.Name SKIP Customer.Address SKIP
   Customer.Address2 SKIP Customer.City Customer.State SKIP
   Customer.Sales-rep WITH SIDE-LABELS.

ON F6 ANYWHERE
  DO:
     IF FRAME-FIELD = "Cust-num"
     THEN RETURN NO-APPLY.
     
     FIND PREV Customer NO-ERROR.
     IF NOT AVAILABLE Customer
     THEN FIND FIRST Customer.
     
     DISPLAY Cust-num Name Address City State Sales-rep
        WITH FRAME cust-frame.
  END.
   
REPEAT WITH FRAME cust-frame:
   PROMPT-FOR Customer.Cust-num.
   FIND Customer USING Cust-num.
   MESSAGE "Press F6 to see the previous Customer.".
   UPDATE Name Address Address2 City State Sales-rep.
   HIDE MESSAGE.
END.
