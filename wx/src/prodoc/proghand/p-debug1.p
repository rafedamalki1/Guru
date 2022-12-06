/* p-debug1.p */

DEFINE FRAME cust-frame
     Customer.Cust-num SKIP Customer.Name SKIP Customer.Address SKIP
     Customer.Address2 SKIP Customer.City SKIP Customer.State SKIP 
     Customer.Sales-rep HELP "Use the space bar to scroll the values"
     WITH SIDE-LABELS.

ON " " OF Customer.Sales-rep
   DO:
      FIND NEXT Salesrep NO-ERROR.
      IF NOT AVAILABLE Salesrep
      THEN FIND FIRST Salesrep.
      DISPLAY Salesrep.Sales-rep @ Customer.Sales-rep 
           WITH FRAME cust-frame.
      RETURN NO-APPLY.
   END.

ON ENTRY, LEAVE, TAB, BACK-TAB, GO, END-ERROR OF Customer.Sales-rep
   DO:
     APPLY LASTKEY TO SELF. 
   END.
   
ON ANY-KEY OF Customer.Sales-rep
   DO: 
      BELL.
      RETURN NO-APPLY.
   END.
  
REPEAT WITH FRAME cust-frame:
   PROMPT-FOR Customer.Cust-num.
   FIND Customer USING Cust-num.
   UPDATE Name Address Address2 City State Customer.Sales-rep. 
END.
