/* p-kystk3.p */

DEFINE FRAME cust-frame
   Customer.Cust-num SKIP Customer.Name SKIP Customer.Address SKIP
   Customer.Address2 SKIP Customer.City Customer.State SKIP
   Customer.Sales-rep WITH SIDE-LABELS.
   
REPEAT WITH FRAME cust-frame:
   PROMPT-FOR Customer.Cust-num.
   FIND Customer USING Cust-num.
   MESSAGE "Press F6 to see the previous Customer.".
   UPDATE Name Address Address2 City State Sales-rep 
       EDITING:
          READKEY. 
          IF KEYLABEL(LASTKEY) = "F6"
          THEN DO:
             FIND PREV Customer NO-ERROR.
             IF NOT AVAILABLE Customer
             THEN FIND FIRST Customer.
             
             DISPLAY Cust-num Name Address City State Sales-rep
                WITH FRAME cust-frame.
          END.
          ELSE APPLY LASTKEY.
       END.
   HIDE MESSAGE.
END.
