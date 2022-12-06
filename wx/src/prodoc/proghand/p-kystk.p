/* p-kystk.p */

DEFINE FRAME cust-frame
   Customer.Cust-num SKIP Customer.Name SKIP Customer.Address SKIP
   Customer.Address2 SKIP Customer.City Customer.State SKIP
   Customer.Sales-rep HELP "Use the space bar to scroll the values."
   WITH SIDE-LABELS.

REPEAT WITH FRAME cust-frame:
   PROMPT-FOR Customer.Cust-num.
   FIND Customer USING Cust-num.
   UPDATE Name Address City State Sales-rep
       EDITING:
          READKEY.
          IF FRAME-FIELD <> "Sales-rep"
          THEN DO:
             APPLY LASTKEY.
          END.
          ELSE DO:
             IF LASTKEY = KEYCODE(" ")
             THEN DO:
                FIND NEXT Salesrep NO-ERROR.
                IF NOT AVAILABLE Salesrep
                THEN FIND FIRST Salesrep.
                
                DISPLAY Salesrep.Sales-rep @ Customer.Sales-rep.
             END.
             ELSE IF LOOKUP(KEYFUNCTION(LASTKEY),
                            "TAB,BACK-TAB,GO,END-ERROR") > 0
                  THEN APPLY LASTKEY.
                  ELSE BELL.
          END.
       END.
END.
