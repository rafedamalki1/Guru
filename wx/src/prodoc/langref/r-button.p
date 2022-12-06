DEFINE BUTTON more-button LABEL "More".
DEFINE BUTTON next-button LABEL "Next".

FORM more-button next-button
   WITH FRAME but-frame ROW 1.

FORM Customer.cust-num name
   WITH FRAME brief ROW 4.

FORM customer EXCEPT cust-num name
   WITH FRAME full ROW 7.
   
ON CHOOSE OF more-button
   DISPLAY customer EXCEPT cust-num name WITH FRAME full.
   
ON CHOOSE OF next-button
   DO:
      HIDE FRAME full.
      FIND NEXT customer NO-ERROR.
      DISPLAY cust-num name WITH FRAME brief.
   END.

FIND FIRST customer.
DISPLAY cust-num name WITH FRAME brief.

ENABLE more-button next-button WITH FRAME but-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
