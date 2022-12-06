DEFINE BUTTON more-button LABEL "MORE".
DEFINE BUTTON next-button LABEL "NEXT".

FORM customer.cust-num customer.name more-button next-button
    WITH FRAME brief.

FORM customer EXCEPT cust-num name
    WITH FRAME full.

ON CHOOSE OF more-button
   DISPLAY customer EXCEPT cust-num name WITH FRAME full.

ON CHOOSE OF next-button
   DO:
      HIDE FRAME full.
      FIND NEXT customer NO-ERROR.
      IF AVAILABLE customer
      THEN DISPLAY customer.cust-num customer.name WITH FRAME brief.
   END.

FIND FIRST customer.

DISPLAY customer.cust-num customer.name WITH FRAME brief.

ENABLE more-button next-button WITH FRAME brief.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW FOCUS more-button.
