DEFINE BUTTON more-button LABEL "MORE".
DEFINE BUTTON next-button LABEL "NEXT".

DEFINE VARIABLE jump-ahead AS LOGICAL INITIAL TRUE.

FORM customer.cust-num customer.name more-button next-button
    WITH FRAME brief.

FORM customer EXCEPT cust-num name
    WITH FRAME full.

ON CHOOSE OF more-button
   DO:
      DISPLAY customer EXCEPT cust-num name WITH FRAME full.
      jump-ahead = FALSE.
   END.

ON CHOOSE OF next-button
   DO:
     jump-ahead = TRUE.
   END.

ON WINDOW-CLOSE OF CURRENT-WINDOW
   DO:
     QUIT.
   END.

ENABLE more-button next-button WITH FRAME brief.

DO WHILE TRUE:
   IF jump-ahead
   THEN RUN next-cust.

   WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW OR CHOOSE OF next-button
           FOCUS more-button PAUSE 3.
END.

PROCEDURE next-cust:
   HIDE FRAME full.
   FIND NEXT customer NO-ERROR.
   IF AVAILABLE customer
   THEN DISPLAY customer.cust-num customer.name WITH FRAME brief.
END.
