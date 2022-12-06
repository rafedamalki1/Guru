DEFINE FRAME cust-frame.
DEFINE QUERY custq FOR customer.

DEFINE BUTTON nextcust LABEL "Next"
   TRIGGERS:
      ON CHOOSE
          DO:
              GET NEXT custq.
              DISPLAY customer WITH FRAME cust-frame.
          END.
   END TRIGGERS.

DEFINE BUTTON prevcust LABEL "Previous"
   TRIGGERS:
      ON CHOOSE
          DO:
              GET PREV custq.
              DISPLAY customer WITH FRAME cust-frame.
          END.
   END TRIGGERS.

OPEN QUERY custq FOR EACH customer.

GET FIRST custq.
DISPLAY customer WITH FRAME cust-frame.

ENABLE nextcust AT COLUMN 1 ROW 7 prevcust WITH FRAME cust-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
