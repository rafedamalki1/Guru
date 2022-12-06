DEFINE BUTTON order-but LABEL "Order"
   TRIGGERS:
      ON CHOOSE
         DO:
            FIND FIRST Order OF Customer NO-ERROR.
            IF AVAILABLE(Order)
            THEN UPDATE Order WITH FRAME upd-dlg
                     VIEW-AS DIALOG-BOX TITLE "Update Order" SIDE-LABELS.
         END.
   END TRIGGERS.
   
FORM
   order-but Customer.Name WITH FRAME x.
   
ON F10 OF Customer.Name 
   DO:
      APPLY "CHOOSE" TO order-but IN FRAME x.
   END.
   
FIND FIRST Customer.

DISPLAY order-but Customer.Name WITH FRAME x.

ENABLE ALL WITH FRAME x.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
