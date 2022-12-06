/* p-frm17.p */

DEFINE BUTTON del-button LABEL "Delete Customer"
   TRIGGERS:
      ON CHOOSE
          DELETE customer.
   END.
DEFINE BUTTON next-button LABEL "Find Another Customer" AUTO-GO. 
DEFINE BUTTON quit-button LABEL "Quit" AUTO-ENDKEY.

REPEAT:
        PROMPT-FOR customer.cust-num quit-button WITH FRAME a.
        FIND customer USING cust-num.
        DISPLAY name.
        UPDATE del-button next-button quit-button WITH FRAME a. 
END.

