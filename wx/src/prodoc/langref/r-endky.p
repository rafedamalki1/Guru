/* r-endky.p */

ON WINDOW-CLOSE OF CURRENT-WINDOW
   STOP.

FOR EACH customer ON ENDKEY UNDO, RETRY:
     DISPLAY cust-num name credit-limit.
     SET credit-limit VALIDATE(credit-limit > 0,"non-zero credit limit").
END.
