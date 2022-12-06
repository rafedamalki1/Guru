/* p-borfrm.p */

FORM
   Customer.Name LABEL "Search String"
   WITH FRAME prompt-frame.

FORM
   Customer.Name Customer.Credit-limit
   WITH FRAME upd-frame.

ON NEXT-FRAME OF Customer.Name IN FRAME prompt-frame
   DO:
      FIND Customer USING Customer.Name.
      DISPLAY Customer.Name Customer.Credit-limit WITH FRAME upd-frame.
      ENABLE Customer.Credit-limit WITH FRAME upd-frame.
   END.
  
ON LEAVE OF Customer.Credit-limit
   ASSIGN Customer.Credit-limit.
   
STATUS INPUT "Press " + KBLABEL("NEXT-FRAME") + " to search; " +
             KBLABEL("END-ERROR") + " to exit".
      
ENABLE Customer.Name WITH FRAME prompt-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
