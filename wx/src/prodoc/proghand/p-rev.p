/* p-rev.p */

DEFINE BUTTON hello-button LABEL "Greeting".
DEFINE BUTTON revert-button LABEL "Revert".


FORM
   hello-button revert-button
   WITH FRAME butt-frame.

ON CHOOSE OF hello-button IN FRAME butt-frame
   DO:
      MESSAGE "Outer message".
   END.

ENABLE hello-button WITH FRAME butt-frame.
RUN inner.

PROCEDURE inner:

   ON CHOOSE OF hello-button IN FRAME butt-frame
      DO: 
         MESSAGE "Inner message".
      END.

   ENABLE revert-button WITH FRAME butt-frame.  
   
   WAIT-FOR CHOOSE OF revert-button.
   
   ON CHOOSE OF hello-button IN FRAME butt-frame
      REVERT.

   DISABLE revert-button WITH FRAME butt-frame.

   WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.

END PROCEDURE.

