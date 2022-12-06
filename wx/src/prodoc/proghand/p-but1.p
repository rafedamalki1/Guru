/* p-but1.p */

DEFINE VARIABLE cnt AS INTEGER.
DEFINE BUTTON count-button LABEL "Show Count".
DEFINE BUTTON quit-button LABEL "Quit".

FORM
   count-button
   quit-button
   WITH FRAME button-frame.

FORM
   cnt VIEW-AS TEXT LABEL "Customer count"
   WITH FRAME cnt-frame SIDE-LABELS.

ON CHOOSE OF count-button
  DO:
     HIDE FRAME cnt-frame.
     cnt = 0.
     FOR EACH customer:
         cnt = cnt + 1.
     END.
     DISPLAY cnt WITH FRAME cnt-frame.
  END.

ENABLE count-button quit-button WITH FRAME button-frame.

WAIT-FOR CHOOSE OF quit-button 
      OR WINDOW-CLOSE OF CURRENT-WINDOW.
