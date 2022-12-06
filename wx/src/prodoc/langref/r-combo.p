/* r-combo.p */

DEFINE VARIABLE hist-date AS DATE FORMAT "99/99/9999"
        VIEW-AS COMBO-BOX 
	LIST-ITEMS 07/04/1776, 07/11/1969, 09/10/1993.
DEFINE VARIABLE hist-event AS CHARACTER INITIAL
 "Declaration of Independence,Man walks on moon,PROGRESS Version 7 ships".        
DEFINE VARIABLE out-string AS CHARACTER FORMAT "x(36)".

DEFINE FRAME main-frame
   hist-date out-string
   WITH NO-LABELS TITLE "Historic Events".

ON VALUE-CHANGED OF hist-date
   DO:
      out-string = ENTRY(SELF:LOOKUP(SELF:SCREEN-VALUE), hist-event).
      DISPLAY out-string WITH FRAME main-frame.
   END.

ENABLE hist-date WITH FRAME main-frame.

APPLY "VALUE-CHANGED" TO hist-date IN FRAME main-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
