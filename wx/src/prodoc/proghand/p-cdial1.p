/* p-cdial1.p */

DEFINE VARIABLE FrontColor  AS INTEGER INITIAL 16.
DEFINE VARIABLE BackColor   AS INTEGER INITIAL 17.
DEFINE VARIABLE ColorSelect AS INTEGER INITIAL 16 
 VIEW-AS RADIO-SET
  RADIO-BUTTONS "Foreground", 16, "Background", 17
   HORIZONTAL.
DEFINE VARIABLE status-ok   AS LOGICAL.
DEFINE BUTTON bOK           LABEL "OK".
DEFINE BUTTON bCANCEL       LABEL "CANCEL".

IF COLOR-TABLE:NUM-ENTRIES < 18 THEN
    COLOR-TABLE:NUM-ENTRIES = 18.
    
status-ok = COLOR-TABLE:SET-DYNAMIC(16, TRUE).
IF NOT status-ok
THEN DO:
   MESSAGE "Cannot make color 16 dynamic.".
   RETURN.
END.

status-ok = COLOR-TABLE:SET-DYNAMIC(17, TRUE).
IF NOT status-ok
THEN DO:
   MESSAGE "Cannot make color 17 dynamic.".
   RETURN.
END.

COLOR-TABLE:SET-RGB-VALUE(16,RGB-VALUE(0,0,0)).
COLOR-TABLE:SET-RGB-VALUE(17,RGB-VALUE(128,128,128)).

FORM
    SKIP(0.5) SPACE(0.5)
    ColorSelect SPACE(2) bOK SPACE(2) bCANCEL
    SPACE(0.5) SKIP(0.5)
    WITH FRAME fColor TITLE "Choose frame colors ..." 
      FGCOLOR FrontColor
      BGCOLOR BackColor VIEW-AS DIALOG-BOX.

ON CHOOSE OF bOK IN FRAME fColor
   DO: 
      ASSIGN ColorSelect.
      SYSTEM-DIALOG COLOR ColorSelect.
   END.

ON CHOOSE OF bCANCEL IN FRAME fColor
   STOP.

ENABLE ColorSelect bOK bCANCEL WITH FRAME fColor.

WAIT-FOR WINDOW-CLOSE OF FRAME fColor.


