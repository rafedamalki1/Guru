/* r-delwid.p */

DEFINE VARIABLE wh-tmp AS WIDGET-HANDLE.
DEFINE VARIABLE i      AS INTEGER NO-UNDO.

DEFINE BUTTON b_quit LABEL "Quit" AUTO-ENDKEY.
DEFINE BUTTON b_make LABEL "Make Buttons".

DEFINE FRAME butt-frame
  b_make b_quit
  WITH CENTERED ROW 2.
  
DEFINE FRAME new-buttons WITH WIDTH 48 CENTERED TITLE "New Buttons".

FRAME new-buttons:HEIGHT-CHARS  = 3.

ON CHOOSE OF b_make IN FRAME butt-frame
DO:
  DISABLE b_make WITH FRAME butt-frame.
  DO i = 1 TO 10:
    CREATE BUTTON wh-tmp
      ASSIGN FRAME  = FRAME new-buttons:HANDLE
             COLUMN = i * 4
             LABEL  = STRING(i)
             SENSITIVE = TRUE
             VISIBLE = TRUE
      TRIGGERS:
        ON CHOOSE 
          PERSISTENT RUN del-self.
      END.
  END.
END.

ENABLE b_make b_quit WITH FRAME butt-frame.

WAIT-FOR CHOOSE OF b_quit IN FRAME butt-frame.

PROCEDURE del-self:
  IF SELF:PREV-SIBLING = ?  AND SELF:NEXT-SIBLING = ? THEN
  DO:
    HIDE FRAME new-buttons.
    ENABLE b_make WITH FRAME butt-frame.
  END.
  MESSAGE "Deleting Widget, Button" SELF:LABEL.
  DELETE WIDGET SELF.
END.
