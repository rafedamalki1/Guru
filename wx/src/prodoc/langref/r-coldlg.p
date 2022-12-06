/* r-coldlg.p */

DEFINE VARIABLE front-color  AS INTEGER INITIAL 9.
DEFINE VARIABLE back-color   AS INTEGER INITIAL 8.
DEFINE VARIABLE curr-color   AS INTEGER INITIAL 9
              VIEW-AS RADIO-SET
                  RADIO-BUTTONS "Foreground", 9,
                                "Background", 8.
DEFINE BUTTON ok-button     LABEL "OK".
DEFINE BUTTON cancel-button LABEL "Cancel" AUTO-ENDKEY.

FORM
    SKIP(0.5) SPACE(0.5)
    curr-color SPACE(2) ok-button SPACE(2) cancel-button
    SPACE(0.5) SKIP(0.5)
WITH FRAME color-frame NO-LABELS
    TITLE "Choose frame colors ..."
    FGCOLOR front-color
    BGCOLOR back-color
    VIEW-AS DIALOG-BOX.

ON CHOOSE OF ok-button IN FRAME color-frame
DO:
   ASSIGN curr-color.
   IF NOT COLOR-TABLE:GET-DYNAMIC(curr-color) AND
      NOT COLOR-TABLE:SET-DYNAMIC(curr-color,TRUE)
   THEN MESSAGE "Color must be DYNAMIC to edit.".
   ELSE SYSTEM-DIALOG COLOR curr-color.
END.

UPDATE curr-color ok-button cancel-button WITH FRAME color-frame.
