/* p-but2.p */

DEFINE BUTTON quit-button LABEL "Quit".
DEFINE VARIABLE image-file AS CHARACTER FORMAT "x(60)" LABEL "Image File".
DEFINE VARIABLE status-ok AS LOGICAL.

FORM
   image-file SKIP
   quit-button SKIP(13)
   WITH FRAME choices-frame SIDE-LABELS.
   
ON GO OF image-file OR RETURN OF image-file
   DO:
      ASSIGN image-file.
      status-ok = quit-button:LOAD-IMAGE-UP(image-file).
      IF status-ok = NO
      THEN MESSAGE "Cannot load this image." VIEW-AS ALERT-BOX
                                             MESSAGE BUTTONS OK.
      STATUS INPUT "Enter another filename or press the button to quit.".
   END.

ENABLE image-file quit-button WITH FRAME choices-frame.
STATUS INPUT "Enter filename of image for Quit button.".

WAIT-FOR CHOOSE OF quit-button OR WINDOW-CLOSE OF CURRENT-WINDOW.
