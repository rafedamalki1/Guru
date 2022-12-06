/* p-select.p */

DEFINE STREAM dirlist.
DEFINE VARIABLE ok-status AS LOGICAL.
DEFINE VARIABLE f-name AS CHARACTER FORMAT "x(14)".
DEFINE VARIABLE list_contents AS CHARACTER FORMAT "x(200)".
DEFINE VARIABLE dir AS CHARACTER FORMAT "x(40)"
          LABEL "Please enter a directory pathname ".
DEFINE VARIABLE sl AS CHARACTER VIEW-AS SELECTION-LIST
          INNER-CHARS 15 INNER-LINES 10 SORT SCROLLBAR-VERTICAL.
FORM
   sl
   WITH FRAME sel-frame NO-LABELS
        TITLE "Please Select a File" WIDTH 50.

FORM
   Readable AS LOGICAL Writable AS LOGICAL
   WITH FRAME file-status SIDE-LABELS TITLE "".
   
FORM
   dir
   WITH FRAME dir-frame SIDE-LABELS.

ON GO, MOUSE-SELECT-DBLCLICK OF dir
   DO:
      ASSIGN dir.
      RUN build-list.
   END.
   
ON DEFAULT-ACTION OF sl
   DO:
       FILE-INFO:FILENAME = dir + "/" + SELF:SCREEN-VALUE.
       IF INDEX(FILE-INFO:FILE-TYPE, "D") > 0
       THEN DO:
          dir = FILE-INFO:PATHNAME.
          RUN build-list.
          DISPLAY dir WITH FRAME dir-frame.
          HIDE FRAME file-status.
       END.
       ELSE DO:       
          ASSIGN Readable = (INDEX(FILE-INFO:FILE-TYPE, "R") > 0)
                 Writable = (INDEX(FILE-INFO:FILE-TYPE, "W") > 0)
              
          FRAME file-status:TITLE = SELF:SCREEN-VALUE.
          DISPLAY Readable Writable WITH FRAME file-status.
       END.
   END.

dir = OS-GETENV("DLC").
DISPLAY dir WITH FRAME dir-frame.
RUN build-list.

ENABLE dir WITH FRAME dir-frame.

ENABLE sl WITH FRAME sel-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.

PROCEDURE build-list:
   ok-status = SESSION:SET-WAIT-STATE("General").

   INPUT STREAM dirlist FROM OS-DIR (dir).

   IMPORT STREAM dirlist f-name.
   list_contents = f-name.
   REPEAT:
      IMPORT STREAM dirlist f-name.
      list_contents = list_contents + "," + f-name.
   END.
   INPUT CLOSE.

   sl:LIST-ITEMS IN FRAME sel-frame = list_contents.
   ok-status = SESSION:SET-WAIT-STATE("").
END PROCEDURE.
