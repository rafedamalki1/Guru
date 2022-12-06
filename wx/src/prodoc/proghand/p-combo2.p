/* p-combo2.p */

DEFINE STREAM dirlist.
DEFINE VARIABLE ok-status AS LOGICAL.
DEFINE VARIABLE f-name AS CHARACTER FORMAT "x(14)".
DEFINE VARIABLE list-contents AS CHARACTER FORMAT "x(200)".
DEFINE VARIABLE dir AS CHARACTER FORMAT "x(40)".
DEFINE VARIABLE flcombo AS CHARACTER FORMAT "x(15)"
                VIEW-AS COMBO-BOX INNER-LINES 10 SORT.
FORM
   "Directory Pathname:"  SKIP
   dir AT 3 SKIP
   "Filename:" SKIP
   flcombo  AT 3
   WITH FRAME sel-frame NO-LABELS TITLE "File Selector".

FORM
   Readable AS LOGICAL Writable AS LOGICAL
   WITH FRAME file-status SIDE-LABELS.
   
ON GO, MOUSE-SELECT-DBLCLICK, RETURN OF dir
   DO:
      ASSIGN dir.
      RUN build-list.
   END.

ON VALUE-CHANGED OF flcombo
   DO:
       FILE-INFO:FILENAME = dir + "/" + SELF:SCREEN-VALUE.
       IF INDEX(FILE-INFO:FILE-TYPE, "D") > 0
       THEN DO:
          HIDE FRAME file-status.
          dir = FILE-INFO:PATHNAME.
          RUN build-list.
          DISPLAY dir WITH FRAME sel-frame.
       END.
       ELSE DO:       
          ASSIGN Readable = (INDEX(FILE-INFO:FILE-TYPE, "R") > 0)
                 Writable = (INDEX(FILE-INFO:FILE-TYPE, "W") > 0)
              
          FRAME file-status:TITLE = "Attributes of " + SELF:SCREEN-VALUE.
          DISPLAY Readable Writable WITH FRAME file-status.
       END.
   END.

dir = OS-GETENV("DLC").
DISPLAY dir WITH FRAME sel-frame.

ENABLE dir flcombo WITH FRAME sel-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.

PROCEDURE build-list:
   ok-status = SESSION:SET-WAIT-STATE("General").

   INPUT STREAM dirlist FROM OS-DIR (dir).

   IMPORT STREAM dirlist f-name.
   list-contents = f-name.
   REPEAT:
      IMPORT STREAM dirlist f-name.
      list-contents = list-contents + flcombo:DELIMITER IN FRAME sel-frame
                      + f-name.
   END.
   INPUT CLOSE.

   flcombo:LIST-ITEMS IN FRAME sel-frame = list-contents.
   ok-status = SESSION:SET-WAIT-STATE("").
END PROCEDURE.
