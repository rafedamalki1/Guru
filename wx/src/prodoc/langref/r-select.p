
DEF STREAM dirlist.
DEF VARIABLE f-name AS CHARACTER FORMAT "x(14)".
DEFINE VARIABLE choice as CHARACTER FORMAT "x(50)"
          LABEL "You have selected".
DEFINE VARIABLE list_contents AS CHARACTER FORMAT "x(200)".
DEFINE VARIABLE dir AS CHARACTER FORMAT "x(40)"
          LABEL "Please enter a directory pathname ".
DEFINE VARIABLE sl AS CHARACTER VIEW-AS SELECTION-LIST
          INNER-CHARS 15 INNER-LINES 10 SORT.
DEFINE FRAME b sl.
   
UPDATE dir WITH FRAME d WITH SIDE-LABELS.
INPUT STREAM dirlist FROM OS-DIR (dir).

IMPORT STREAM dirlist f-name.
list_contents = f-name.
REPEAT:
   IMPORT STREAM dirlist f-name.
   list_contents = list_contents + "," + f-name.
END.
INPUT CLOSE.

sl:LIST-ITEMS = list_contents.

UPDATE sl WITH FRAME b NO-LABELS TITLE "Please Select a File" WIDTH 50.
      
choice = sl:screen-value.   
      
DISPLAY choice WITH FRAME c SIDE-LABELS.
