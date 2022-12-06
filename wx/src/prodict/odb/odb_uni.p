DEFINE INPUT PARAM file-name AS CHAR.
DEFINE FRAME xx .
FIND _file WHERE _file-name = file-name NO-ERROR.

IF AVAILABLE _file THEN

FOR EACH _index of _file:
  DISPLAY  _index-name  LABEL "    Name" WITH FRAME xx
    SIDE-LABELS 8 DOWN CENTERED TITLE "Indexes Of " + _file-name ROW 8.
  UPDATE _uniq  LABEL "Unique" WITH SIDE-LABELS FRAME xx  8 DOWN CENTERED
ROW 8.
END.
ELSE
  IF NOT file-name BEGINS "SQL" AND NOT file-name BEGINS "Get" THEN
    MESSAGE "File" file-name "was not Found" . 
HIDE FRAME xx.
