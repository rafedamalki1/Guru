  DEF VAR OKvald        AS LOGICAL.
  DEF VAR filnamn       AS CHAR.
  DEF VAR cFileName     AS CHAR.
  DEF VAR        cPath  AS CHAR.
  DEF VAR        i  AS INTEGER.
  FILE-INFO:FILE-NAME = "WSTART.W".
  DISP SUBSTRING(FILE-INFO:FULL-PATHNAME,1,INDEX(FILE-INFO:FULL-PATHNAME,FILE-INFO:FILE-NAME) - 1) FORMAT "x(40)".
  DISP cPath FORMAT "x(40)".

  SYSTEM-DIALOG GET-FILE filnamn
      TITLE          "Välj den fil som motsvarar kunddata"
      FILTERS        "All Files (*.*)"  "*.*"
      INITIAL-DIR    "a:"
      MUST-EXIST         
      USE-FILENAME
      UPDATE OKvald.
      IF OKvald = TRUE THEN DO:
      END.
