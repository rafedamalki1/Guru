DEFINE VARIABLE db-recid         AS RECID     NO-UNDO.
DEFINE VARIABLE db_count         AS INTEGER   NO-UNDO.
DEFINE VARIABLE db-name          AS CHARACTER NO-UNDO FORMAT "X(32)".
DEFINE VARIABLE file-name        AS CHARACTER NO-UNDO FORMAT "x(32)".
DEFINE VARIABLE odbtyp           AS CHARACTER NO-UNDO. /* list of ODBC-types */

HIDE FRAME ff.
HIDE FRAME ff0.
HIDE FRAME ff1.
HIDE FRAME ff3.

DISPLAY "Change Uniqueness Of Indexes" SKIP WITH FRAME ff0 CENTERED NO-BOX.
ASSIGN
  odbtyp      = {adecomm/ds_type.i
                  &direction = "ODBC"
                  &from-type = "odbtyp"
                  }.

FOR EACH _db:
  IF CAN-DO(odbtyp,_db-type) THEN db-recid = RECID(_db).
  db_count = db_count + 1.
END.
IF db_count < 2 THEN RETURN. 
IF db_count > 2 THEN DO:
  UPDATE db-name WITH FRAME ff CENTERED NO-LABELS
   TITLE "Logical Data Source Name" ROW 4.
  FIND FIRST _db WHERE _db-name = db-name NO-ERROR.
  IF AVAILABLE _db THEN db-recid = RECID(_db).
END.


UPDATE file-name WITH FRAME ff1 CENTERED TITLE
  "Table Name - '*' For ALL " ROW 4 NO-LABELS.


IF file-name  BEGINS "*" THEN DO:
  FOR EACH _file WHERE _file._db-recid = db-recid:
    RUN "prodict/odb/odb_uni.p" (_file-name). END.
END.
ELSE DO:
  HIDE FRAME ff1. DISPLAY file-name WITH FRAME ff3 CENTERED TITLE
  "Table Name" NO-LABELS ROW 4. 
  REPEAT:
  IF file-name = "" THEN UNDO, LEAVE.
  RUN "prodict/odb/odb_uni.p" (file-name).
  file-name = "".
  UPDATE file-name WITH FRAME ff3 CENTERED TITLE
  "Table Name" NO-LABELS ROW 4.
END.    END.
HIDE FRAME ff0.
HIDE FRAME ff1.
HIDE FRAME ff.
HIDE FRAME ff3.
