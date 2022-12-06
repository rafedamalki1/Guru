/* p-dynfil1.p */

DEFINE BUTTON bCustNumber LABEL "Customer Number".
DEFINE BUTTON bDelete LABEL "Delete Field".
DEFINE VARIABLE fCustHandle AS WIDGET-HANDLE.
DEFINE VARIABLE lCustHandle AS WIDGET-HANDLE.
DEFINE FRAME CustFrame
  SKIP(3)
  SPACE (1) bCustNumber bDelete
  WITH SIZE 40 BY 5 SIDE-LABELS.

ON CHOOSE OF bCustNumber IN FRAME CustFrame
DO:
  IF fCustHandle <> ? THEN
  DO:
    MESSAGE bCustNumber:LABEL "field already exists.".
    RETURN.
  END.
  CREATE TEXT lCustHandle
    ASSIGN
    FRAME = FRAME CustFrame:HANDLE
    DATA-TYPE = "CHARACTER"
    FORMAT = "x(16)"
    SCREEN-VALUE = "Customer Number:"
    ROW = 2
    COLUMN = 2
  .
  CREATE FILL-IN fCustHandle
    ASSIGN
      FRAME = FRAME CustFrame:HANDLE
      DATA-TYPE = "INTEGER"
      FORMAT = ">>>>9"
      SIDE-LABEL-HANDLE = lCustHandle
      ROW = 2
      COLUMN = lCustHandle:COLUMN + lCustHandle:WIDTH-CHARS + 1
      SENSITIVE = TRUE
      VISIBLE = TRUE
    TRIGGERS:
      ON RETURN PERSISTENT RUN SetFieldTrig.
    END TRIGGERS
  .
END.

ON CHOOSE OF bDelete IN FRAME CustFrame
DO:
  IF fCustHandle <> ? THEN
  DO:
    DELETE WIDGET fCustHandle.
    fCustHandle = ?.
    DELETE WIDGET lCustHandle.
  END.
END.

ENABLE ALL WITH FRAME CustFrame.
WAIT-FOR GO OF FRAME CustFrame.

PROCEDURE SetFieldTrig:
  MESSAGE "You entered" lCustHandle:SCREEN-VALUE
    fCustHandle:SCREEN-VALUE.
END PROCEDURE.
