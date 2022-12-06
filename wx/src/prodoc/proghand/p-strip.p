/* p-strip.p */

DEFINE VARIABLE abc AS character EXTENT 3
 INITIAL ["Add", "Update", "Delete"].

DISPLAY abc NO-LABELS WITH ROW 8 CENTERED
  TITLE "Customer Maintenance".
  
CHOOSE FIELD abc AUTO-RETURN.

IF FRAME-VALUE = "add" THEN MESSAGE "Add customer".
IF FRAME-VALUE = "update" THEN MESSAGE "Update customer".
IF FRAME-VALUE = "delete" THEN MESSAGE "Delete customer".

