/* p-secadm.p */

FIND permission "security".
IF NOT CAN-DO(can-run)
  THEN DO:
    MESSAGE "You are not authorized to run this procedure.".
    RETURN.
END.

DISPLAY "Please enter one or more security ids"
	"for the security administrator,"
	"separating the ids with commas" SKIP(1).
UPDATE can-run.
