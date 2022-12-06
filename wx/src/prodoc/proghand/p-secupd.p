/* p-secupd.p */

DO FOR permission:
  FIND permission "security" NO-LOCK.
  IF NOT CAN-DO(can-run)
  THEN DO:
    MESSAGE "You are not authorized to run this procedure.".
    RETURN.
  END.
END.

REPEAT FOR permission:
  PROMPT-FOR activity.
  FIND permission USING activity.
  UPDATE can-run.
END.
