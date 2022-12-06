/* p-adcus4.p */

DO FOR permission:
  FIND permission "p-adcust.p" NO-LOCK.
  IF NOT CAN-DO(can-run)
  THEN DO:
    MESSAGE "You are not authorized to run this procedure.".
    RETURN.
  END.
END.

REPEAT:
  INSERT customer WITH 2 COLUMNS.
END.
