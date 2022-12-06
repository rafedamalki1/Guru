/* p-adcus2.p */

IF NOT CAN-DO("manager,salesrep")
THEN DO:
  MESSAGE "You are not authorized to run this procedure.".
  RETURN.
END.

REPEAT:
  INSERT customer WITH 2 COLUMNS.
END.
