/* p-delcs2.p */

DO FOR permission:
  FIND permission "p-delcs.p" NO-LOCK.
  IF NOT CAN-DO(can-run)
  THEN DO:
    MESSAGE "You are not authorized to run this procedure.".
    RETURN.
  END.
END.

PROMPT-FOR customer.cust-num.
FIND customer USING cust-num.
DELETE customer.
