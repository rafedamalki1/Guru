/* r-input.p */

FOR EACH CUSTOMER:
  DISPLAY cust-num name credit-limit LABEL "Current credit limit"
	  WITH FRAME a 1 DOWN ROW 1.
  PROMPT-FOR credit-limit LABEL "New credit limit" WITH SIDE-LABELS
	     NO-BOX ROW 10 FRAME b.
  IF INPUT FRAME b credit-limit <> credit-limit
  THEN DO:
    DISPLAY "Changing max credit of" name SKIP
	    "from" credit-limit "to" INPUT FRAME b credit-limit
	    WITH FRAME c ROW 15 NO-LABELS.
    credit-limit = INPUT FRAME b credit-limit.
  END.
  ELSE DISPLAY "No change in credit limit" WITH FRAME d ROW 15.
END.
