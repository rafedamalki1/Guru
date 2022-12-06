/* r-bkcol.p */

FOR EACH customer:
  DISPLAY cust-num name credit-limit WITH 10 DOWN FRAME cust
	  CENTERED TITLE "Customer Credit Information".
  IF credit-limit >= 75000
  THEN DO:
    UPDATE credit-limit LABEL "Credit limit too high.  Please change"
	   WITH SIDE-LABELS FRAME x COLOR MESSAGES
	   PROMPT MESSAGES ROW 17 CENTERED.
    DISPLAY credit-limit WITH FRAME cust.
  END.
END.
