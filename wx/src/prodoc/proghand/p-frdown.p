/* p-frdown.p */

DEFINE VARIABLE ans AS LOGICAL.

FOR EACH customer:
  DISPLAY cust-num name credit-limit.
  IF FRAME-LINE = FRAME-DOWN
  THEN DO:
    MESSAGE "Do you want to see the next page?" VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Screen Full"
         UPDATE ans.
    IF NOT ans
    THEN LEAVE.
  END.
END.

