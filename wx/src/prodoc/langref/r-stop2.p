DEFINE VARIABLE ans  AS LOGICAL.

FOR EACH customer ON STOP UNDO, RETRY:
   DISPLAY cust-num name.
   UPDATE credit-limit.
   ans = NO.
   MESSAGE "Stopping now undoes changes to this record."
           "Do you want to stop now?" VIEW-AS ALERT-BOX QUESTION
           BUTTONS YES-NO UPDATE ans.

   IF ans THEN STOP.
END.
