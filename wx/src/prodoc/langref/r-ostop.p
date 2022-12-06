FOR EACH customer ON STOP UNDO, RETRY: 

   DISPLAY cust-num name credit-limit.
   UPDATE credit-limit.
   IF credit-limit > 100000
   THEN STOP.
END.
