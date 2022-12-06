/* r-qoff.p */

OPEN QUERY cust-query FOR EACH customer.

 REPEAT:
   GET NEXT cust-query.
   
   IF QUERY-OFF-END("cust-query")
   THEN LEAVE.
   
   DISPLAY cust-num name.
END.
