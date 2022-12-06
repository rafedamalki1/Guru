/* r-resrow.p */

DEFINE QUERY cust-query FOR customer SCROLLING.
OPEN QUERY cust-query FOR EACH customer WHERE Country = "USA".

REPEAT:
    GET NEXT cust-query.
    IF QUERY-OFF-END("cust-query") THEN LEAVE.
    DISPLAY CURRENT-RESULT-ROW("cust-query") LABEL "Result Row" 
        Cust-num Name.
END.
