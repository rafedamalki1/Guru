/* r-thispr.p */

DEFINE QUERY custq FOR customer.
DEFINE BROWSE custb QUERY custq
    DISPLAY name balance phone WITH 10 DOWN.
DEFINE BUTTON bName LABEL "Query on Name".
DEFINE BUTTON bBalance LABEL "Query on Balance".
DEFINE BUTTON bCancel LABEL "Cancel".

DEFINE FRAME CustFrame custb SKIP
    bName bBalance bCancel.
    
DEFINE VARIABLE custwin AS WIDGET-HANDLE.

ON CHOOSE OF bName IN FRAME CustFrame DO:
    custwin:TITLE = "Customers by Name".
    OPEN QUERY custq FOR EACH customer BY name.
END.        

ON CHOOSE OF bBalance IN FRAME CustFrame DO:
    custwin:TITLE = "Customers by Balance".
    OPEN QUERY custq FOR EACH customer BY balance DESCENDING.
END.  

IF THIS-PROCEDURE:PERSISTENT THEN DO:
    THIS-PROCEDURE:PRIVATE-DATA = "Customer Browse".
    CREATE WIDGET-POOL.
END.

CREATE WINDOW custwin
    ASSIGN
        TITLE = "Customer Browser"
        SCROLL-BARS = FALSE
        MAX-HEIGHT-CHARS = FRAME CustFrame:HEIGHT-CHARS
        MAX-WIDTH-CHARS = FRAME CustFrame:WIDTH-CHARS.
        
THIS-PROCEDURE:CURRENT-WINDOW = custwin.

ENABLE ALL WITH FRAME CustFrame.

IF THIS-PROCEDURE:PERSISTENT THEN DO:
    ON CHOOSE OF bCancel IN FRAME CustFrame DO:
        RUN destroy-query.
    END.
END.
ELSE DO: 
    WAIT-FOR CHOOSE OF bCancel IN FRAME CustFrame.
END.

PROCEDURE destroy-query:
    DELETE PROCEDURE THIS-PROCEDURE.
    DELETE WIDGET-POOL.
END.




     



