/* r-perprc.p */

DEFINE QUERY custq FOR customer.
DEFINE BROWSE custb QUERY custq
    DISPLAY name balance credit-limit phone WITH 10 DOWN.
DEFINE BUTTON bName LABEL "Query on Name".
DEFINE BUTTON bBalance LABEL "Query on Balance".
DEFINE BUTTON bCredit LABEL "Query on Credit".
DEFINE BUTTON bHide LABEL "Hide Query".
DEFINE BUTTON bCancel LABEL "Cancel".

DEFINE FRAME CustFrame custb SKIP
    bName bBalance bCredit bHide bCancel.
    
DEFINE VARIABLE custwin AS WIDGET-HANDLE.

ON CHOOSE OF bName IN FRAME CustFrame DO:
    custwin:TITLE = "Customers by Name".
    OPEN QUERY custq FOR EACH customer BY name.
END.        

ON CHOOSE OF bBalance IN FRAME CustFrame DO:
    custwin:TITLE = "Customers by Balance".
    OPEN QUERY custq FOR EACH customer BY balance DESCENDING.
END.  

ON CHOOSE OF bCredit IN FRAME CustFrame DO:
    custwin:TITLE = "Customers by Credit".
    OPEN QUERY custq FOR EACH customer BY credit-limit DESCENDING.
END.  

ON VALUE-CHANGED OF BROWSE custb DO:
    IF customer.balance >= (customer.credit-limit * 0.75) THEN DO:
        BELL.
        MESSAGE "Evaluate" customer.name "for credit increase.".
    END.
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
    ON CHOOSE OF bHide IN FRAME CustFrame DO:
        custwin:VISIBLE = FALSE.
    END.
END.
ELSE DO: 
    WAIT-FOR CHOOSE OF bHide, bCancel IN FRAME CustFrame.
END.

PROCEDURE recall-query:
    custwin:VISIBLE = TRUE.
END.

PROCEDURE destroy-query:
    DELETE PROCEDURE THIS-PROCEDURE NO-ERROR.
    DELETE WIDGET-POOL.
END.


     



