/**********  DEFINE QUERY  **********/
DEFINE QUERY Cust-Query FOR Customer.

/**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE Answer AS LOGICAL.
DEFINE BUTTON btn-Prev LABEL "Prev".
DEFINE BUTTON btn-Next LABEL "Next".
DEFINE BUTTON btn-Update LABEL "Update".
DEFINE BUTTON btn-Add LABEL "Add".
DEFINE BUTTON btn-Delete LABEL "Delete".
DEFINE BUTTON btn-Exit LABEL "Exit".
DEFINE BUTTON btn-OK LABEL "OK" AUTO-GO.
DEFINE BUTTON btn-Cancel LABEL "Cancel" AUTO-ENDKEY.

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    SKIP(1) 
    sports.Customer.Cust-Num LABEL "Customer No." COLON 16
    sports.Customer.Name LABEL "Customer Name" COLON 16
    sports.Customer.Contact COLON 16
    sports.Customer.Phone LABEL "Phone No." COLON 16 
    sports.Customer.Sales-Rep LABEL "Rep. Code" COLON 16
    sports.Customer.Credit-Limit LABEL "Credit Limit" COLON 16
    sports.Customer.Balance COLON 16
    sports.Customer.Terms COLON 16
    sports.Customer.Discount COLON 16 SKIP(1)
    btn-Prev TO 8
    btn-Next 
    btn-Update 
    btn-Add
    btn-Delete
    btn-Exit SPACE(2) SKIP(1)
        WITH SIDE-LABELS USE-TEXT CENTERED ROW 2 THREE-D
        TITLE "Database Access Form for the Customer Table".
        
/**********  DEFINE TRIGGERS  **********/     
ON CHOOSE OF btn-Prev 
DO:
    GET PREV Cust-Query.
    IF QUERY-OFF-END("Cust-Query") THEN GET LAST Cust-Query.   
    DISPLAY Customer.Cust-Num Name Contact Phone Sales-Rep Credit-Limit 
        Balance Terms Discount WITH FRAME Frame1.
END.

ON CHOOSE OF btn-Next 
DO:
    GET NEXT Cust-Query.
    IF QUERY-OFF-END("Cust-Query") THEN GET FIRST Cust-Query.
    DISPLAY Customer.Cust-Num Name Contact Phone Sales-Rep Credit-Limit 
        Balance Terms Discount WITH FRAME Frame1.
END.

/**********  MAIN LOGIC  **********/
OPEN QUERY Cust-Query FOR EACH Customer.
GET FIRST Cust-Query.
DISPLAY Customer.Cust-Num Name Contact Phone Sales-Rep Credit-Limit 
    Balance Terms Discount WITH FRAME Frame1.
ENABLE btn-Next btn-Prev btn-Exit WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.
CLOSE QUERY Cust-Query




