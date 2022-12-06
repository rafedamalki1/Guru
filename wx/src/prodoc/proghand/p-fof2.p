/* p-fof2.p */

DEFINE QUERY custq FOR customer.
DEFINE VARIABLE cnt AS INTEGER.
DEFINE BUTTON bprev LABEL "<".
DEFINE BUTTON bnext LABEL ">".
DEFINE BUTTON bupdate LABEL "Update".
DEFINE BUTTON bcommit LABEL "Commit".
DEFINE FRAME cust-fr SKIP(.5)
    SPACE(8) customer.name customer.cust-num customer.sales-rep
    customer.comments AT COLUMN 6 ROW 13.5
    WITH SIDE-LABELS TITLE "Customer Data" KEEP-TAB-ORDER 
        SIZE 80 BY 15.
DEFINE FRAME cont-fr SKIP(.5)
    customer.address COLON 17 SKIP
    customer.address2 COLON 17 SKIP
    customer.city COLON 17 SKIP
    customer.state COLON 17 SKIP 
    customer.postal-code COLON 17 SKIP
    customer.country COLON 17 SKIP
    customer.contact COLON 17 SKIP 
    customer.phone COLON 17
    WITH SIDE-LABELS TITLE "Contact Informaion" 
        SIZE 40 BY 10 AT COLUMN 1 ROW 3.
DEFINE FRAME ctrl-fr SKIP(.12)
    SPACE(.5) bprev bnext bupdate bcommit
    WITH TITLE "Find and Update Customer" 
        SIZE 27.5 BY 2 AT COLUMN 46 ROW 10.5.
DEFINE FRAME acct-fr SKIP(.5)
    customer.balance COLON 15 SKIP 
    customer.credit-limit COLON 15 SKIP
    customer.discount COLON 15 SKIP 
    customer.terms COLON 15    
    WITH SIDE-LABELS TITLE "Account Information" 
        SIZE 39.8 BY 6 AT COLUMN 41 ROW 3.

ON CHOOSE OF bnext DO:
    GET NEXT custq.
    IF NOT AVAILABLE customer THEN GET FIRST custq.
    RUN display-proc IN THIS-PROCEDURE.
END.

ON CHOOSE OF bprev DO:
    GET PREV custq.
    IF NOT AVAILABLE customer THEN GET LAST custq.
    RUN display-proc IN THIS-PROCEDURE.
END.

ON CHOOSE OF bupdate DO:
    DISABLE ALL WITH FRAME ctrl-fr.
    ENABLE bcommit WITH FRAME ctrl-fr.
    ENABLE ALL EXCEPT customer.cust-num WITH FRAME cust-fr.
    ENABLE ALL WITH FRAME cont-fr.
    ENABLE ALL EXCEPT customer.balance WITH FRAME acct-fr.
END.

ON CHOOSE OF bcommit DO:
    DISABLE ALL WITH FRAME cust-fr.
    DISABLE ALL WITH FRAME cont-fr.
    DISABLE ALL WITH FRAME acct-fr.
    ENABLE ALL WITH FRAME ctrl-fr.
    DISABLE bcommit WITH FRAME ctrl-fr.
    GET CURRENT custq EXCLUSIVE-LOCK.
    IF NOT CURRENT-CHANGED(customer) THEN DO:
        ASSIGN customer EXCEPT customer.cust-num customer.balance.
        RELEASE customer.
    END.
    ELSE DO:
        GET CURRENT custq NO-LOCK.
        RUN display-proc IN THIS-PROCEDURE.
        DO cnt = 1 TO 12: BELL. END.
        MESSAGE "Customer" customer.name SKIP
                "was changed by another user." SKIP
                "Please try again..." VIEW-AS ALERT-BOX.
    END.
END.

FRAME cont-fr:FRAME = FRAME cust-fr:HANDLE.
FRAME acct-fr:FRAME = FRAME cust-fr:HANDLE.
FRAME ctrl-fr:FRAME = FRAME cust-fr:HANDLE.

CURRENT-WINDOW:TITLE = "Update ...".
OPEN QUERY custq PRESELECT EACH customer NO-LOCK BY customer.name.
GET FIRST custq.
RUN display-proc IN THIS-PROCEDURE.
ENABLE ALL WITH FRAME ctrl-fr.
DISABLE bcommit WITH FRAME ctrl-fr.

WAIT-FOR WINDOW-CLOSE OF FRAME cust-fr.

PROCEDURE display-proc:
    DISPLAY 
        customer.name customer.cust-num customer.sales-rep
        customer.comments WITH FRAME cust-fr.
    DISPLAY
        customer.address customer.address2 customer.city customer.state 
        customer.postal-code customer.country customer.contact 
        customer.phone WITH FRAME cont-fr.
    DISPLAY 
        customer.balance customer.credit-limit
        customer.discount customer.terms WITH FRAME acct-fr.
END.
