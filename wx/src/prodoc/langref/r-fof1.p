/* r-fof1.p */

DEFINE QUERY custq FOR customer.
DEFINE BUTTON bprev LABEL "<".
DEFINE BUTTON bnext LABEL ">".
DEFINE FRAME cust-fr SKIP(.5)
    SPACE(8) customer.name customer.cust-num customer.sales-rep
    customer.comments AT COLUMN 6 ROW 13.5
    WITH SIDE-LABELS TITLE "Customer Data" 
        SIZE 80 BY 15 VIEW-AS DIALOG-BOX.
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
    SPACE(4) bprev bnext
    WITH TITLE "PREVIOUS/NEXT"
        SIZE 15 BY 2 AT COLUMN 53 ROW 10.5.
DEFINE FRAME acct-fr SKIP(.5)
    customer.balance COLON 15 SKIP 
    customer.credit-limit COLON 15 SKIP
    customer.discount COLON 15 SKIP 
    customer.terms COLON 15    
    WITH SIDE-LABELS TITLE "Account Information"
        SIZE 38.85 BY 6 AT COLUMN 41 ROW 3.

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

FRAME cont-fr:FRAME = FRAME cust-fr:HANDLE.
FRAME acct-fr:FRAME = FRAME cust-fr:HANDLE.
FRAME ctrl-fr:FRAME = FRAME cust-fr:HANDLE.

OPEN QUERY custq PRESELECT EACH customer BY customer.name.
GET FIRST custq.
RUN display-proc IN THIS-PROCEDURE.
ENABLE ALL WITH FRAME ctrl-fr.

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
