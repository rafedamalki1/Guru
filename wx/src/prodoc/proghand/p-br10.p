/* p-br10.p */

DEFINE QUERY q1 FOR customer SCROLLING.
DEFINE BROWSE b1 QUERY q1 DISPLAY cust-num name credit-limit
    ENABLE name credit-limit
    WITH 10 DOWN SEPARATORS.

DEFINE VARIABLE method-return AS LOGICAL.
DEFINE VARIABLE new-cust AS INTEGER.
DEFINE BUTTON b-add LABEL "Add Row" SIZE 30 BY 1.

DEFINE FRAME f1
    b1 SKIP(.5)
    b-add TO 38
        WITH NO-BOX SIDE-LABELS ROW 2 CENTERED.

ON ROW-LEAVE OF b1 IN FRAME f1
DO:
    /* Row-leave triggers only if there is more than one
       row in the browse already. */
    IF b1:NEW-ROW THEN DO:
        MESSAGE "New Row!" VIEW-AS ALERT-BOX.
        CREATE customer.
        ASSIGN INPUT BROWSE b1 name credit-limit.
        DISPLAY cust-num WITH BROWSE b1.
    END.
    method-return = b1:CREATE-RESULT-LIST-ENTRY().
END.

ON CHOOSE OF b-add IN FRAME f1 /* Add */
DO:
    IF AVAILABLE(customer) THEN DO:
        /* There is at least one row in the browse already. */
        method-return = b1:INSERT-ROW().
        END.
    ELSE DO:
        /* There are no rows in the browse yet. */
        CREATE customer.
        cust-num = CURRENT-VALUE(next-cust-num).
        new-cust = cust-num.
        /* Open query will refresh all informaton for the
           new row in the browse, including credit-limit. */
        OPEN QUERY q1 FOR EACH CUSTOMER
            WHERE cust-num = new-cust.
        DISPLAY cust-num WITH BROWSE b1.
        APPLY "ENTRY" to browse b1.
        END.
END.

/* The following open query statement will result in an
   empty browse, since no customer names begin with "XYZ". */
OPEN QUERY q1 FOR EACH customer where name begins "XYZ" NO-LOCK.

ENABLE b1 b-add WITH FRAME f1 .
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.


