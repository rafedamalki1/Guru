/* r-brows2.p */

DEFINE QUERY q1 FOR customer SCROLLING.

DEFINE BROWSE b1 QUERY q1 NO-LOCK DISPLAY cust-num name phone
    ENABLE name phone WITH 15 DOWN NO-ASSIGN SEPARATORS.
     
DEFINE VARIABLE method-return AS LOGICAL NO-UNDO.     
     
DEFINE BUTTON button1 LABEL "New Row".     

DEFINE FRAME f1 
    SKIP(1)
    SPACE(8) b1 SKIP(1)
    SPACE(8) button1
        WITH NO-BOX.

ON ROW-LEAVE OF b1 IN FRAME f1 /* No-Assign Browser */
DO:
    /* If new row, create record and assign values in browse. */
    IF b1:NEW-ROW THEN
    DO:
        CREATE CUSTOMER.
        ASSIGN INPUT BROWSE b1 name phone.
        DISPLAY cust-num WITH BROWSE b1.

        /* Add an entry to the result list */
        method-return = b1:CREATE-RESULT-LIST-ENTRY().
        RETURN.
    END.

    /* If record exists and was changed in browse, update record. */
    IF BROWSE b1:CURRENT-ROW-MODIFIED then
    DO:
        GET CURRENT q1 EXCLUSIVE-LOCK.

    /* Check if data in viewport is different from database. */
    IF CURRENT-CHANGED customer THEN
    DO:
        MESSAGE "This record has been changed by another user."
           SKIP "Please re-enter your changes.".
        DISPLAY cust-num name phone WITH BROWSE b1.
        RETURN NO-APPLY.
    END.
    ELSE /* Record is the same, so update record with exclusive-lock */
        ASSIGN INPUT BROWSE b1 name phone.

    /* Downgrade the lock to a no-lock. */
    GET CURRENT q1 NO-LOCK.
    END.
END.

ON CHOOSE OF button1 IN FRAME f1 /* Insert */
DO:
    method-return = b1:INSERT-ROW("AFTER").
END.


OPEN QUERY q1 FOR EACH customer.
ENABLE ALL WITH FRAME f1.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW