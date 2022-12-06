/* p-lock13.p */

FORM customer.name customer.balance WITH FRAME upd.

ON GO OF FRAME upd DO:
    DO TRANSACTION:
        FIND CURRENT customer EXCLUSIVE-LOCK.
        IF CURRENT-CHANGED customer THEN DO:
            MESSAGE "This record has been changed by another user"
                    SKIP
                    "Please re-enter your changes."
                    VIEW-AS ALERT-BOX.
            DISPLAY customer.name customer.balance with frame upd.
            RETURN NO-APPLY. 
        END.
        ASSIGN customer.name customer.balance.
    END.
    FIND CURRENT customer NO-LOCK.
END.

FIND FIRST customer NO-LOCK.
DISPLAY customer.name customer.balance WITH FRAME upd.
DO ON ENDKEY UNDO, LEAVE:
    ENABLE customer.name customer.balance WITH FRAME upd.
    WAIT-FOR "GO" OF FRAME upd.
END.

