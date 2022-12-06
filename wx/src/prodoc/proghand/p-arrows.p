/* p-arrows.p */

DEFINE BUTTON next-but LABEL "Next" IMAGE-UP FILE "BTN-DOWN-ARROW"
                      SIZE-CHARS 6 BY 1.
DEFINE BUTTON prev-but LABEL "Prev" IMAGE-UP FILE "BTN-UP-ARROW"
                      SIZE-CHARS 6 BY 1.
DEFINE BUTTON quit-but LABEL "Quit".

DEFINE VARIABLE title-string AS CHARACTER
                        INITIAL "Customer Browser".

FORM
    next-but prev-but quit-but
    WITH FRAME button-frame.

FORM
    customer.cust-num customer.name
    WITH FRAME name-frame.

ON CHOOSE OF next-but
    DO:
        FIND NEXT customer NO-ERROR.
        IF NOT AVAILABLE(customer)
        THEN DO:
             MESSAGE "This is the last customer."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK
               TITLE title-string.
             FIND LAST customer.
        END.
        DISPLAY customer.cust-num customer.name
                WITH FRAME name-frame.
    END.

ON CHOOSE OF prev-but
    DO:
        FIND PREV customer NO-ERROR.
        IF NOT AVAILABLE(customer)
        THEN DO:
             MESSAGE "This is the first customer."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK 
                TITLE title-string.
             FIND FIRST customer.
        END.
        DISPLAY customer.cust-num customer.name
            WITH FRAME name-frame.
    END.

ENABLE next-but prev-but quit-but WITH FRAME button-frame.
FIND FIRST customer NO-LOCK.
DISPLAY customer.cust-num customer.name
                WITH FRAME name-frame.

WAIT-FOR CHOOSE OF quit-but OR WINDOW-CLOSE OF CURRENT-WINDOW.

