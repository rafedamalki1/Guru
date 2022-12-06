/* p-frm6.p */

DISPLAY "Daily Report".

FOR EACH customer:
    DISPLAY name.
    DISPLAY cust-num.
    FOR EACH order OF customer:
        DISPLAY order-date.
        FOR EACH order-line OF order:
            FIND item OF order-line.
            DISPLAY item-name.
        END.
    END.
END.
