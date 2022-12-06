/* p-frm5.p */

DISPLAY "Daily Report" WITH FRAME p-frame CENTERED.

FOR EACH customer:
    DISPLAY cust-num name.
    DISPLAY phone WITH FRAME aaa.
    FOR EACH order OF customer WITH FRAME bbb:
        DISPLAY order-num order-date.
        FOR EACH order-line OF order:
            FIND item OF order-line.
            DISPLAY item-name WITH FRAME bbb.
            DOWN WITH FRAME bbb.
        END.
    END.
END.
