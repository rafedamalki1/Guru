/* p-ovrlay.p */

FOR EACH customer:
    DISPLAY customer WITH 2 COLUMNS TITLE "CUSTOMER INFORMATION".
    FOR EACH order OF customer:
        DISPLAY order-num order-date ship-date promise-date carrier 
                instructions WITH 2 COLUMNS 1 DOWN OVERLAY
                TITLE "CUSTOMER'S ORDERS" ROW 7 COLUMN 10.
    END.
END.
