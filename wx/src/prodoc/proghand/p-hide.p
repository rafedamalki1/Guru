/* p-hide.p */

FOR EACH customer:
    DISPLAY name WITH FRAME f1 DOWN.
    DISPLAY credit-limit WITH FRAME f2.
    FOR EACH order OF customer:
        DISPLAY order-num.
    END.
END.
