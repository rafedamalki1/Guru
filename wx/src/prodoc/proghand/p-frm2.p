/* p-frm2.p */

DEFINE VARIABLE count AS INTEGER.

DISPLAY "Daily Report".

FOR EACH customer:
    count = 0.
    DISPLAY name.
    DISPLAY cust-num.
    FOR EACH order OF customer:
        count = count + 1.
    END.
    DISPLAY count LABEL "Total Orders".
END.
