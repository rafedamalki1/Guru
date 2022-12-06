/* p-exprc1.p */

DEFINE INPUT PARAMETER pcust-num LIKE customer.cust-num.
DEFINE OUTPUT PARAMETER pname LIKE customer.name INITIAL ?.

FOR EACH customer:
    IF customer.cust-num = pcust-num THEN DO:
        pname = customer.name.
        RETURN.
    END.
END.
