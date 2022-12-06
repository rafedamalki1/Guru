/* p-form1.p */

FORM customer.name contact AT 40 SKIP
     customer.address credit-limit AT 40 SKIP
     customer.city customer.state NO-LABEL
     customer.postal-code NO-LABEL balance AT 40 SKIP(1)
     phone
     HEADER "Customer Maintenance" AT 25 SKIP(1)
     WITH SIDE-LABELS NO-UNDERLINE FRAME a.

FOR EACH customer WITH FRAME a:
    DISPLAY balance.
    UPDATE name address city state postal-code phone contact credit-limit.
END.
