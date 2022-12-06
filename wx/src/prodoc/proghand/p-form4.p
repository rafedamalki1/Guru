/* p-form4.p */

DEFINE FRAME a
     name contact AT 40 SKIP
     address credit-limit AT 40 SKIP
     city customer.state NO-LABEL SKIP
     postal-code AT COLUMN-OF customer.state ROW-OF city + 1  NO-LABEL
     SKIP
     phone balance AT COLUMN 40 ROW-OF city SKIP(1)
     WITH SIDE-LABELS TITLE "Customer Maintenance".

ASSIGN customer.state:WIDTH-CHARS IN FRAME a = postal-code:WIDTH-CHARS.

FOR EACH customer WITH FRAME a:
        DISPLAY balance.
        UPDATE name address city state postal-code phone
               contact credit-limit.
END.

