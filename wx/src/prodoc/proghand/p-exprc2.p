/* p-exprc2.p */

DEFINE VARIABLE vcust-num LIKE customer.cust-num INITIAL 0.
DEFINE VARIABLE vname LIKE customer.name.

DO WHILE NOT vcust-num = ?:
    SET vcust-num LABEL "Customer Number" WITH SIDE-LABELS.
    IF NOT vcust-num = ? THEN DO:
        RUN p-exprc1.p (INPUT vcust-num, OUTPUT vname).
        MESSAGE "Customer" vcust-num "is" vname + ".".
    END.
END.
