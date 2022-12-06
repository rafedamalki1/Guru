/* p-var.p */

DEFINE VARIABLE ctr AS INTEGER INITIAL 0.

REPEAT:
    CREATE customer.
    ctr = ctr + 1.
    UPDATE cust-num name WITH NO-BOX.
END.
DISPLAY ctr  "customer records were created"
    WITH NO-BOX NO-LABELS COLUMN 35.
