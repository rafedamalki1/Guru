/* p-frline.p */

DEFINE VARIABLE ans AS LOGICAL LABEL
        "Do you want to delete this customer?".

DEFINE FRAME a customer.cust-num customer.name customer.credit-limit.

STATUS INPUT "Enter data, or press CTRL-G to delete the customer".

ON CTRL-G OF customer.cust-num, customer.name, credit-limit
   DO:
      UPDATE ans WITH ROW FRAME-ROW(a) + 2 + FRAME-LINE(a) COLUMN 10
         SIDE-LABELS OVERLAY FRAME del-frame.
      IF ans THEN DELETE customer.
      HIDE FRAME del-frame.
   END.
        

REPEAT WITH FRAME a 10 DOWN:
    FIND NEXT cust.
    UPDATE cust-num name credit-limit.
END.

