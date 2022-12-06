/* p-frm13.p */

/* This procedure produces flashing */

FORM customer.cust-num customer.name customer.credit-limit
     WITH FRAME cust-frame.

FOR EACH customer:
    DISPLAY cust-num name credit-limit WITH FRAME cust-frame.
END.
