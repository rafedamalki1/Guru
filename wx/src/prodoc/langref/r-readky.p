/* r-readky.p */

FOR EACH customer:
    DISPLAY cust-num name address city st WITH 1 DOWN.
    MESSAGE "If you want to delete this customer, press Y".
    MESSAGE "Otherwise, press any other key.".
    READKEY.
    IF CHR(LASTKEY) = "Y" THEN DELETE customer.
    ELSE
    IF KEYFUNCTION(LASTKEY) = "END-ERROR" THEN LEAVE.
END.
