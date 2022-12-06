/* r-othru2.p */

OUTPUT THROUGH crypt mypass >ecust.

FOR EACH customer WHERE cust-num < 10:
    DISPLAY name WITH NO-LABELS NO-BOX.
END.

OUTPUT CLOSE.

UNIX crypt mypass <ecust.
