/* p-fldls2.p */

REPEAT PRESELECT EACH customer FIELDS (name cust-num balance):
    FIND NEXT customer.
    DISPLAY name cust-num balance.
END.
