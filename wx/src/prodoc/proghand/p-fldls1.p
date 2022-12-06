/* p-fldls1.p */

DEFINE QUERY custq FOR customer FIELDS (name cust-num balance).
OPEN QUERY custq PRESELECT EACH customer.
REPEAT:
    GET NEXT custq.
    IF AVAILABLE(customer)
        THEN DISPLAY name cust-num balance.
        ELSE LEAVE.
END.
