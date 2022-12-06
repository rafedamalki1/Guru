/* p-test.p */

DEFINE VARIABLE ctr AS INTEGER.

REPEAT ctr = 1 TO 999:
    CREATE customer.
    cust-num = ctr.
    name = "custname" + string(ctr,"999").
END.
