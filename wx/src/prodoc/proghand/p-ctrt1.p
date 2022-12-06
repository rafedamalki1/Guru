/* p-ctrt1.p */

DEFINE VARIABLE iter AS INTEGER.
DEFINE FRAME alpha Customer.Name Customer.Phone.

iter = 10.
MESSAGE "iter =" iter VIEW-AS ALERT-BOX.
FOR EACH Customer WITH FRAME alpha iter DOWN:
    iter = 5.
    DISPLAY name phone balance WITH TITLE "Customer Balances".
END.
MESSAGE "iter =" iter VIEW-AS ALERT-BOX.
