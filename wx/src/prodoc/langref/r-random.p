/* r-random.p */

DEFINE VARIABLE onum AS INTEGER.
DEFINE VARIABLE olnum AS INTEGER.

DO onum = 1 TO 10 TRANSACTION:
    CREATE order.
    order.order-num = onum.
    order.order-date = TODAY.
    DO olnum = 1 TO RANDOM(1,9):
	CREATE order-line.
	order-line.line-num = olnum.
	order-line.item-num = olnum.
    END.
END.
