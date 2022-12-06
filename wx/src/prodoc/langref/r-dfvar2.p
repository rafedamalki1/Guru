/* r-dfvar2.p */

DEFINE SHARED VARIABLE del AS LOGICAL.
DEFINE SHARED VARIABLE nrecs AS INTEGER.

OUTPUT TO PRINTER.
nrecs = 0.
FOR EACH order WHERE ship-date <> ?:
    nrecs = nrecs + 1.
    FOR EACH order-line OF order:
	DISPLAY order-num line-num qty price.
	IF del THEN DELETE order-line.
    END.
    IF del THEN DELETE order.
END.
OUTPUT CLOSE.
