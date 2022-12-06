/* r-out.p */

OUTPUT TO cust.dat.

FOR EACH customer:
    DISPLAY cust-num name address address2 city state country SKIP(2)
	WITH 1 COLUMN SIDE-LABELS.
END.

OUTPUT CLOSE.
DISPLAY "Finished".
