/* p-chgin2.p */

DEFINE VARIABLE cust-num-var LIKE customer.cust-num.
DEFINE VARIABLE name-var LIKE customer.name.
DEFINE VARIABLE sales-rep-var LIKE customer.sales-rep.
DEFINE VARIABLE answer AS LOGICAL.

DISPLAY "The customers in the data file are:  " WITH NO-BOX.

INPUT FROM p-datfl.d.

REPEAT WITH 10 DOWN COLUMN 38:
    SET cust-num-var name-var sales-rep-var WITH NO-BOX.
END.

INPUT FROM TERMINAL.

SET answer
    LABEL "Do you want to create database records for these customers?"
    WITH SIDE-LABELS NO-BOX FRAME ans-frame.
IF answer
THEN DO:
    DISPLAY "Creating records for..." WITH FRAME ans-frame.
    INPUT FROM p-datfl.d.
    REPEAT:
	CREATE customer.
	SET cust-num name sales-rep WITH NO-BOX COLUMN 28.
    END.
END.
