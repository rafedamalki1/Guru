/* p-txn11.p */

DEFINE VARIABLE answer AS LOGICAL.
DEFINE NEW SHARED VARIABLE cust-num-var AS ROWID.

REPEAT WITH 1 DOWN:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num.
    DISPLAY name sales-rep.
    UPDATE credit-limit balance.
    SET answer LABEL "Do you want to do order processing?"
	WITH FRAME a NO-HIDE.
    IF answer THEN DO:
	cust-num-var = ROWID(customer).
	RUN p-txn11a.p.
    END.
END.

