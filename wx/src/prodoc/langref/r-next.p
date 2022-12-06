/* r-next.p */

PROMPT-FOR customer.sales-rep
    LABEL "Enter salesman initials"
    WITH SIDE-LABELS CENTERED.

FOR EACH customer:
    IF sales-rep <> INPUT sales-rep THEN NEXT.
    DISPLAY cust-num name city st WITH CENTERED USE-TEXT.
END.
