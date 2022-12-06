/* p-form2.p */

DEFINE FRAME b WITH ROW 7 CENTERED SIDE-LABELS TITLE "Customer Info".

VIEW FRAME b.

REPEAT:
    PROMPT-FOR customer.cust-num WITH FRAME a ROW 1.
    FIND customer USING cust-num NO-LOCK.
    DISPLAY customer EXCEPT comments WITH FRAME b.
END.
