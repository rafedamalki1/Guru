/* r-frfile.p */

FOR EACH customer, EACH order OF customer:
  DISPLAY order-num WITH CENTERED ROW 2 FRAME onum.
  UPDATE
    customer.cust-num AT 5 order.cust-num AT 30 SKIP
    customer.name  AT 5
    customer.city AT 5
    customer.state AT 5
    customer.postal-code AT 5
    WITH ROW 8 CENTERED 1 DOWN NO-LABELS
      EDITING:
	MESSAGE "     The field" FRAME-FIELD "is from the" FRAME-FILE
		"file     ".
	READKEY.
	APPLY LASTKEY.
      END.  /* Editing */
END.
