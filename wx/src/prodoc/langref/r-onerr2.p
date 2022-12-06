/* r-onerr2.p */

ord:
  REPEAT:
    PROMPT-FOR order.order-num.
    FIND order USING order-num.
    DISPLAY order-date ship-date.
    FOR EACH order-line OF order ON ERROR UNDO ord, NEXT ord:
	FIND item OF order-line.
	alloc = alloc - qty.
	on-hand = on-hand - qty.
	if on-hand < 0 then undo ord, next ord.
    END.
    MESSAGE "Order quantities processed".
  END.
