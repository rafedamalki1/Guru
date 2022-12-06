/* r-accum.p */

FOR EACH order:
    DISPLAY order-num cust-num order-date promise-date ship-date.
    FOR EACH order-line OF order:
	DISPLAY line-num item-num qty price.
	DISPLAY qty * price LABEL "Ext Price".
	ACCUMULATE qty * price (TOTAL).
	DISPLAY (ACCUM TOTAL qty * price) LABEL "Accum Total".
    END.
    DISPLAY (ACCUM TOTAL qty * order-line.price) LABEL "Total".
END.
DISPLAY (ACCUM TOTAL qty * order-line.price) LABEL "Grand Total"
   WITH ROW 1.
