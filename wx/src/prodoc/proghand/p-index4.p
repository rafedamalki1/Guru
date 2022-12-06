/* p-index4.p */

REPEAT:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num.
    FOR EACH order OF customer:
	DISPLAY order-num terms.
	FOR EACH order-line OF order:
	    DISPLAY line-num qty.
	    FIND item OF order-line.
	    DISPLAY idesc on-hand alloc.
	END.
    END.
END.
