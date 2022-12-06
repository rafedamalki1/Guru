/* p-itlist.p */

FOR EACH customer WHERE max-credit > 500:
    DISPLAY name address city st zip max-credit.
    FOR EACH order OF customer:
	DISPLAY order-num pdate sdate.
	FOR EACH order-line OF order:
	    FIND item OF order-line.
	    DISPLAY line-num item.item-num idesc qty price.
	END.
    END.
END.
