/* r-comm2.p  */

/* step through unshipped orders */
FOR EACH order WHERE ship-date = ?:
    /* display order date, promise date, terms */
    DISPLAY order-date promise-date terms. 

/*
    FOR EACH order-line OF order:
	/* display all order-lines of each order */
	DISPLAY order-line.
    END.
*/

END.
