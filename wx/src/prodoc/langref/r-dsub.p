/* r-dsub.p */

DISPLAY "ORDERS SCHEDULED TO SHIP MORE THAN ONE WEEK LATE".
FOR EACH order WHERE ship-date = ?:
    IF (TODAY - 7) > promise-date
    THEN DISPLAY order.order-num order.cust-num promise-date
	(TODAY - promise-date) LABEL "Days Late".
END.
