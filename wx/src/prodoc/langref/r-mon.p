/* r-mon.p */

FOR EACH order:
    IF (MONTH(promise-date) < MONTH(TODAY) OR
       YEAR(promise-date) < YEAR(TODAY))
	  AND ship-date = ?
    THEN DISPLAY order-num LABEL "Order Num"
		 po LABEL "P.O. Num"
		 promise-date LABEL "Promised By"
		 order-date LABEL "Ordered" terms
	WITH TITLE "These orders are overdue".
END.
