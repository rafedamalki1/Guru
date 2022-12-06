/* r-recph.p */

FOR EACH customer FIELDS (cust-num name credit-limit) 
        WHERE credit-limit GE 50000, 
    EACH order FIELDS (order-num order-date terms) OF customer:
    DISPLAY customer.cust-num customer.name credit-limit
	    order.order-num order-date order.terms.
END.
