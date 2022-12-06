/* r-eq.p */

PROMPT-FOR order.sales-rep WITH SIDE-LABELS CENTERED.

FOR EACH order WHERE sales-rep EQ INPUT sales-rep:
    DISPLAY order-num cust-num order-date promise-date ship-date
       WITH CENTERED.
END.
