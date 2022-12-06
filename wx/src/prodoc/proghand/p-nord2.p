/* p-nord2.p */

RUN istrans.p "procedure".

ordblock:
REPEAT:
    RUN istrans.p "ordblock".
    CREATE order.
    UPDATE order-num cust-num odate.

    olblock:
    REPEAT:
	RUN istrans.p "olblock".
	CREATE order-line.
	order-line.order-num = order.order-num.
	SET line-num qty item-num price.
    END.
END.
