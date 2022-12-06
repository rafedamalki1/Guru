REPEAT:
    CREATE customer.
    UPDATE cust-num name
	sales-rep VALIDATE(CAN-FIND(salesrep WHERE salesrep.sales-rep =
	customer.sales-rep),"Invalid sales rep -- please re-enter").
END.
