/* r-custin.p */

FOR EACH customer:
    {r-cstord.i &frame-options = "CENTERED ROW 3 NO-LABEL"}.
    UPDATE cust.cust-num name address address2 city state postal-code
	   phone credit-limit WITH FRAME cust-ord.
END.
