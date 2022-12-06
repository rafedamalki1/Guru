/* r-trunc.p */

FOR EACH customer:
    FORM cust-num name credit-limit
	 new-max LIKE credit-limit LABEL "New Credit limit".
    DISPLAY cust-num name credit-limit.
    credit-limit = TRUNCATE( (credit-limit * 2) / 1000 ,0) * 1000.
    IF credit-limit < 15000 THEN credit-limit = 15000.
    DISPLAY credit-limit @ new-max.
END.
