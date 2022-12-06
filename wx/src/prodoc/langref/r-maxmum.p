/* r-maxmum.p */

DEFINE VARIABLE cred-lim2 AS DECIMAL
    FORMAT ">>,>>9.99".

FOR EACH customer:
    IF credit-limit < 20000 THEN cred-lim2 = credit-limit + 10000.
    ELSE cred-lim2 = 30000.
    DISPLAY credit-limit cred-lim2
	MAXIMUM(cred-lim2, credit-limit)
	LABEL "Maximum of these two values".
END.
