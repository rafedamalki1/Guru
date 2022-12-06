/* r-round.p */

FOR EACH customer:
    DISPLAY cust-num name credit-limit.
    credit-limit = ROUND( (credit-limit * 1.1) / 100 ,0) * 100.
    PAUSE.
    DISPLAY credit-limit.
END.
