OUTPUT TO custdump2.

FOR EACH customer:
   EXPORT DELIMITER ";" cust-num name credit-limit.
END.

OUTPUT CLOSE.
