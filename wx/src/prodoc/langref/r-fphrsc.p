/* r-fphrsc.p */

FOR EACH customer WHERE cust-num <= 50:
   DISPLAY cust-num name credit-limit WITH SCROLL 1 USE-TEXT.
   IF credit-limit >= 50000
   THEN COLOR DISPLAY MESSAGES credit-limit.
END.
