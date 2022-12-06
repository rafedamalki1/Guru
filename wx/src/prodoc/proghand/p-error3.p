/* p-error3.p */

DEFINE VARIABLE do-lookup AS LOGICAL.

REPEAT WITH 1 COLUMN 1 DOWN:
  UPDATE do-lookup LABEL "Do you want to look up a customer?"
     WITH FRAME ask-frame.
     
  IF do-lookup
  THEN DO:
     DO ON ERROR UNDO, RETRY:
        PROMPT-FOR customer.cust-num.
        FIND customer USING cust-num.
        DISPLAY name address city state postal-code credit-limit.
     END.
  END.   
  ELSE LEAVE.     
END.
