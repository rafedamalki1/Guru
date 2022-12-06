/* r-scroll.p */

DEFINE VARIABLE ans AS CHARACTER FORMAT "x".

FORM cust.cust-num cust.name credit-limit
   WITH FRAME cust 10 DOWN.

FORM  "1 - scroll up" SKIP
      "2 - scroll from-current up" SKIP
      "3 - scroll down" SKIP
      "4 - scroll from-current down" SKIP
      "5 - scroll from-current "
WITH FRAME instruct TITLE "Instructions:"
  COLUMN 50.

VIEW FRAME cust.

REPEAT WHILE FRAME-LINE(cust) <= FRAME-DOWN(cust):
   FIND NEXT customer.
   DISPLAY cust-num name credit-limit WITH FRAME cust TITLE "Customers".
   DOWN WITH FRAME cust.
END.

UP FRAME-DOWN(cust) / 2 WITH FRAME cust.
VIEW FRAME instruct.

REPEAT WITH FRAME cust:
   CHOOSE ROW cust.name KEYS ans AUTO-RETURN NO-ERROR WITH FRAME cust.
   IF ans = "1" THEN SCROLL UP.
   ELSE IF ans = "2" THEN SCROLL FROM-CURRENT UP.
   ELSE IF ans = "3" THEN SCROLL DOWN.
   ELSE IF ans = "4" THEN SCROLL FROM-CURRENT DOWN.
   ELSE IF ans = "5" THEN SCROLL FROM-CURRENT.
   VIEW FRAME cust.
   ans = "".
END.
