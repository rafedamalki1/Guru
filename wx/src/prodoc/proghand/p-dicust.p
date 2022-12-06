/* p-dicust.p */

DEFINE NEW SHARED FRAME cust-frame.
DEFINE NEW SHARED VARIABLE csz AS CHARACTER FORMAT "x(22)".
DEFINE NEW SHARED BUFFER xcust FOR customer.

REPEAT:
   {p-dicust.i}     /* include file for layout of shared frame */

   PROMPT-FOR xcust.cust-num WITH 1 DOWN FRAME cust-frame.
   FIND xcust USING xcust.cust-num NO-ERROR.
   IF NOT AVAILABLE(xcust)
   THEN DO:
      MESSAGE "Customer not found".
      NEXT.
   END.

   DISPLAY
      name phone address sales-rep
      city + ", " + state + " " + STRING(postal-code) @ csz
      credit-limit WITH FRAME cust-frame.

   RUN p-updord.p.    /* External procedure to update customer's orders */

END.
