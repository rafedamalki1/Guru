/* r-shrfrm.p */

DEFINE NEW SHARED FRAME cust-frame.
DEFINE NEW SHARED VARIABLE csz
      AS CHARACTER FORMAT "x(29)".
DEFINE NEW SHARED BUFFER xcust FOR customer.

FOR EACH xcust WHERE  cust-num <=20:

      {r-shrfrm.i}  /* include file for layout of shared frame */

DISPLAY name phone address sales-rep
	city + ", " + st + " " + postal-code @ csz
	credit-limit WITH FRAME cust-frame.

RUN r-updord.p.  /* External procedure to update customer's orders */
END.
