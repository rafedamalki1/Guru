/* r-frline.p */

DEFINE VARIABLE ans AS LOGICAL LABEL "Do you want to delete this customer ?".

IF KBLABEL("GET") = "GET"
THEN ON F3 GET.

STATUS INPUT "Enter data, or use the " + KBLABEL("get")
	     + " key to delete the customer".
get-cust:
  FOR EACH customer WITH 10 DOWN:
    UPDATE cust-num name credit-limit
    editing:
      READKEY.
      IF KEYFUNCTION(lastkey) = "get"
      THEN DO:
	UPDATE ans WITH ROW FRAME-ROW + 3 + FRAME-LINE + 5
	  COLUMN 10 SIDE-LABELS OVERLAY FRAME del-frame.
	IF ans
	THEN DO:
	  DELETE customer.
	  NEXT get-cust.
	END.
      END.
      APPLY LASTKEY.
   END.
 END.
