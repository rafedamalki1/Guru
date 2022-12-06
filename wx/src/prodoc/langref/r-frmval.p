/* r-frmval.p */

DEFINE VARIABLE txt AS CHARACTER INITIAL "PROGRESS".
DEFINE VARIABLE tmpdate AS DATE INITIAL TODAY.

IF KBLABEL("GET") = "GET"
THEN ON F3 GET.

STATUS INPUT "Enter data or use the "
	     + KBLABEL("GET")
	     + " key to enter the unknown value (?)".
UPDATE txt tmpdate
EDITING:
  READKEY.
  IF KEYFUNCTION(LASTKEY) = "GET"
  THEN DO:
    FRAME-VALUE = ?.
    NEXT.
  END.
  APPLY LASTKEY.
END.
