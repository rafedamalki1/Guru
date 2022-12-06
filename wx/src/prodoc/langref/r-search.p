/* r-search.p */

DEFINE VARIABLE fullname AS CHARACTER FORMAT "x(55)".
DEFINE VARIABLE filename AS CHARACTER FORMAT "x(20)".

REPEAT:
   UPDATE filename HELP "Try entering 'help.r' or 'dict.r'"
       WITH FRAME a SIDE-LABELS CENTERED.
   fullname = SEARCH(filename).
   IF fullname = ?
   THEN
       DISPLAY "UNABLE TO FIND FILE " filename
	   WITH FRAME b ROW 6 CENTERED NO-LABELS.
   ELSE
       DISPLAY "Fully Qualified Path Name Of:" filename
	   SKIP(2) "is:"
	   fullname WITH FRAME c ROW 6 NO-LABELS CENTERED.
END.
