DEFINE VARIABLE i     AS INTEGER.
DEFINE VARIABLE plist AS CHARACTER FORMAT "x(70)".

FORM
  plist
  WITH FRAME what-prog OVERLAY ROW 10 CENTERED 5 DOWN NO-LABELS
     TITLE " Program Trace ".

i = 2. /* Skip the current routine: PROGRAM-NAME(1) */
DO WHILE PROGRAM-NAME(i) <> ?:
   IF i = 2
   THEN plist = "Currently in       : " + PROGRAM-NAME(i).
   ELSE plist = "Which was called by: " + PROGRAM-NAME(i).

   i = i + 1.
   DISPLAY plist WITH FRAME what-prog.
   DOWN WITH FRAME what-prog.
END.

PAUSE.

HIDE FRAME what-prog.
