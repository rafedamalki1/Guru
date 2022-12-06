/* r-rindex.p */
DEFINE VARIABLE rindx AS INTEGER.
DEFINE VARIABLE source AS CHARACTER FORMAT "x(45)".
DEFINE VARIABLE target AS CHARACTER FORMAT "x(45)".
REPEAT:
    PROMPT-FOR SOURCE
      LABEL "Enter a character string to do pattern matching:"
	WITH FRAME s1 CENTERED.
    PROMPT-FOR target
      LABEL "Enter a pattern to match the string:"
	WITH FRAME s1 CENTERED.
    rindx = R-INDEX(INPUT source, INPUT target).
    IF rindx <> 0 tHEN DO:
       DISPLAY "The target pattern:" INPUT target NO-lABEL
	       " last appears in position" rindx NO-LABEl SKIP
	       WITH FRAME r1 ROW 12 CENTERED.
       DISPLAY " in the source string:" INPUT source NO-LABEL
	       WITH FRAME r1 ROW 12 CENTERED.
    HIDE FRAME r1.
    END.
    IF rindx = 0 THEN DO:
       DISPLAY " The target pattern:" INPUT target NO-LABEL
		" could not be found" SKIP
		WITH FRAME r2 ROW 12 CENTERED.
       DISPLAY " in the source string: " INPUT source NO-LABEL
		WITH FRAME r2 ROW 12 CENTERED.
    HIDE FRAME r2.
    END.
 END.
