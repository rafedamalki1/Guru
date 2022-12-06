


/* r-unx.p */

DEFINE VARIABLE proc AS CHARACTER FORMAT "x(40)".

REPEAT:
	DISPLAY "Enter L to list your files"
		WITH ROW 5 CENTERED FRAME a.
	SET proc LABEL "Enter a valid Procedure Name to run"
		WITH ROW 9 CENTERED FRAME b.
	IF proc = "L" THEN
		IF OPSYS = "UNIX" THEN UNIX ls.
		ELSE IF OPSYS = "WIN32" then DOS dir.
		ELSE display "Operating system" OPSYS "is not supported".

  ELSE DO:
		HIDE FRAME a.
		HIDE FRAME b.
		RUN VALUE(proc).
   END.
END.





