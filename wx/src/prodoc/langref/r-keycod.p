/* r-keycod.p */

DEFINE VARIABLE msg  AS CHARACTER EXTENT 3.
DEFINE VARIABLE i    AS INTEGER INITIAL 1.
DEFINE VARIABLE newi AS INTEGER INITIAL 1.

DISPLAY "     Please choose     " SKIP(1)
	" 1  Run order entry    " @ msg[1]
	  ATTR-SPACE SKIP
	" 2  Run receivables    " @ msg[2]
	  ATTR-SPACE SKIP
	" 3  Exit               " @ msg[3]
	  ATTR-SPACE SKIP
	WITH CENTERED FRAME menu NO-LABELS.

REPEAT:
    COLOR DISPLAY MESSAGES msg[i] WITH FRAME menu.
    READKEY.
    IF LASTKEY = KEYCODE("CURSOR-DOWN") AND i < 3
    THEN newi = i + 1.
    ELSE IF LASTKEY = KEYCODE("CURSOR-UP") AND i > 1
    THEN newi = i - 1.
    ELSE IF LASTKEY = KEYCODE("GO") OR
       LASTKEY = KEYCODE("RETURN")
    THEN LEAVE.

    IF i <> newi THEN COLOR DISPLAY NORMAL
      msg[i] WITH FRAME menu.
    i = newi.
END.
