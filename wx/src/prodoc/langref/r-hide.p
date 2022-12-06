/* r-hide.p */

DEFINE VARIABLE selection AS INTEGER FORM "9".

FORM   "Please Make A Selection:" SKIP(2)
       "    1. Hide Frame A.    " SKIP
       "    2. Hide Frame B.    " SKIP
       "    3. Hide All.        " SKIP
       "    4. Hide This Frame  " SKIP
       "    5. Exit             " SKIP(2)
       WITH FRAME X NO-LABELS.

REPEAT:
    VIEW FRAME x.
    DISPLAY "This is frame A."
	WITH FRAME a ROW 1 COLUMN 60.
    DISPLAY "This is frame B."
	WITH FRAME b ROW 16 COLUMN 10 4 DOWN.
    MESSAGE "Make your selection!".
    UPDATE
	"Selection: " selection
	     VALIDATE(0 < selection AND selection < 7,
		       "Invalid selection")  AUTO-RETURN
	WITH FRAME x.
    IF selection = 1 THEN HIDE FRAME a.
    ELSE IF selection = 2 THEN HIDE FRAME b.
    ELSE IF selection = 3 THEN HIDE ALL.
    ELSE IF selection = 4 THEN HIDE FRAME x.
    ELSE IF selection = 5 THEN LEAVE.
    PAUSE.
END.
