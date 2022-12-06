/* r-dfvar4.p */

DEFINE VARIABLE dow AS CHARACTER FORMAT "x(9)" EXTENT 7
       INITIAL ["Sunday",   "Monday", "Tuesday", "Wednesday",
		"Thursday", "Friday", "Saturday"].

DEFINE VARIABLE dob AS DATE INITIAL TODAY.

REPEAT WITH SIDE-LABELS 1 DOWN CENTERED ROW 10 TITLE "Date of Birth":
    DISPLAY SKIP(1).
    UPDATE dob LABEL "Enter date of birth".
    DISPLAY dow[WEEKDAY(dob)] LABEL "It was a".
END.
