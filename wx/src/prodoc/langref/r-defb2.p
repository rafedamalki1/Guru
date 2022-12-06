/* r-defb2.p */

DEFINE VARIABLE tname AS CHARACTER FORMAT "x(12)" LABEL "Table name".
DEFINE VARIABLE conditions AS CHARACTER FORMAT "x(60)"
		LABEL "Conditions".


REPEAT:
    UPDATE tname COLON 12 conditions COLON 12 WITH SIDE-LABELS 1 DOWN.
    HIDE ALL.
    IF conditions <> "" THEN
	RUN r-defb3.p tname "WHERE" conditions.
    ELSE RUN r-defb3.p tname.
END.
