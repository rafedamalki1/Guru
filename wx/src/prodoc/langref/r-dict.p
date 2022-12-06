/* r-dict.p */

DEFINE VARIABLE ans AS LOGICAL.

DISPLAY "Do you want to access the Dictionary?"
	WITH ROW 7 COLUMN 20 NO-LABELS.
UPDATE ans.
IF ans THEN DICTIONARY.
