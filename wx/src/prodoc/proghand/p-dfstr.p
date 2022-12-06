/* p-dfstr.p */

DEFINE STREAM rpt.
DEFINE STREAM exceptions.
DEFINE VARIABLE fnr AS CHARACTER FORMAT "x(12)".
DEFINE VARIABLE fne AS CHARACTER FORMAT "x(12)".
DEFINE VARIABLE excount AS INTEGER LABEL "Total Number of exceptions".
DEFINE VARIABLE exception AS LOGICAL.

SET fnr LABEL "Enter filename for report output" SKIP(1)
    fne LABEL "Enter filename for exception output"
    WITH SIDE-LABELS FRAME fnames.

OUTPUT STREAM rpt TO VALUE(fnr) PAGED.
OUTPUT STREAM exceptions TO VALUE(fne) PAGED.

DISPLAY STREAM rpt "Item Inventory Report" SKIP(2)
    WITH CENTERED NO-BOX FRAME rpt-frame STREAM-IO.
DISPLAY STREAM exceptions "Item Exception Report" SKIP(2)
    WITH CENTERED NO-BOX FRAME except-frame STREAM-IO.

FOR EACH item:
    IF on-hand < alloc THEN DO:
	DISPLAY STREAM exceptions item-num item-name on-hand alloc
	    WITH FRAME exitem DOWN STREAM-IO.
	excount = excount + 1.
	exception = TRUE.
    END.
    DISPLAY STREAM rpt item-num item-name WITH NO-LABELS NO-BOX STREAM-IO.
    IF exception
    THEN DISPLAY STREAM rpt "See Exception Report".
    exception = FALSE.
END.

DISPLAY STREAM exceptions SKIP(1) excount
      WITH FRAME exc SIDE-LABELS STREAM-IO.
DISPLAY STREAM rpt WITH FRAME exc STREAM-IO.

OUTPUT STREAM rpt CLOSE.
OUTPUT STREAM exceptions CLOSE.
