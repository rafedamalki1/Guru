/* r-dfstr.p */

DEFINE NEW SHARED STREAM rpt.
DEFINE STREAM exceptions.
DEFINE VARIABLE fnr AS CHARACTER FORMAT "x(12)".
DEFINE VARIABLE fne AS CHARACTER FORMAT "x(12)".
DEFINE VARIABLE excount AS INTEGER LABEL "Total Number of exceptions".
DEFINE NEW SHARED BUFFER xitem FOR item.

SET fnr LABEL "Enter filename for report output" SKIP(1)
    fne LABEL "Enter filename for exception output"
    WITH SIDE-LABELS FRAME fnames.

OUTPUT STREAM rpt TO VALUE(fnr) PAGED.
OUTPUT STREAM exceptions TO VALUE(fne) PAGED.

FOR EACH xitem:
    IF on-hand < alloc THEN DO:
	DISPLAY STREAM exceptions item-num item-name on-hand alloc
	    WITH FRAME exitem DOWN.
	excount = excount + 1.
    END.
    RUN r-dfstr2.p.
END.

DISPLAY STREAM exceptions SKIP(1) excount WITH FRAME exc SIDE-LABELS.
DISPLAY STREAM rpt WITH FRAME exc.

OUTPUT STREAM rpt CLOSE.
OUTPUT STREAM exceptions CLOSE.
