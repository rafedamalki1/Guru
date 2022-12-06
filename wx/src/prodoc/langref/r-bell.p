/* r-bell.p */

DEFINE VARIABLE outfile AS CHARACTER FORMAT "x(8)"
    LABEL "Output file name".

getfile:
DO ON ERROR UNDO, RETRY:
    SET outfile WITH SIDE-LABELS.
    IF SEARCH(outfile) = outfile THEN DO:
	MESSAGE "A file named" outfile "already exists in your directory".
	MESSAGE "Please use another name".
	BELL.
	UNDO getfile, RETRY getfile.
    END.
END.

OUTPUT TO VALUE(outfile).

FOR EACH customer:
    DISPLAY name credit-limit.
END.
