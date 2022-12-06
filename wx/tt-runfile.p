/*tt-runfile.p*/
{MTRLTEMP.I}
DEFINE VARIABLE FILENAME AS CHARACTER NO-UNDO.
FILENAME = "c:\ksvedit\test.txt".
RUN tt-file.p(INPUT temp-table mtrltemp:handle, INPUT filename, INPUT "")
