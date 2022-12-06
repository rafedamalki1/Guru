/* r-defb3.p */

DEFINE NEW SHARED BUFFER rec FOR {1} PRESELECT.
DEFINE VARIABLE flist AS CHARACTER EXTENT 12.
DEFINE VARIABLE I AS INTEGER.

FIND _File "{1}".
FOR EACH _Field OF _File USE-INDEX _Field-posit:
    IF i >= 12 THEN LEAVE.
    i = i + 1.
    flist[i] = _Field._Field-name.
END.
DO PRESELECT EACH rec {2} {3} {4} {5} {6} {7} {8} {9} {10} {11} {12}:
    RUN r-defb4.p "{1}" flist[1] flist[2] flist[3] flist[4] flist[5]
	flist[6] flist[7] flist[8] flist[9] flist[10] flist[11] flist[12].
END.
