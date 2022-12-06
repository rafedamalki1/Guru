  /*XIN.I*/
 
 /*funkar inte i runtime*/ 

DEFINE VARIABLE lgh AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn1 AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE progvag AS CHARACTER FORMAT "X(20)" NO-UNDO.

prognamn1 = "{1}".
progvag = SUBSTRING("{2}",12).
lgh = LENGTH(progvag).
progvag = SUBSTRING(progvag,1,lgh - 10).
lgh = LENGTH(prognamn1).   
prognamn1 = SUBSTRING(prognamn1,1,lgh) + ".old".
prognamn1 = "{3}" + prognamn1. 
prognamn1 = LC(prognamn1).

OUTPUT TO {4} APPEND.
PUT "finnsfil = " AT 4 "{5}"  SKIP.
PUT "IF finnsfil NE ? THEN DO:" AT 4 SKIP .
PUT "{2}" AT 7
    "REPEAT:" AT 7 SKIP
    "CREATE " AT 10  "{1}" "." SKIP
    "ASSIGN." AT 10 SKIP 
    "IMPORT " AT 10 "{1} " "NO-ERROR." SKIP
    "END." AT 7 SKIP
    "INPUT CLOSE." AT 7 SKIP.
PUT UNFORMATTED "OS-RENAME " AT 7 progvag " " prognamn1 "."  SKIP.
PUT "END." AT 4 SKIP .
OUTPUT CLOSE. 


 
