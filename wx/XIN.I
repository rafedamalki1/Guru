  /*XIN.I*/
 
 /*funkar inte i runtime*/ 
DEFINE VARIABLE lgh AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn1 AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE progvag AS CHARACTER FORMAT "X(20)" NO-UNDO.
OUTPUT TO d:\DELAD\PRO9\GURU\WX\XINLADD.P APPEND.

PUT "{2}" AT 1
    "REPEAT:" AT 1 SKIP
    "CREATE " AT 4  "{1}" "." SKIP
    "ASSIGN." AT 4 SKIP 
    "IMPORT " AT 4 "{1} " "NO-ERROR." SKIP
    "END." AT 1 SKIP
    "INPUT CLOSE." AT 1 SKIP.
OUTPUT CLOSE. 
prognamn1 = "{1}".

progvag = SUBSTRING("{2}",12).
lgh = LENGTH(progvag).
progvag = SUBSTRING(progvag,1,lgh - 10).
lgh = LENGTH(prognamn1).   
prognamn1 = SUBSTRING(prognamn1,1,lgh) + ".old".
prognamn1 = "{3}" + prognamn1. 
prognamn1 = LC(prognamn1).
OUTPUT TO d:/DELAD/PRO9/GURU/WX/XINLADD.P APPEND.

PUT UNFORMATTED "OS-RENAME " progvag " " prognamn1 "." SKIP.
    
OUTPUT CLOSE. 


 
