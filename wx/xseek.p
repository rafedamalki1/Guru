DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.



DEFINE NEW SHARED VARIABLE deci AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE dew AS LOGICAL NO-UNDO.
DEFINE new SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.

globforetag = "elpa".
RUN SEEKPATH.P.
DISPLAY dlcvar wtidvar guruvar.