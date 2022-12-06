/*GuruClasserStart.p*/

DEFINE INPUT-OUTPUT PARAMETER nyprog AS LOGICAL NO-UNDO.
DEFINE INPUT  PARAMETER bytanv AS CHARACTER NO-UNDO.

DEFINE VARIABLE cc AS HANDLE NO-UNDO.

Guru.Konstanter:globanvbyt = bytanv.
nyprog = FALSE.
            
RUN Modules\Global\startclass.p.

IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
   SESSION:DEBUG-ALERT = YES.
END.   
 