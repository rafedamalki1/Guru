/*AUTOTRANS.P*/
&Scoped-define NEW NEW 
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE filename AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO. 
{VALDBDEF.I}
prognamnque = "D:\delad\PRO9S\autotid.txt". 
prognamn = "D:\delad\PRO9\guru\trec.CSH".
OUTPUT TO VALUE(prognamnque) APPEND.
PUT "START AUTOK�RNING TREC " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.
IF WEEKDAY(TODAY) = 2 THEN DO:
   OUTPUT TO  VALUE(prognamnque).
   PUT "T�MD" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.     
END.
{VALDBTRANS.I}.
OPEN QUERY vq FOR EACH valdbtemp WHERE valdbtemp.DBNAMN NE "UTBI" NO-LOCK.
GET FIRST vq NO-LOCK.
DO WHILE AVAILABLE(valdbtemp): 
   RUN val_UI.
   IF CONNECTED(LDBNAME(1)) THEN DO:   
      SAVE CACHE COMPLETE VALUE(LDBNAME(1)) TO VALUE(prognamn).
      DISCONNECT VALUE(LDBNAME(1)) NO-ERROR. 
   END.  
   GET NEXT vq NO-LOCK.
END.


OUTPUT TO VALUE(prognamnque)  APPEND.
PUT "BACKUPP START " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.

RUN TRANSBACK.P (INPUT prognamnque).
OUTPUT TO VALUE(prognamnque) APPEND.
PUT "SLUT AUTOK�RNING TRANS " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.

PROCEDURE val_UI :
   CONNECT VALUE(valdbtemp.DBCON) NO-ERROR.       
END PROCEDURE.


