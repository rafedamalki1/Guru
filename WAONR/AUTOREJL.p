/*AUTOREJL.P*/
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}

DEFINE VARIABLE dbfilename AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO. 
DEFINE VARIABLE progflytt AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque2 AS CHARACTER NO-UNDO.
{VALDBDEF.I}
{VALDBREJL.I}
FIND FIRST valdbtemp NO-LOCK NO-ERROR.
IF AVAILABLE valdbtemp THEN DO:
   progflytt = SUBSTRING(valdbtemp.DBPLATS,1,INDEX(valdbtemp.DBPLATS,"DB\") - 1).
   prognamnque = progflytt + "autotid.txt". 
   prognamnque2 = progflytt + "autotidkop.txt".
END.
ELSE DO:
   prognamnque = "autotid.txt".
   prognamnque2 = "autotidkop.txt".
END.
OUTPUT TO VALUE(prognamnque) APPEND.
PUT "START AUTOK�RNING " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.
IF WEEKDAY(TODAY) = 2 THEN DO:
   OS-COPY VALUE(prognamnque) VALUE(prognamnque2).
   OUTPUT TO  VALUE(prognamnque).
   PUT "T�MD" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.     
END.
OPEN QUERY vq FOR EACH valdbtemp WHERE valdbtemp.DBNAMN NE "UTBI" NO-LOCK.
GET FIRST vq NO-LOCK.
DO WHILE AVAILABLE(valdbtemp): 
   {AppSprinSet.I}
   dbfilename = valdbtemp.DBNAMN.
   
   RUN val_UI.
   IF CONNECTED(LDBNAME(1)) THEN DO:       
     /* IF prognamn NE "" THEN SAVE CACHE COMPLETE VALUE(LDBNAME(1)) TO VALUE(prognamn).*/
      RUN ALIASSATT.P.
      RUN MEDDBORT.P.
      RUN textut_UI (INPUT "MEDDBORT KLAR").        
   END.  
   {DBBACKAI.I}
   GET NEXT vq NO-LOCK.
END.

OUTPUT TO VALUE(prognamnque) APPEND.
PUT "SLUT AUTOK�RNING " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.
PROCEDURE val_UI :
   CONNECT VALUE(valdbtemp.DBCON) NO-ERROR.         
END PROCEDURE.

PROCEDURE textut_UI:
   DEFINE INPUT PARAMETER meddvar AS CHARACTER NO-UNDO.
   OUTPUT TO VALUE(prognamnque) APPEND.
   PUT UNFORMATTED meddvar " " valdbtemp.FORETAG " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE.