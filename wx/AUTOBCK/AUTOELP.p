/*AUTOELP.P*/
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}

DEFINE VARIABLE dbfilename AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO. 
DEFINE VARIABLE prognamnque2 AS CHARACTER NO-UNDO.
{VALDBDEF.I}
CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "ELPA"
      valdbtemp.GFORETAG = "ELPA"
      valdbtemp.DBNAMN = "RT9"
      valdbtemp.ORDNING = 1      
      valdbtemp.DBCON = "-db RT9 -S 3327 -H pc112 -N TCP"
      valdbtemp.DBPLATS = "c:\DELAD\PRO11S\DB\"
      valdbtemp.APPCON = "-AppService app11elpool -H PC112 -S 2755'
      valdbtemp.VALDB = "Guru kalkutveckling".      
prognamnque = "c:\delad\PRO11S\autotid.txt". 
prognamnque2 = "C:\delad\PRO11S\autotidkop.txt".
OUTPUT TO VALUE(prognamnque) APPEND.
PUT "START AUTOKÖRNING " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.
IF DAY(TODAY) = 28 AND valdbtemp.WWWFTP = FALSE  THEN DO:
   OS-COPY VALUE(prognamnque) VALUE(prognamnque2).
   OUTPUT TO  VALUE(prognamnque).
   PUT "TÖMD" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.     
END.
OPEN QUERY vq FOR EACH valdbtemp WHERE valdbtemp.DBNAMN NE "UTBI" NO-LOCK.
GET FIRST vq NO-LOCK.
DO WHILE AVAILABLE(valdbtemp): 
   dbfilename = valdbtemp.DBNAMN.
   
   RUN val_UI.
   IF CONNECTED(LDBNAME(1)) THEN DO:       
     /* IF prognamn NE "" THEN SAVE CACHE COMPLETE VALUE(LDBNAME(1)) TO VALUE(prognamn).*/
      RUN ALIASSATT.P.
              
   END.  
   {DBBACKAI.I}
   GET NEXT vq NO-LOCK.
END.
OUTPUT TO VALUE(prognamnque) APPEND.
PUT "SLUT AUTOKÖRNING " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.
PROCEDURE val_UI :
   CONNECT VALUE(valdbtemp.DBCON) NO-ERROR.         
END PROCEDURE.

PROCEDURE textut_UI:
   DEFINE INPUT PARAMETER meddvar AS CHARACTER NO-UNDO.
   OUTPUT TO VALUE(prognamnque) APPEND.
   PUT UNFORMATTED meddvar " " globforetag " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE.
