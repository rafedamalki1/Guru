/*AUTOKALMAR.P*/
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}
DEFINE VARIABLE dbfilename AS CHARACTER NO-UNDO.

DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO. 
DEFINE VARIABLE prognamnque2 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE skick  AS LOGICAL.        /* Email status  */
DEFINE VARIABLE efel AS CHARACTER.      /* Status txt  */
DEFINE VARIABLE dbbackupp AS CHARACTER NO-UNDO.
DEFINE VARIABLE utfil AS CHARACTER.
DEFINE VARIABLE sokfil AS CHARACTER.
DEFINE VARIABLE gfore AS CHARACTER NO-UNDO.
{VALDBDEF.I}
ASSIGN
dbbackupp = "BACKUP\"
prognamnque = "D:\delad\server\PRO9S\autotid.txt"
prognamnque2 = "D:\delad\server\PRO9S\autotidsp.txt"
utfil = "D:\delad\klient\pro9\nyahlsell.txt". 

RUN textut_UI (INPUT "START AUTOKÖRNING").
OUTPUT CLOSE.
IF WEEKDAY(TODAY) = 2 THEN DO:
   OS-COPY VALUE(prognamnque) VALUE(prognamnque2).    
   OUTPUT TO  VALUE(prognamnque).
   PUT "TÖMmer" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.     
END.
OUTPUT TO d:\DELAD\server\PRO9S\autotid.txt  APPEND.
PUT "nu".
OUTPUT CLOSE.
{VALDBKALM.I}.

OPEN QUERY vq FOR EACH valdbtemp WHERE valdbtemp.DBNAMN = "gkal"  
NO-LOCK.
GET FIRST vq NO-LOCK.
DO WHILE AVAILABLE(valdbtemp): 
   {AppSprinSet.I}
   RUN val_UI.
   
   IF CONNECTED(LDBNAME(1)) THEN DO:   
      
     /*INGA CHACHE FILER FÖR GRANINGE*/
      RUN ALIASSATT.P.
      RUN GFSATT.P (OUTPUT gfore).
      Guru.Konstanter:globforetag = gfore.
      RUN MEDDBORT.P.
      RUN textut_UI (INPUT "MEDDBORT KLAR").
      /*autotid uppföljning mm*/
      RUN AUTOST.P.
      /*FAKTFOR*/
      RUN textut_UI (INPUT "NYA PRISLISTOR").  
      RUN BESTTABPRIS.P.
      RUN textut_UI (INPUT "NYA PRISLISTOR KLAR").  
      IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
         RUN textut_UI (INPUT "NYA KOSTREG").
         RUN GKALINDEVIS.P.
         RUN textut_UI (INPUT "NYA KOSTREG KLAR").
         RUN textut_UI (INPUT "NYA AO").
         RUN GKAONDEVIS.P.
         RUN textut_UI (INPUT "NYA AO KLAR").
         RUN textut_UI (INPUT "UPPDAT MTRL").
         RUN GKALMTRL.P.
         RUN textut_UI (INPUT "UPPDAT MTRL KLAR").
      END.
      
      RUN FORSTTID.P.
      DISCONNECT VALUE(LDBNAME(1)) NO-ERROR. 
   END.  
   
   GET NEXT vq NO-LOCK.
END.

sokfil = SEARCH(utfil).
IF sokfil = ? THEN sokfil = sokfil.
ELSE OS-DELETE VALUE(utfil). 
/*OBS SKA EJ KÖR VIA DBBACKAI.I*/
OPEN QUERY vq FOR EACH valdbtemp WHERE valdbtemp.DBNAMN = "gkal" NO-LOCK.
GET FIRST vq NO-LOCK.
DO WHILE AVAILABLE(valdbtemp): 
   dbfilename = valdbtemp.DBNAMN.
   RUN val_UI.
   OUTPUT TO VALUE(prognamnque)  APPEND.
   PUT "BACKUPP START " dbfilename " "  TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
   IF dbfilename NE "" THEN RUN DBBACKAI.P (INPUT prognamnque,INPUT dbfilename, INPUT "backup\",INPUT valdbtemp.WWWFTP).
   DISCONNECT VALUE(LDBNAME(1)) NO-ERROR.
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
   PUT UNFORMATTED meddvar " " Guru.Konstanter:globforetag " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE.
