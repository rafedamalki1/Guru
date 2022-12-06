/*AUTOES.P 2009-08-26*/
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}

DEFINE VARIABLE utfil AS CHARACTER.
DEFINE VARIABLE sokfil AS CHARACTER.
DEFINE VARIABLE dbfilename AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO. 
DEFINE VARIABLE progflytt AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque2 AS CHARACTER NO-UNDO.
{VALDBDEF.I}
  
{VALDBGRAN.I}

FIND FIRST valdbtemp WHERE valdbtemp.GFORETAG = "GRANES" NO-LOCK NO-ERROR.

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
PUT "START AUTOKÖRNING " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.
IF WEEKDAY(TODAY) = 2 THEN DO:
   OS-COPY VALUE(prognamnque) VALUE(prognamnque2).
   OUTPUT TO  VALUE(prognamnque).
   PUT "TÖMD" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.     
END.
OPEN QUERY vq FOR EACH valdbtemp WHERE valdbtemp.DBNAMN NE "UTBI" NO-LOCK.
GET FIRST vq NO-LOCK.
DO WHILE AVAILABLE(valdbtemp): 
   dbfilename = valdbtemp.DBNAMN.
   PUT "db "  dbfilename  STRING(TIME,"HH:MM:SS") SKIP.
   RUN val_UI.
   IF CONNECTED(LDBNAME(1)) THEN DO:       
     /* IF prognamn NE "" THEN SAVE CACHE COMPLETE VALUE(LDBNAME(1)) TO VALUE(prognamn).*/
    /*INGA CHACHE FILER FÖR GRANINGE*/
      
      RUN ALIASSATT.P.
      RUN GFSATT.P (OUTPUT globforetag).
      RUN MEDDBORT.P.
      RUN textut_UI (INPUT "MEDDBORT KLAR").
      IF globforetag = "GRAN" THEN DO:
         utfil = "d:\elpool\delad\pro9\wrk\nyahlsell.txt". 
         sokfil = SEARCH(utfil).
         IF sokfil = ? THEN sokfil = sokfil.
         ELSE DO:  
            RUN textut_UI (INPUT "UPPDAT MTRL").
            RUN GRANAHLS.P (INPUT 1, INPUT 1).
            OS-DELETE VALUE(utfil).
            RUN textut_UI (INPUT "UPPDAT MTRL KLAR").
         END.            
         utfil = "d:\elpool\delad\pro9\wrk\nyonninen.txt". 
         sokfil = SEARCH(utfil).
         IF sokfil = ? THEN sokfil = sokfil.
         ELSE DO:  
            RUN textut_UI (INPUT "UPPDAT MTRL").
            RUN GRANAHLS.P (INPUT 1, INPUT 2).
            OS-DELETE VALUE(utfil).
            RUN textut_UI (INPUT "UPPDAT MTRL KLAR").
         END.     
         RUN textut_UI (INPUT "NAPRBER.P").
         RUN NAPRBER.P.  /* uppdatera pris i beredning om katalog uppdaterad*/
         RUN textut_UI (INPUT "NAPRBER.P KLAR").           
      END.
      
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
