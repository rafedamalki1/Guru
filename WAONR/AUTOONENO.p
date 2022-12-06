/*AUTOONENO.p*/
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}

DEFINE VARIABLE utfil AS CHARACTER.
DEFINE VARIABLE sokfil AS CHARACTER.
DEFINE VARIABLE dbfilename AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO. 
DEFINE VARIABLE progflytt AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE gfore AS CHARACTER NO-UNDO.
{VALDBDEF.I}
{VALDBONE.I}  

FIND FIRST valdbtemp WHERE valdbtemp.GFORETAG = "ONENO" NO-LOCK NO-ERROR.

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
   {AppSprinSet.I}
   dbfilename = valdbtemp.DBNAMN.
   PUT "db "  dbfilename  STRING(TIME,"HH:MM:SS") SKIP.
   RUN val_UI.
   IF CONNECTED(LDBNAME(1)) THEN DO:       
     
      RUN ALIASSATT.P.
      
      RUN GFSATT.P (OUTPUT gfore).
      Guru.Konstanter:globforetag = gfore.
      RUN MEDDBORT.P.
      RUN textut_UI (INPUT "MEDDBORT KLAR").
      /* KÖRS BARA I ESMTRLAUTO.P  startar 1.30 varje natt
      IF valdbtemp.GFORETAG = "ONENO" OR valdbtemp.GFORETAG = "ONENOUTBI" THEN DO:
         RUN textut_UI (INPUT "NAPRBERIFS.P"). 
         RUN NAPRBERIFS.P.  /* uppdatera pris i beredning om katalog uppdaterad*/
         RUN textut_UI (INPUT "NAPRBERIFS.P KLAR").
      END.
      ELSE DO: 
         IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
            RUN textut_UI (INPUT "NAPRBER.P").
            RUN NAPRBER.P.  /* uppdatera pris i beredning om katalog uppdaterad*/
            RUN textut_UI (INPUT "NAPRBER.P KLAR").           
         END.
      END.
      */
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
   PUT UNFORMATTED meddvar " " Guru.Konstanter:globforetag " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE.







