/*AUTOWWW2EGNAIPVAL.p*/
/*Används ej*/
{VALDBDEF.I}
{VALDBUMBR.I}
DEFINE VARIABLE dbfilename AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque2 AS CHARACTER NO-UNDO.
{Computer_LanIP.I}
RUN dbserver_UI.
FIND FIRST valdbtemp WHERE valdbtemp.DBPLATS NE "" NO-LOCK NO-ERROR.
prognamnque = SUBSTRING(valdbtemp.DBPLATS,1,INDEX(valdbtemp.DBPLATS,"\DB\")). 
prognamnque = prognamnque + "autotid.txt".
prognamnque2 = prognamnque +  "autotidkop.txt".
IF DAY(TODAY) = 28 THEN DO:
   OS-COPY VALUE(prognamnque) VALUE(prognamnque2).
   OUTPUT TO  VALUE(prognamnque).
   PUT "TÖMD" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.     
END.

RUN textut_UI("www2START").
RUN dbbackup_UI.

RUN textut_UI("SLUT AUTOKÖRNING").
QUIT.




PROCEDURE dbbackup_UI :
   OPEN QUERY vq FOR EACH valdbtemp WHERE valdbtemp.DBNAMN NE "UTBI" NO-LOCK.
   GET FIRST vq NO-LOCK.
   DO WHILE AVAILABLE(valdbtemp): 
      /*Anders Olsson Elpool i Umeå AB  21 apr 2017 12:20:12 
      en databas ska stoppas tillfälligt stätt 
      valdbtemp.WWWFTP = ? i valddbxxxx.i kompilera detta program och ta ner db. 
      */
      IF valdbtemp.WWWFTP = ? OR {TAEJMEDDB.I}  THEN.
      ELSE DO:
         RUN textut_UI("Nu ska " + valdbtemp.DBNAMN + " STARTA").
         {AppSprinSet.I}
         dbfilename = valdbtemp.DBNAMN.
         RUN val_UI.
         IF CONNECTED(LDBNAME(1)) THEN DO:
                   
            RUN ALIASSATT.P.    
       /*     RUN HDHAND.P.*/
            RUN MEDDBORT.P.
            RUN textut_UI (INPUT "MEDDBORT KLAR").
            IF valdbtemp.DBNAMN = "SUNDNAT" THEN RUN AUTOSU9BERGET.p (INPUT valdbtemp.FORETAG ).
            IF valdbtemp.DBNAMN = "UMBR" THEN DO:
               RUN textut_UI("mark meddelande " + valdbtemp.DBNAMN + " START").
               RUN AUTOMARKUM.P.  /*skicka meddelande valda datum*/
               RUN textut_UI("mark meddelande " + valdbtemp.DBNAMN + " SLUT").
            END.    
         END.  
         
         {DBBACKAI.I}
      END.   
      GET NEXT vq NO-LOCK.
   END. 
END PROCEDURE.

PROCEDURE val_UI :
   CONNECT VALUE(valdbtemp.DBCON) NO-ERROR.         
END PROCEDURE.

PROCEDURE dbserver_UI :
    FOR EACH valdbtemp:
      IF {TAEJMEDDB.I} THEN DO:
         DELETE valdbtemp.
         NEXT.
      END.
      IF INDEX(valdbtemp.DBCON,Computer_LanIP) NE 0 THEN .   
      ELSE DO:
         DELETE valdbtemp.
         NEXT.
      END.   
   END.
END PROCEDURE.

PROCEDURE textut_UI :
   DEFINE INPUT  PARAMETER instart AS CHARACTER NO-UNDO.
   OUTPUT TO VALUE(prognamnque) APPEND.
   PUT UNFORMATTED instart " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE.

