/*AUTOVATTDP.P*/
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}


DEFINE VARIABLE filename AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO. 
DEFINE VARIABLE prognamnque2 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE skick  AS LOGICAL.        /* Email status  */
DEFINE VARIABLE efel AS CHARACTER.      /* Status txt  */
DEFINE VARIABLE utfil AS CHARACTER.
DEFINE VARIABLE sokfil AS CHARACTER.
DEFINE VARIABLE dbfilename AS CHARACTER NO-UNDO.
DEFINE VARIABLE gfore AS CHARACTER NO-UNDO.
prognamnque = "e:\delad\pro9s\autotid.txt".
prognamnque2 = "e:\delad\pro9s\autotidkop.txt".
OUTPUT TO e:\delad\pro9s\autotid.txt APPEND.
PUT "START AUTOKÖRNING VATTENFALL " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.
IF WEEKDAY(TODAY) = 2 THEN DO:
   OS-COPY VALUE(prognamnque) VALUE(prognamnque2).
   OUTPUT TO VALUE(prognamnque).
   PUT "TÖMD" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.     
   
END.
{VALDBDEF.I}
{VALDBVESABDP.I}
/*{VALDBVELD.I}*/


OPEN QUERY vq FOR EACH valdbtemp WHERE valdbtemp.DBNAMN NE "VUTBI" NO-LOCK.
GET FIRST vq NO-LOCK.
DO WHILE AVAILABLE(valdbtemp): 
   IF {TAEJMEDDB.I} THEN dbfilename = dbfilename.
   ELSE IF valdbtemp.GFORETAG = "NVSAB" THEN dbfilename = dbfilename.
   ELSE DO:
      {AppSprinSet.I}
      dbfilename = valdbtemp.DBNAMN.
      RUN val_UI.
      IF CONNECTED(LDBNAME(1)) THEN DO:   
        /*INGA CHACHE FILER FÖR GRANINGE*/
         RUN ALIASSATT.P.
         RUN GFSATT.P (OUTPUT gfore).
   Guru.Konstanter:globforetag = gfore.
         RUN MEDDBORT.P.
         RUN textut_UI (INPUT "MEDDBORT KLAR").
         IF valdbtemp.DBNAMN = "VORD" OR valdbtemp.DBNAMN = "VOST" OR 
         valdbtemp.DBNAMN = "VAST" OR valdbtemp.DBNAMN = "VSYD" OR valdbtemp.DBNAMN = "VSAB" THEN DO:    
            utfil = "e:\delad\pro9\guru\nyelekt.txt".
            sokfil = SEARCH(utfil).
            IF sokfil = ? THEN sokfil = sokfil.
            ELSE DO:         
               RUN textut_UI (INPUT "MTRL UPP START").
               RUN GRANAHLS.P (INPUT 1, INPUT 1).
               OS-DELETE VALUE(utfil).
               RUN textut_UI (INPUT "MTRL UPP KLART").
            END.            
            /*ANVÄNDS EJ
            utfil = "e:\delad\pro9\guru\nyonninen.txt".
            sokfil = SEARCH(utfil).
            IF sokfil = ? THEN sokfil = sokfil.
            ELSE DO:         
               RUN textut_UI (INPUT "MTRL UPP START").
               RUN GRANAHLS.P (INPUT 1, INPUT 2).
               OS-DELETE VALUE(utfil).
               RUN textut_UI (INPUT "MTRL UPP KLART").
            END.*/
           
                        
         END.
         
      END.  
      {DBBACKAI.I}
   END.
   GET NEXT vq NO-LOCK.
END.

CLOSE QUERY vq.

RUN textut_UI (INPUT "SLUT AUTOKÖRNING").

PROCEDURE val_UI :
   CONNECT VALUE(valdbtemp.DBCON) NO-ERROR.       
END PROCEDURE.

PROCEDURE textut_UI:
   DEFINE INPUT PARAMETER meddvar AS CHARACTER NO-UNDO.
   OUTPUT TO VALUE(prognamnque) APPEND.
   PUT UNFORMATTED meddvar " " Guru.Konstanter:globforetag " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE.
