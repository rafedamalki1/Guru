/*AUTOMISV.P*/
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}

DEFINE VARIABLE dbfilename AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO. 
DEFINE VARIABLE progflytt AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque2 AS CHARACTER NO-UNDO.
{VALDBDEF.I}
{VALDBMISV.I}
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
IF DAY(TODAY) = 28 THEN DO:
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
   IF valdbtemp.ORDNING = 99 THEN. 
   ELSE RUN val_UI.
   
   IF CONNECTED(LDBNAME(1)) THEN DO:       
      RUN ALIASSATT.P.
      /*
      SAVE CACHE COMPLETE VALUE(LDBNAME(1)) TO VALUE(prognamn).
      */
      RUN AUTOFLEX.P.
      RUN textut_UI (INPUT "flex KLAR").
       
      RUN AUREG.P (INPUT TRUE).
      RUN textut_UI (INPUT "ALLATIDERKLAR").  
      RUN FORSTTID.P.
      RUN textut_UI (INPUT "TIDL�GEN").  
      
      IF WEEKDAY(TODAY) = 6 THEN DO:
         RUN textut_UI (INPUT "FLEXKOLL").            
         RUN FLAVVAPPNA.P.
         RUN textut_UI (INPUT "FLEXKOLL KLAR").            
      END.
      /* Flyttad till eget program. K�rs m�ndagmorgon 8.00 ist�llet f�r att inte st�ra 20160921 Lena
      IF WEEKDAY(TODAY) = 1 THEN DO:
         RUN textut_UI (INPUT "F�RDIG KOLL").            
         RUN EJFEPOST.P.
         RUN textut_UI (INPUT "F�RDIGKOLL KLAR").            
      END.*/   
      IF WEEKDAY(TODAY) = 2 THEN DO:
         RUN textut_UI (INPUT "GODK�ND KOLL").            
         RUN EJGEPOST.P.
         RUN textut_UI (INPUT "GODK�NDKOLL KLAR").            
      END.
      IF WEEKDAY(TODAY) = 2 THEN DO:
         RUN textut_UI (INPUT "DISP M�NAD �V KOLL").            
         RUN DISPOVMANEPOST.P.
         RUN textut_UI (INPUT "DISP M�NAD �V KOLL KLAR").            
      END.   
      IF DAY(TODAY) = 1 THEN DO:
         RUN textut_UI (INPUT "DISPENS �VERTID KOLL").            
         RUN DISPOVEPOST.P.
         RUN textut_UI (INPUT "DISPENS �VERTID KLAR").            
      END.
               
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
