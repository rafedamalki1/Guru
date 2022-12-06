/*AUTOLULE.P*/
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}

DEFINE VARIABLE dbfilename AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO. 
DEFINE VARIABLE prognamnque2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE gfore AS CHARACTER NO-UNDO.
{VALDBDEF.I}
{VALDBLUL.I}
prognamnque = "D:\elpool\delad\PRO9S\autotid.txt". 
prognamnque2 = "D:\elpool\delad\PRO9S\autotidkop.txt". 
OUTPUT TO VALUE(prognamnque) APPEND.
PUT "START AUTOKÖRNING " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.
IF DAY(TODAY) = 28 THEN DO:
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
   IF valdbtemp.ORDNING = 99 THEN. 
   ELSE RUN val_UI.
   IF CONNECTED(LDBNAME(1)) THEN DO:       
     /* IF prognamn NE "" THEN SAVE CACHE COMPLETE VALUE(LDBNAME(1)) TO VALUE(prognamn).*/
      RUN ALIASSATT.P.
      RUN GFSATT.P (OUTPUT gfore).
      Guru.Konstanter:globforetag = gfore.
      RUN MEDDBORT.P.
      RUN textut_UI (INPUT "MEDDBORT KLAR").
      /*FAKTFOR*/
      /* borttaget 20201203 luleå kör ej fakturering Lena
      RUN textut_UI (INPUT "NYA PRISLISTOR").  
      RUN BESTTABPRIS.P.
      RUN textut_UI (INPUT "NYA PRISLISTOR KLAR").*/  
      RUN AOUTLUL.P.
      RUN textut_UI (INPUT "NYA Projklar").  
      /*
      RUN KUNDUTLUL.P.
      RUN textut_UI (INPUT "NYA kunder").  
      */
      RUN LULEIN.P.
      RUN textut_UI (INPUT "LULE IN OK").  
      /* Låter den ligga kvar, men körningen behövs troligen inte om inte något datum sätts till någon annan körning.. 20201203 luleå  Lena*/
     /*
      RUN LULEEKOSTART.P (INPUT prognamnque,INPUT TODAY, INPUT TRUE,INPUT TRUE,INPUT "").
      RUN textut_UI (INPUT "EKO UT OK").  
      /* Låter den ligga kvar, men körningen behövs troligen inte om inte något datum sätts till någon annan körning.. 20201203 luleå  Lena*/
      */
      RUN AUTOST.P. 
      RUN textut_UI (INPUT "ALLATIDERKLAR").  
      
      RUN textut_UI (INPUT "NYA kostreg och tid").  
      RUN LEVUTLULE.P.
      RUN textut_UI (INPUT "NYA leveranser").  
      /* borttaget 20201203 luleå kör ej ingen tid Lena
      RUN FORSTTID.P.
      RUN textut_UI (INPUT "Ao start").  */

      RUN textut_UI (INPUT "Dep-check start").  
      RUN DEPCHECKLU.P.
      RUN textut_UI (INPUT "Dep-check klar").  
               
   END.  
   ELSE DO:
      OUTPUT TO VALUE(prognamnque) APPEND.
      PUT valdbtemp.DBNAMN "GICK INTE ATT ANSLUTA" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
      OUTPUT CLOSE.
   END.   
   {DBBACKAI.I}
   GET NEXT vq NO-LOCK.
END.

OUTPUT TO VALUE(prognamnque) APPEND.
PUT "SLUT AUTOKÖRNING " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.
QUIT.
PROCEDURE val_UI :
   CONNECT VALUE(valdbtemp.DBCON) NO-ERROR.         
END PROCEDURE.

PROCEDURE textut_UI:
   DEFINE INPUT PARAMETER meddvar AS CHARACTER NO-UNDO.
   OUTPUT TO VALUE(prognamnque) APPEND.
   PUT UNFORMATTED meddvar " " Guru.Konstanter:globforetag " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE.
