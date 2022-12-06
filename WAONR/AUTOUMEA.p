/*AUTOUMEA.P*/

&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}

DEFINE VARIABLE dbfilename AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO. 
DEFINE VARIABLE prognamnque2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE gfore AS CHARACTER NO-UNDO.
{VALDBDEF.I}
{VALDBUMEA.I}
prognamnque = "D:\delad\PRO9S\autotid.txt". 
prognamnque2 = "D:\delad\PRO9S\autotidkop.txt".  
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
      
      RUN val_UI.
      IF CONNECTED(LDBNAME(1)) THEN DO:       
     /* IF prognamn NE "" THEN SAVE CACHE COMPLETE VALUE(LDBNAME(1)) TO VALUE(prognamn).*/
      RUN ALIASSATT.P.
      RUN GFSATT.P (OUTPUT gfore).
      Guru.Konstanter:globforetag = gfore.
      RUN textut_UI (INPUT "mark meddelande  START").
      RUN AUTOMARKUM.P.      
      RUN textut_UI (INPUT "mark meddelande KLART").
      
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

