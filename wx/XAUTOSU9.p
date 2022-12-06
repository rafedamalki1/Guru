/*XAUTOSU9.P*/
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO. 
ASSIGN
prognamnque = "D:\delad\server\pro9S\autotid.txt"
               
prognamn = "D:\DELAD\klient\pro9\guru\sund9.CSH".
OUTPUT TO D:\delad\server\pro9S\autotid.txt APPEND. 
PUT "START AUTOKÖRNING SU " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.
IF WEEKDAY(TODAY) = 2 THEN DO:
   OUTPUT TO D:\delad\server\pro9S\autotid.txt.
   PUT "TÖMD" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.     
   /*
   kommando = "prolog D:\delad\server\pro9s\db\sund8".
   OS-COMMAND SILENT VALUE(kommando).
   */
END.           
prognamn = "D:\DELAD\klient\pro9\guru\sundn9.CSH".
CONNECT -db sundn9 -S 2516 -H seguru -N TCP NO-ERROR. 
IF CONNECTED("sundn9") THEN DO:    
   SAVE CACHE COMPLETE VALUE(LDBNAME(1)) TO VALUE(prognamn).
   RUN AUTOFLST.P.
   OUTPUT TO D:\delad\server\pro9S\autotid.txt APPEND.
   PUT "flex KLAR " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
   /*FAKTFOR*/
   RUN textut_UI (INPUT "NYA PRISLISTOR").  
   RUN BESTTABPRIS.P.
   RUN textut_UI (INPUT "NYA PRISLISTOR KLAR").  
   RUN AUTOST.P. 
   RUN textut_UI (INPUT "ALLATIDERKLAR").  
   RUN FORSTTID.P.
   RUN textut_UI (INPUT "TIDLÄGEN").  
   RUN textut_UI (INPUT "LÖPAND UTAN").  
   RUN FAKTKOLL.P.
   RUN textut_UI (INPUT "LÖPAND UTAN KLAR").
   RUN textut_UI (INPUT "NYA FAKTMEDD").  
   RUN FAKMEDF.P.
   RUN textut_UI (INPUT "NYA FAKTMEDD KLAR").
   IF WEEKDAY(TODAY) = 6 THEN DO:
      RUN textut_UI (INPUT "FLEXKOLL").            
      RUN FLAVVAPPNA.P.
      RUN textut_UI (INPUT "FLEXKOLL KLAR").            
   END.     
   DISCONNECT sundn9 NO-ERROR. 
END.

OUTPUT TO D:\delad\server\pro9S\autotid.txt APPEND.
PUT "DB2 KLAR " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.
OUTPUT TO D:\delad\server\pro9S\autotid.txt APPEND.
PUT "BACKUPP START " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.
RUN SUONBACK9.P.

OUTPUT TO D:\delad\server\pro9S\autotid.txt APPEND.
PUT "SLUT AUTOKÖRNING SU " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.
QUIT.

PROCEDURE textut_UI:
   DEFINE INPUT PARAMETER meddvar AS CHARACTER NO-UNDO.
   OUTPUT TO VALUE(prognamnque) APPEND.
   PUT UNFORMATTED meddvar " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE.
