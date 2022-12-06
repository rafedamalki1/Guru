/*APPCONPAS.P*/
/*kör regedt32
gå till HKEY_CURRENT_USER SOFTWARE PSC PROGRESS 8.2B WINCHAR STARTUP OCH SÄTT PROPATH
gå till HKEY_LOCAL_MACHINE SOFTWARE PSC PROGRESS 8.2B WINCHAR STARTUP OCH SÄTT PROPATH
för bakgrunds start och schedule HKEY_LOCAL_MACHINE SOFTWARE PSC PROGRESS 8.2B STARTUP OCH SÄTT PROPATH
OM DET INTE FINNS NÅGON PROGRESS KÖR INI2REG.EXE I DLC\BIN OCH VÄLJ PROGRESS.INI
*/
SESSION:SERVER-CONNECTION-BOUND-REQUEST = TRUE.


 
&Scoped-define NEW NEW GLOBAL

&Scoped-define NEW 
DEFINE INPUT PARAMETER user_id AS CHARACTER.
DEFINE INPUT PARAMETER password AS CHARACTER.
DEFINE INPUT PARAMETER app_server_info AS CHARACTER.
DEFINE VARIABLE koppla AS CHARACTER NO-UNDO.
DEFINE VARIABLE ivar AS INTEGER NO-UNDO.
DEFINE VARIABLE lognamvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE kopplaut AS CHARACTER NO-UNDO.
DEFINE VARIABLE feltxt AS CHARACTER NO-UNDO.

DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
/*VALDBDEF.I*/




DEFINE NEW SHARED VARIABLE guruvarmellan AS CHARACTER NO-UNDO.
 

DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE varmess AS CHARACTER NO-UNDO.
DEFINE VARIABLE varerror AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
/*VALDBTEMP.I*/
DEFINE TEMP-TABLE valdbtemp 
   FIELD ORDNING AS INTEGER
   FIELD FORETAG AS CHARACTER LABEL "Företag"
   FIELD VALDB AS CHARACTER   LABEL "Databas"
   FIELD DBNAMN AS CHARACTER 
   FIELD DBCON AS CHARACTER 
   FIELD DBCACHE AS CHARACTER
   FIELD DBPLATS AS CHARACTER
   FIELD Guru.Konstanter:appcon AS CHARACTER 
   FIELD GFORETAG AS CHARACTER
   FIELD WWWFTP AS LOGICAL
   FIELD WWWSTART AS CHARACTER
   FIELD WWWSTART10 AS CHARACTER
   
   INDEX ORDNING AS PRIMARY FORETAG ORDNING
   INDEX ORDNING2 ORDNING FORETAG 
   INDEX GFORETAG GFORETAG
   INDEX DBNAMN DBNAMN.
DEFINE BUFFER valdbtempbuff FOR valdbtemp.
    
DEFINE VARIABLE isweb AS LOGICAL NO-UNDO.

DO TRANSACTION:
   
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  {WWWGURU11.I}
      valdbtemp.WWWSTART10 = {WWWGURU10.I}
      valdbtemp.FORETAG = "NOSS"
      valdbtemp.GFORETAG = "NOSS"
      valdbtemp.DBNAMN = "NOSS"
      valdbtemp.ORDNING = 0      
          
      valdbtemp.DBCON = "-db NOSS -H " + {www2db.I} + " -S 2853 -N TCP"
      /*
      valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"     
      */
      valdbtemp.APPCON = "-URL https://pas.guruonweb.se:8445/apsv -sessionModel Session-managed"              
      valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
      valdbtemp.DBCACHE = ""      
      valdbtemp.VALDB = "Nossebro Energi".   
      valdbtemp.WWWFTP = TRUE.   
                                                
END.

IF user_id NE "inUser" THEN QUIT.
IF password NE "inPasswd"  THEN QUIT.


filnamn = "appconPas.txt".
DELETE ALIAS RT9 . 
demokvar = FALSE.  
IF WEEKDAY(TODAY) = 2 THEN DO:
   OUTPUT TO VALUE(guruvar + "appconPas.txt").
   PUT "TÖMD" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE. 
   OUTPUT TO VALUE(guruvar + "felappPAS.txt").
   PUT "TÖMD" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.       
END.

FIND FIRST valdbtemp WHERE valdbtemp.GFORETAG = TRIM(app_server_info) NO-ERROR.
IF NOT AVAILABLE valdbtemp THEN DO:
   OUTPUT TO VALUE(guruvar + "felappPAS.txt") APPEND.
   PUT UNFORMATTED app_server_info TODAY string(TIME,"hh:mm:ss") SKIP.
   OUTPUT CLOSE.  
    
   RETURN.
END.

OUTPUT TO VALUE(guruvar + "felappPAS.txt") APPEND.
PUT UNFORMATTED "OK " valdbtemp.GFORETAG valdbtemp.DBNAMN TODAY string(TIME,"hh:mm:ss") SKIP.
OUTPUT CLOSE.  

koppla = valdbtemp.DBPLATS + valdbtemp.DBNAMN.
/*dbflex*/
lognamvar =  " -ld rt9".
kopplaut = koppla + " " + lognamvar.
koppla = koppla + " " + lognamvar + " -P 'PASSWORD' -U 'USER' ".



CONNECT VALUE(koppla) NO-ERROR.  

OUTPUT TO VALUE(guruvar + filnamn) APPEND.
PUT UNFORMATTED kopplaut + " " + STRING(CONNECTED("rt9")) + " första " + string(TIME,"hh:mm:ss") SKIP.
DO ivar = 1 TO ERROR-STATUS:NUM-MESSAGES:
   PUT UNFORMATTED ERROR-STATUS:GET-NUMBER(ivar) " " TODAY " " string(time,"hh:mm:ss") SKIP.
   feltxt = ERROR-STATUS:GET-MESSAGE(ivar). 
   PUT UNFORMATTED feltxt SKIP.      
END.     
OUTPUT CLOSE.
 
IF NOT CONNECTED("rt9") THEN DO: 
   koppla = valdbtemp.DBCON.
   koppla = REPLACE(koppla,"www.guruonweb.se","webguru") NO-ERROR.
   koppla = REPLACE(koppla,"www2.guruonweb.se","webguru") NO-ERROR.
   
   kopplaut = koppla + " " + lognamvar.
   koppla = koppla + " " + lognamvar + " -P 'PASSWORD' -U 'USER' ".  
   CONNECT VALUE(koppla) NO-ERROR. 
   OUTPUT TO VALUE(guruvar + filnamn) APPEND.
   PUT UNFORMATTED kopplaut + " " + STRING(CONNECTED("rt9")) + " andra " + app_server_info SKIP.
   DO ivar = 1 TO ERROR-STATUS:NUM-MESSAGES:
      PUT UNFORMATTED ERROR-STATUS:GET-NUMBER(ivar) " " TODAY " " string(time,"hh:mm:ss") SKIP.
      feltxt = ERROR-STATUS:GET-MESSAGE(ivar). 
      PUT feltxt SKIP.      
   END.     
   OUTPUT CLOSE.
END.
IF NOT CONNECTED("rt9") THEN DO:
   ivar = 1.
   OUTPUT TO VALUE(guruvar + filnamn) APPEND.
   PUT UNFORMATTED kopplaut + " " + STRING(CONNECTED("rt9")) + " " + app_server_info SKIP.
   DO ivar = 1 TO ERROR-STATUS:NUM-MESSAGES:
      PUT UNFORMATTED ERROR-STATUS:GET-NUMBER(ivar) " " TODAY " " string(time,"hh:mm:ss") SKIP.
      feltxt = ERROR-STATUS:GET-MESSAGE(ivar). 
      PUT feltxt SKIP.      
   END.      
     
   OUTPUT CLOSE.
END.   


IF CONNECTED("rt9") THEN DO:
   /*VERALIAS.I*/
   CREATE ALIAS RT9 FOR DATABASE VALUE(LDBNAME(1)) NO-ERROR.
END.   
