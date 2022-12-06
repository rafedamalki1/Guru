/*APPpasCON9.p*/
/*
används inte 


kör regedt32
gå till HKEY_CURRENT_USER SOFTWARE PSC PROGRESS 8.2B WINCHAR STARTUP OCH SÄTT PROPATH
gå till HKEY_LOCAL_MACHINE SOFTWARE PSC PROGRESS 8.2B WINCHAR STARTUP OCH SÄTT PROPATH
för bakgrunds start och schedule HKEY_LOCAL_MACHINE SOFTWARE PSC PROGRESS 8.2B STARTUP OCH SÄTT PROPATH
OM DET INTE FINNS NÅGON PROGRESS KÖR INI2REG.EXE I DLC\BIN OCH VÄLJ PROGRESS.INI
*/
/*
&Scoped-define NEW 
DEFINE INPUT PARAMETER user_id AS CHARACTER.
DEFINE INPUT PARAMETER password AS CHARACTER.
DEFINE INPUT PARAMETER app_server_info AS CHARACTER.
DEFINE VARIABLE koppla AS CHARACTER NO-UNDO.

OUTPUT TO C:\PROTEMP9\TT.TXT.
PUT UNFORMA PROPATH.
OUTPUT CLOSE.
  */
SESSION:SERVER-CONNECTION-BOUND-REQUEST = TRUE.

 
&Scoped-define NEW NEW GLOBAL
/*{EGENBVAR.I}*/
/*SESSION:NUMERIC-FORMAT = "AMERICAN".                                                                                    
SESSION:NUMERIC-FORMAT = "EUROPEAN"*/

&Scoped-define NEW 
DEFINE INPUT PARAMETER user_id AS CHARACTER.
DEFINE INPUT PARAMETER password AS CHARACTER.
DEFINE INPUT PARAMETER app_server_info AS CHARACTER.
DEFINE VARIABLE koppla AS CHARACTER NO-UNDO.

{VALDBDEF.I}
{VALDBALL.I}
  /*
IF app_server_info BEGINS "VAST" THEN DO:
   FIND FIRST valdbtemp WHERE valdbtemp.GFORETAG = SUBSTRING(app_server_info,1,4) AND 
   valdbtemp.DBNAMN = SUBSTRING(app_server_info,5)
   NO-ERROR.
   IF NOT AVAILABLE valdbtemp THEN DO:
      FIND FIRST valdbtemp WHERE valdbtemp.GFORETAG = "VAST" AND 
      valdbtemp.DBNAMN = "VAST"
      NO-ERROR.
   END.
END.
ELSE 
   */
 
demokvar = FALSE.  
FIND FIRST valdbtemp WHERE valdbtemp.GFORETAG = TRIM(app_server_info) NO-ERROR.
IF NOT AVAILABLE valdbtemp THEN DO:
   OUTPUT TO felapp.txt APPEND.
   PUT app_server_info.
   OUTPUT CLOSE.   
   RETURN.
END.
koppla = valdbtemp.DBPLATS + valdbtemp.DBNAMN.
koppla = koppla + " -P 'KAGGEN' -U 'ELPAO'".

CONNECT VALUE(koppla) NO-ERROR. 
/*
OUTPUT TO VALUE(guruvar + "appcon.txt") APPEND.
PUT UNFORMATTED koppla + " " + STRING(CONNECTED(valdbtemp.DBNAMN)) SKIP.
OUTPUT CLOSE.
*/

IF NOT CONNECTED(valdbtemp.DBNAMN) THEN DO: 
   OUTPUT TO VALUE(guruvar + "appcon.txt") APPEND.
   PUT UNFORMATTED koppla + " " + STRING(CONNECTED(valdbtemp.DBNAMN)) SKIP.
   OUTPUT CLOSE.  
   koppla = valdbtemp.DBCON.
   koppla = REPLACE(koppla,"www.guruonweb.se","webguru") NO-ERROR.
   koppla = REPLACE(kommando,"www2.guruonweb.se","webguru") NO-ERROR.
   koppla = koppla + " -P 'KAGGEN' -U 'ELPAO'".  
   CONNECT VALUE(koppla) NO-ERROR. 
END.
IF NOT CONNECTED(valdbtemp.DBNAMN) THEN DO:
   OUTPUT TO VALUE(guruvar + "appcon.txt") APPEND.
   PUT UNFORMATTED koppla + " " + STRING(CONNECTED(valdbtemp.DBNAMN)) SKIP.
   OUTPUT CLOSE.
END.   
{VERALIAS.I}

   
RUN FORVER.P (INPUT valdbtemp.GFORETAG,INPUT valdbtemp.APPCON).   

