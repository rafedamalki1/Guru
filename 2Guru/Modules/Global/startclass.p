/*startclass.p*/
/*Anders Olsson Elpool i Umeå AB  27 apr 2017 10:58:43 
vill ta reda på hur PROGRESS och Guru är installerat.
Förenkling:
kör man INTE webclienten är hkeyvar alltid lokal admin
kör man v10 är hkeyvar alltid lokal admin


HKEYSTART.I
första kollen om man kör som CURRENT USER eller local
  
laddar hkeyvar

  
1. webclient eller annan client
2. admin eller CURRENT
via HKEYSTARTLOAD.I
HKEYSTARTLOAD.I
  om webclient
   HKEYADMPER.I

  annars koll VAR finns
   SOFTWARE\PSC\


HKEYADMPER.I
  om 10  HKEYADMPER10.p
  om 11 HKEYADMPER11.p

HKEYADMPER10.p
VAR finns SOFTWARE\PSC\WEBCLIENT\
HKEYADMPER11.p
SESSION:WC-ADMIN-APP
retunerar TRUE om GURU är installerad som lokal admin inget koll på VAR webclienten finns

i startclass.p om webclient
  HKEYLOCAMACHINE.I
   hkeyvar för VAR är PROGRESS installerat!!!!!
   via HKEYADMPER10.p 
   
   APPLICATIONLOAD.I

   kollar VAR är applikationen installerad!!! via HKEYADMPER.I 
*/


&Scoped-define NEW NEW


DEFINE VARIABLE globforetag AS CHARACTER NO-UNDO. 
DEFINE VARIABLE hpApi AS HANDLE NO-UNDO.
DEFINE VARIABLE hpWinFunc AS HANDLE NO-UNDO.
DEFINE VARIABLE companyname AS CHARACTER NO-UNDO.
DEFINE VARIABLE appnamn AS CHARACTER NO-UNDO.

DEFINE VARIABLE kommandofran AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommandotill AS CHARACTER NO-UNDO.
DEFINE VARIABLE wcstart AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE oeversion AS CHARACTER NO-UNDO.
DEFINE VARIABLE dlcvarin AS CHARACTER NO-UNDO.
DEFINE VARIABLE guruvarin AS CHARACTER NO-UNDO.
DEFINE VARIABLE dlcdir AS CHARACTER NO-UNDO.
DEFINE VARIABLE wtid AS CHARACTER NO-UNDO.
{VARFORETYP.I}
{HKEYSTART.I}
DEFINE TEMP-TABLE provag
   FIELD VAG AS CHARACTER.
{FORETEMP.I} 
/*{GURUTEXTERTT.I}*/
RUN Modules\Global\NUMBERSEPA.P.
Guru.Konstanter:HandleKopieraKlistraglobal().
Guru.Konstanter:globalhkeyvar = hkeyvar.
Guru.Konstanter:appcon = Guru.Konstanter:appcon.
Guru.Konstanter:apphand = Guru.Konstanter:apphand.
Guru.Konstanter:globradbrytch = CHR(10).
{windowsclass.i}

Guru.Konstanter:hpApi = hpApi.
Guru.Konstanter:hpWinFunc = hpWinFunc.
IF Guru.Konstanter:appcon THEN DO:
   RUN FORETAGHTM.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (OUTPUT globforetag).
END.
ELSE DO:
   RUN FORETAGHTM.P (OUTPUT globforetag).
END.
Guru.Konstanter:globforetag = globforetag.
IF Guru.Konstanter:appcon THEN DO:
  RUN EGENBENFORE.p ON Guru.Konstanter:apphand TRANSACTION DISTINCT  (OUTPUT TABLE foretemp).
  
END.
ELSE DO:
   RUN EGENBENFORE.p (OUTPUT TABLE foretemp).
   
END.
FIND FIRST foretemp NO-ERROR.

ASSIGN
Guru.Konstanter:globnystart = TRUE
Guru.Konstanter:globforetag = foretemp.FORETAG
Guru.GlobalaVariabler:plusdval = foretemp.PLUSD.

REPEAT i=1 TO NUM-ENTRIES(PROPATH):
    CREATE provag.
    provag.VAG = TRIM(STRING(ENTRY(i,PROPATH),"x(78)")).
    provag.VAG = REPLACE(provag.VAG,"PROPATH=","").
END.

ASSIGN
Guru.Konstanter:dlcvar = ""
Guru.Konstanter:wtidvar = ""
Guru.Konstanter:dlcvar = ""
Guru.Konstanter:gurubilder = "".



IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:  
   /*Anders Olsson Elpool i Umeå AB  2 feb 2017 14:19:53 
   HKEY_LOCAL_MACHINE
   */
   
   LOAD "SOFTWARE\PSC\WEBCLIENT\" + PROVERSION  BASE-KEY hkeyvar NO-ERROR.
   {HKEYLOCAMACHINE.I}
   USE "SOFTWARE\PSC\WEBCLIENT\" + PROVERSION.
   GET-KEY-VALUE SECTION "STARTUP" KEY "DLC" VALUE dlcvarin.
   UNLOAD "SOFTWARE\PSC\WEBCLIENT\" + PROVERSION.
   Guru.Konstanter:dlcvar = dlcvarin + "\BIN\".
   
   {APPLICATIONLOAD.I}
   USE companyname.
   GET-KEY-VALUE SECTION appnamn KEY "PROWCAPPFILE" VALUE guruvarin.
   
   UNLOAD companyname.
   Guru.Konstanter:guruvar = SUBSTRING(guruvarin,1,INDEX(guruvarin,"wcapp") - 1).
   Guru.Konstanter:wtidvar = Guru.Konstanter:guruvar + "CTID\".        
END.
ELSE DO:
   ASSIGN 
   companyname = "SOFTWARE\PSC\PROGRESS\"
   appnamn = PROVERSION.
   LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
   UNLOAD companyname + appnamn + "\".  
   LOAD companyname BASE-KEY hkeyvar NO-ERROR.
   IF ERROR-STATUS:NUM-MESSAGES > 0 THEN.
   ELSE DO:
      USE companyname. 
      GET-KEY-VALUE SECTION appnamn + "\Startup" KEY "DLC" VALUE dlcdir.
      UNLOAD companyname.
     
      Guru.Konstanter:dlcvar = dlcdir + "\BIN\".
      
      /*
      wtid = dlcdir.
      Guru.Konstanter:wtidvar = REPLACE(wtid,"DLC","GURU\WTID\").
      */
      
   END.
         
   FOR EACH provag:
      IF Guru.Konstanter:dlcvar = "" THEN DO:
         IF INDEX(provag.VAG,"DLC") NE 0 THEN DO:
            Guru.Konstanter:dlcvar = SUBSTRING(provag.VAG,1,INDEX(provag.VAG,"DLC")) + "LC\BIN\".
         END.
      END.   
      IF Guru.Konstanter:wtidvar = "" THEN DO:
         IF INDEX(provag.VAG,"GURU") NE 0 THEN DO: 
            IF INDEX(provag.VAG,CHR(80) + CHR(67) + CHR(49) + CHR(49) + CHR(50)) NE 0 THEN DO:
               IF INDEX(provag.VAG,"KOMP10") NE 0 THEN DO:
                  Guru.Konstanter:wtidvar = SUBSTRING(provag.VAG,1,INDEX(provag.VAG,"KOMP10")) + "OMP10\".
               END.              
            END.  
            IF  Guru.Konstanter:wtidvar = "" THEN DO:    
               IF INDEX(provag.VAG,"WTID") NE 0 THEN DO:
                  Guru.Konstanter:wtidvar = SUBSTRING(provag.VAG,1,INDEX(provag.VAG,"WTID")) + "TID\".
               END.             
               ELSE IF INDEX(provag.VAG,"KOMP10") NE 0 THEN DO:
                  Guru.Konstanter:wtidvar = SUBSTRING(provag.VAG,1,INDEX(provag.VAG,"KOMP10")) + "OMP10\".
               END.              
            END.
         END.
      END. 
   END.
   IF INDEX(Guru.Konstanter:wtidvar,"WTID") NE 0 THEN DO:
      Guru.Konstanter:guruvar = SUBSTRING( Guru.Konstanter:wtidvar,1,INDEX( Guru.Konstanter:wtidvar,"WTID") - 1).
   END.
   ELSE DO:
      IF INDEX(Guru.Konstanter:wtidvar,"KOMP10") NE 0 THEN 
      DO:
         Guru.Konstanter:guruvar = SUBSTRING(Guru.Konstanter:wtidvar,1,INDEX(Guru.Konstanter:wtidvar,"KOMP10") - 1).
      END.
   END.   
END.
Guru.Konstanter:wtidvar = TRIM(Guru.Konstanter:wtidvar).
Guru.Konstanter:guruvar = TRIM(Guru.Konstanter:guruvar).
Guru.Konstanter:dlcvar =  TRIM(Guru.Konstanter:dlcvar).
Guru.Konstanter:gurubilder = Guru.Konstanter:wtidvar + "BILDER\".
IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:  
  
END.
ELSE IF PROGRESS = "FULL" THEN DO:
   IF INDEX(Guru.Konstanter:gurubilder,"KOMP10") = 0 THEN DO:
      Guru.Konstanter:gurubilder = REPLACE(Guru.Konstanter:gurubilder,"WTID","CTID").      
   END.
END.
 
IF Guru.Konstanter:appcon THEN DO:                           
   RUN STYREAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
   (INPUT Guru.Konstanter:globforetag, INPUT-OUTPUT varforetypval, INPUT-OUTPUT varforetypchar, INPUT Guru.Konstanter:globnystart). 
END.
ELSE DO:
   RUN STYREAPP.P 
   (INPUT Guru.Konstanter:globforetag, INPUT-OUTPUT varforetypval, INPUT-OUTPUT varforetypchar, INPUT Guru.Konstanter:globnystart).                  
END. 
{STYREAPPLADD.I}   
IF Guru.Konstanter:varforetypval[25] = 22 THEN .
ELSE DO:
   Guru.Konstanter:varforetypval[25] = ?.
   RUN win_UI.
END.
 
PROCEDURE win_UI :
   DEFINE VARIABLE ostxt AS CHARACTER NO-UNDO.
   DEFINE VARIABLE oshelp AS CHARACTER NO-UNDO.
   IF OPSYS = "win32" THEN.
   ELSE RETURN.
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN.
   ELSE IF Guru.Konstanter:globforetag = "BORL" THEN.
   ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN.
   ELSE RETURN.
   ostxt = SESSION:TEMP-DIR + "os" + STRING(MTIME) + ".txt".
   OUTPUT TO VALUE(ostxt).
   PUT "" SKIP.
   OUTPUT CLOSE.
   
   oshelp = 'ver.dll > ' + '"' + ostxt + '"'.     
   /*winxp*/
   OS-COMMAND SILENT VALUE(oshelp) NO-ERROR.
   oshelp = ostxt.  
   INPUT FROM VALUE(oshelp).
   REPEAT:
      IMPORT UNFORMATTED oshelp.
      IF oshelp NE "" THEN LEAVE.
   END.
   INPUT CLOSE.
   IF INDEX(oshelp,"windows 2000") > 0 THEN Guru.Konstanter:varforetypval[25] = ?.
   ELSE Guru.Konstanter:varforetypval[25] = 22.
   
   OS-DELETE VALUE(ostxt) NO-ERROR.
      
END PROCEDURE.





