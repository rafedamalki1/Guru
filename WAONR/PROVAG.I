/*PROVAG.I*/
DEFINE VARIABLE gurucert AS CHARACTER NO-UNDO.
DEFINE VARIABLE dlccert AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommandofran AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommandotill AS CHARACTER NO-UNDO.
DEFINE VARIABLE wcstart AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE oeversion AS CHARACTER NO-UNDO.
DEFINE VARIABLE companyname AS CHARACTER NO-UNDO.
DEFINE VARIABLE appnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE hkeyvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE pvdlc AS CHARACTER NO-UNDO.
DEFINE VARIABLE pvguruvar AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE provag
   FIELD VAG AS CHARACTER.
REPEAT i = 1 TO NUM-ENTRIES(PROPATH):
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
   {HKEYADMPER.I}
   
   LOAD "SOFTWARE\PSC\WEBCLIENT\" + PROVERSION  BASE-KEY hkeyvar NO-ERROR.
   {HKEYLOCAMACHINE.I}
   USE "SOFTWARE\PSC\WEBCLIENT\" + PROVERSION.
   GET-KEY-VALUE SECTION "STARTUP" KEY "DLC" VALUE pvdlc.
   UNLOAD "SOFTWARE\PSC\WEBCLIENT\" + PROVERSION.
   Guru.Konstanter:dlcvar = pvdlc + "\BIN\".
   
   {HKEYCURRENTUSER.I}
   {APPLICATIONLOAD.I}
   
   USE companyname.
   GET-KEY-VALUE SECTION appnamn KEY "PROWCAPPFILE" VALUE pvguruvar.
   UNLOAD companyname.
   Guru.Konstanter:guruvar = SUBSTRING(pvguruvar,1,INDEX(pvguruvar,"wcapp") - 1).
   Guru.Konstanter:wtidvar = Guru.Konstanter:guruvar + "CTID\".
  
    
    
END.
ELSE DO:
   ASSIGN 
   companyname = "SOFTWARE\PSC\PROGRESS\"
   hkeyvar = "HKEY_LOCAL_MACHINE"
   appnamn = PROVERSION.
   LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
   UNLOAD companyname + appnamn + "\".  
   LOAD companyname BASE-KEY hkeyvar NO-ERROR.
   IF ERROR-STATUS:NUM-MESSAGES > 0 THEN.
   ELSE DO:
      USE companyname. 
      GET-KEY-VALUE SECTION appnamn + "\Startup" KEY "DLC" VALUE pvdlc.
      UNLOAD companyname.
      /*
      Guru.Konstanter:wtidvar = REPLACE(Guru.Konstanter:dlcvar,"DLC","GURU\WTID\").
      */
      Guru.Konstanter:dlcvar = pvdlc + "\BIN\".
       
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
             IF Guru.Konstanter:wtidvar = "" THEN DO:    
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
      Guru.Konstanter:guruvar = SUBSTRING(Guru.Konstanter:wtidvar,1,INDEX(Guru.Konstanter:wtidvar,"WTID") - 1).
   END.
   ELSE DO:
      IF INDEX(Guru.Konstanter:wtidvar,"KOMP10") NE 0 THEN DO:
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

IF OPSYS = "unix" THEN DO:
   Guru.Konstanter:wtidvar = REPLACE(Guru.Konstanter:wtidvar,"\","/").
   Guru.Konstanter:guruvar = REPLACE(Guru.Konstanter:guruvar,"\","/").
   Guru.Konstanter:dlcvar = REPLACE(Guru.Konstanter:dlcvar,"\","/").
   Guru.Konstanter:gurubilder = REPLACE(Guru.Konstanter:gurubilder,"\","/").
   
END.
dlccert = REPLACE(Guru.Konstanter:dlcvar,"\bin","\certs").

gurucert = REPLACE(Guru.Konstanter:wtidvar,"\WTID","\CINSTALL").
gurucert = REPLACE(gurucert,"\CTID","\CINSTALL").



