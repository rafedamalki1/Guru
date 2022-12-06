/*KUNDIN.P*/
 /*
 ENHET         char        ENHET 
 BESTID        char        KUNDNR Alltid 6 pos 
 LEVERANSPLATS char        LEVERANSPLATS 
 BESTNAMN      char        NAMN 
 ADRESS        char        ADRESS   
 PNR           char        POSTNUMMER
 ORT           char        POSTADRESS
 FAKADRESS     char        COADRESS + ADRESS
 FAKPNR        char        POSTNUMMER
 FAKORT        char        POSTADRESS
 FAKINT        inte        Grundvärde 30            
 FDAGAR        inte        Grundvärde 30
 MOTPARTID     inte        KONCERNKUND
 MOMSID        inte        Momskod
 KUNDTYP       char        KUNDTYP
 PRISNUM       inte        RABATTBREV   
 PRELAN        INTEGER     PRELAN  O eller 1.
 NYPOST        LOGICAL     TRUE/FALSE  OBS! BEHÖVS EV. Om kundnr återanvänds.
 Filnamn KU*****.UPP.
***** = DATUM + HHMMSS för unikt namn. 
Filen läses till /u10/guru/import.
Efter in läsning i Guru kopieras filen till /u12/guru/import/KU*****.OLD
*/

{VALDBDEF.I}
DEFINE INPUT PARAMETER startfore AS CHARACTER NO-UNDO.
DEFINE VARIABLE chdatvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE utvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE indatvar AS INTEGER NO-UNDO.
DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnk AS CHARACTER NO-UNDO.
DEFINE VARIABLE importvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE importvark AS CHARACTER NO-UNDO.
DEFINE VARIABLE startnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE filnamnstart AS CHARACTER NO-UNDO.
DEFINE VARIABLE filnamnslut AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tidin
   FIELD ENHET         AS INTEGER 
   FIELD BESTID        AS CHARACTER 
   FIELD LEVERANSPLATS AS CHARACTER 
   FIELD BESTNAMN      AS CHARACTER 
   FIELD ADRESS        AS CHARACTER   
   FIELD PNR           AS CHARACTER
   FIELD ORT           AS CHARACTER
   FIELD FAKADRESS     AS CHARACTER
   FIELD FAKPNR        AS CHARACTER
   FIELD FAKORT        AS CHARACTER
   FIELD FAKINT        AS INTEGER            
   FIELD FDAGAR        AS INTEGER
   FIELD MOTPARTID     AS INTEGER
   FIELD MOMSID        AS INTEGER
   FIELD KUNDTYP       AS CHARACTER
   FIELD PRISNUM       AS INTEGER   
   FIELD PRELAN        AS INTEGER
   FIELD NYPOST        AS LOGICAL
   INDEX ENHET IS PRIMARY ENHET.
DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
DEFINE TEMP-TABLE kollforetag                     
   FIELD ENHET              AS INTEGER FORMAT "999"
   FIELD DATABAS            AS CHARACTER
   INDEX ENHET IS PRIMARY ENHET.
   {AMERICANEUROPEAN.I}
IF OPSYS = "UNIX" THEN DO: 
   ASSIGN
   importvar = "import/"
   importvark = "impkopia/".
END.
ELSE DO:
   ASSIGN
   importvar = "import\"
   importvark = "impkopia\".
END.
IF startfore = "GRANINGE" THEN DO:
   RUN skapfore_UI (INPUT 401,INPUT "GRANADM9").
   RUN skapfore_UI (INPUT 409,INPUT "GRANNORD").
   RUN skapfore_UI (INPUT 415,INPUT "GRANIT").
   ASSIGN
   startnamn = "\\granguru\DELAD\server\PRO9S\"
   filnamnstart = "kund"
   filnamnslut = "d".
  
END.
IF startfore = "ELPOOL" THEN DO:
   RUN skapfore_UI (INPUT 401,INPUT "RT9").
   RUN skapfore_UI (INPUT 409,INPUT "RT9").
   RUN skapfore_UI (INPUT 415,INPUT "RT9").
   ASSIGN 
   startnamn = "\\pc112\DELAD\PRO9\guru\"
   filnamnstart = "kund"
   filnamnslut = "d".   
END.
IF OPSYS = "UNIX" THEN DO: 
   kommando = "ls " + startnamn + importvar + filnamnstart + "*." + filnamnslut + ">" + startnamn + "import/" + filnamnstart + ".txt".
   kommandoprog = startnamn + importvar + filnamnstart + ".txt".
END.    
ELSE DO: 
   ASSIGN
   kommando = "DIR/a:-d /b " + startnamn + importvar + filnamnstart + "*." + filnamnslut + ">" + startnamn + "IMPORT\" + filnamnstart + ".txt".   
   kommandoprog = startnamn + importvar + filnamnstart + ".txt".
END. 
EMPTY TEMP-TABLE infil NO-ERROR. 

OS-DELETE VALUE(kommandoprog).
OS-COMMAND SILENT VALUE(kommando) . 

/*VILKA FILER*/
INPUT FROM VALUE(kommandoprog) NO-ECHO.
REPEAT:
   DO TRANSACTION: 
      CREATE infil.
      ASSIGN.
      IMPORT infil   NO-ERROR.
   END.
END.
INPUT CLOSE.
FOR EACH infil:   
   IF INDEX(infil.PROGNAMN,".d") = 0 THEN DO:       
      DELETE infil.
      NEXT.
   END.
   infil.PROGNAMN = SUBSTRING(infil.PROGNAMN,1,INDEX(infil.PROGNAMN,".d") - 1).   
END.
FOR EACH infil:     
   ASSIGN
   prognamn = startnamn + importvar + infil.PROGNAMN 
   prognamnk = startnamn + importvark + infil.PROGNAMN + STRING(TODAY,"999999") + "." + filnamnslut.
   prognamnque = startnamn + importvar + infil.PROGNAMN + ".q".
   RUN in_UI.   
   OS-RENAME VALUE(prognamn) VALUE(prognamnk).  
   OS-DELETE VALUE(prognamnque). 
END.
/*SKAPA KUNDER*/
RUN skapkund_UI.
{EUROPEANAMERICAN.I}
PROCEDURE in_UI: 
   EMPTY TEMP-TABLE intid NO-ERROR. 
   EMPTY TEMP-TABLE tidin NO-ERROR.    
   IF OPSYS = "UNIX" THEN DO:
      kommando = SEARCH("quoter").
      IF kommando = ? THEN DO:          
         RETURN.       
      END.   
   END.
   ELSE DO:      
      kommando = SEARCH("quoter.exe").
      IF kommando = ? THEN RETURN.       
   END.      
   
   OS-COMMAND SILENT VALUE(kommando) VALUE(prognamn) > VALUE(prognamnque).
   IF OS-ERROR > 0 THEN DO:         
      RETURN.
   END.   
   /*LÄS IN RADER FRÅN VARJE FIL*/
   
   INPUT FROM VALUE(prognamnque) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         SET words VIEW-AS EDITOR INNER-CHARS 78 INNER-LINES 80 WITH FRAME DDD WIDTH 80.   
         REPEAT:
            IF INDEX(words,'"',1) = 0 THEN LEAVE.
            words = REPLACE(words,'"',' ').
         END.
         CREATE intid.   
         ASSIGN intid.TIN = words.            
      END.
   END.
   INPUT CLOSE.  
   /*DELA UPP RADER PÅ RÄTT SÄTT*/  
   OUTPUT TO VALUE(prognamnque).
   FOR EACH intid:      
      PUT UNFORMATTED intid.TIN SKIP.        
   END.
   OUTPUT CLOSE.
  
   /*SKAPA POSTER*/
   INPUT FROM VALUE(prognamnque) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidin.
         ASSIGN.
         IMPORT DELIMITER "$" tidin   NO-ERROR.    
      END.               
   END.
   INPUT CLOSE.         
   FOR EACH tidin :
      FIND FIRST kollforetag WHERE kollforetag.ENHET = tidin.ENHET NO-ERROR.
      IF NOT AVAILABLE kollforetag THEN DELETE tidin.      
   END.               
   musz = FALSE.   
   /*DELA UPP POSERT TILL OLIKA FILER FÖR OLIKA DATABASER*/
   FOR EACH kollforetag:         
      utvar = startnamn + importvar + STRING(kollforetag.ENHET) + "." + filnamnslut. 
      OUTPUT TO VALUE(utvar) APPEND.
      FOR EACH tidin WHERE tidin.ENHET = kollforetag.ENHET:
         EXPORT tidin.
      END.
      OUTPUT CLOSE.
      IF OPSYS = "unix" THEN RUN oschmod_UI (INPUT utvar).
   END.   
END PROCEDURE.        


PROCEDURE skapkund_UI:   
   musz = FALSE.
   /*TA IN RÄTT POSTER TILL RÄTT DATABAS*/
   FOR EACH kollforetag:         
      utvar = startnamn + importvar + STRING(kollforetag.ENHET) + "." + filnamnslut.       
      kommando = SEARCH(utvar).
      IF kommando NE ? THEN DO:          
         RUN inkund_UI.
      END.
   END.
END PROCEDURE. 
PROCEDURE oschmod_UI:
   DEFINE INPUT PARAMETER kommandoextra AS CHARACTER FORMAT "X(132)" NO-UNDO.
   IF OPSYS NE "UNIX" THEN RETURN.
   kommandoextra = "chmod 777 " + kommandoextra.  
   OS-COMMAND VALUE(kommandoextra).     
END PROCEDURE.

PROCEDURE inkund_UI:
   /*TA IN RÄTT POSTER TILL RÄTT DATABAS*/
   EMPTY TEMP-TABLE tidin NO-ERROR.    
   INPUT FROM VALUE(kommando).
   REPEAT:
      DO TRANSACTION: 
         CREATE tidin.
         ASSIGN.
         IMPORT tidin NO-ERROR.
      END.               
   END.
   INPUT CLOSE.
   FIND FIRST tidin WHERE tidin.ENHET = kollforetag.ENHET NO-LOCK NO-ERROR.
   IF AVAILABLE tidin THEN DO:
      RUN con_UI.             
      IF CONNECTED(valdbtemp.DBNAMN) THEN DO:
         IF startfore = "GRANINGE" THEN DO:
            RUN GRSKAPKU.P (INPUT tidin.ENHET,INPUT TABLE tidin).           
         END.
         FOR EACH tidin WHERE tidin.ENHET = kollforetag.ENHET:
            DELETE tidin.
         END.    
         OS-DELETE VALUE(kommando).
         {DELALIAS.I}
         DISCONNECT VALUE(kollforetag.DATABAS) NO-ERROR.
      END.              
   END.
   ELSE OS-DELETE VALUE(kommando).      
   musz = FALSE.
END PROCEDURE.   
PROCEDURE con_UI :
   DEFINE VARIABLE koppla AS CHARACTER NO-UNDO.
   {VALDBALL.I}
   FIND FIRST valdbtemp WHERE valdbtemp.DBNAMN = kollforetag.DATABAS.
   koppla = valdbtemp.DBCON.
   CONNECT VALUE(koppla) NO-ERROR. 
   {VERALIAS.I}   
END PROCEDURE. 
PROCEDURE skapfore_UI :
   DEFINE INPUT PARAMETER varfor AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER vardb  AS CHARACTER NO-UNDO.
   CREATE kollforetag. 
   ASSIGN
   kollforetag.ENHET = varfor 
   kollforetag.DATABAS = vardb.   
END PROCEDURE.
