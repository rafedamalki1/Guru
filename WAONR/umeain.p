/*umeain.p KÖRS SEPARAT*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}

{NAMNDB.I}
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE VARIABLE skick AS LOGICAL NO-UNDO.
DEFINE VARIABLE efel AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE servervar AS CHARACTER LABEL "Smtp Server" NO-UNDO.
DEFINE VARIABLE franvar AS CHARACTER NO-UNDO.
IF namndb() = "UTBI" THEN RETURN.
ASSIGN
franvar = "guru@umeaenergi.se"
servervar = "oden.umeaenergi.se".
{SMTPDEF3.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
RUN STYRFORE.P (INPUT FORETAG.FORETAG).
DEFINE TEMP-TABLE aonrin NO-UNDO
   FIELD AONRCHAR AS CHARACTER
   FIELD ORT AS CHARACTER
   FIELD SIGN AS CHARACTER
   FIELD EPOST AS CHARACTER
   INDEX AONRCHAR AONRCHAR.
DEFINE TEMP-TABLE aonrtemp NO-UNDO
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD ORT AS CHARACTER
   FIELD SIGN AS CHARACTER
   FIELD EPOST AS CHARACTER
   INDEX AONR AONR DELNR
   INDEX EPOST EPOST.
   
DEFINE TEMP-TABLE eposttemp NO-UNDO
   FIELD EPOST AS CHARACTER
   FIELD MEDD AS CHARACTER
   INDEX EPOST EPOST.
DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
{SOKDEF.I}
DEFINE VARIABLE aonrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE delnrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamnvar AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE prognamnkopia AS CHARACTER NO-UNDO. 
DEFINE VARIABLE loggfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE filnamnstart AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER NO-UNDO.
DEFINE VARIABLE globanv AS CHARACTER NO-UNDO.
{AMERICANEUROPEAN.I}
loggfil     = "D:\delad\PRO9S\autotid.txt". 
prognamnvar = "D:\delad\PRO9S\IMPORT\". 
prognamnkopia = "D:\delad\PRO9s\IMKOPIA\".
filnamnstart = "XALPROJ".
kommando = "DIR/a:-d /b " + prognamnvar + filnamnstart + "*.* > " + prognamnvar + "GURUIN.TXT".   
kommandoprog = prognamnvar + "GURUIN.TXT".
OS-DELETE VALUE(kommandoprog) NO-ERROR.
OS-COMMAND SILENT VALUE(kommando).
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
   IF INDEX(infil.PROGNAMN,filnamnstart) = 0 THEN DO:       
      DELETE infil.
      NEXT.
   END.
END.
FIND FIRST infil NO-ERROR.
IF NOT AVAILABLE infil THEN DO:

   OUTPUT TO  VALUE(loggfil)  APPEND.
   PUT "DET FANNS INGEN FIL FRÅN XAL MED AONR  " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
   RETURN.
END.
FOR EACH infil:  
   EMPTY TEMP-TABLE aonrin NO-ERROR. 
   EMPTY TEMP-TABLE aonrtemp NO-ERROR. 
   RUN in_UI (INPUT prognamnvar + infil.PROGNAMN).      
   OS-RENAME VALUE(prognamnvar + infil.PROGNAMN) VALUE(prognamnkopia + infil.PROGNAMN).
   RUN ut_UI.
   RUN aonr_UI.
END.
RUN emedd_UI.
{EUROPEANAMERICAN.I}
PROCEDURE emedd_UI.
   DEFINE VARIABLE ctillvar   AS CHARACTER LABEL "Kopia" NO-UNDO.
   FOR EACH eposttemp WHERE eposttemp.EPOST NE "":  
      ASSIGN 
      mailhub             = servervar     
      EmailTo             = eposttemp.EPOST 
      EmailFrom           = franvar
      EmailCC             = ""
      Attachmentstyp      = ""
      LocalFiles          = ""
      Subject             = "Nya " + LC("projnr") + " i Guru"
      Bodysmtp            = eposttemp.MEDD
      MIMEHeader          = "type=text/html:charset=iso-8859-1:filetype=ascii"
      BodyType            = "".
      
      RUN smtpmail_UI (INPUT FALSE).
      IF oSuccessful = TRUE THEN DO TRANSACTION:
         oSuccessful = FALSE.               
      END.            
   END.     
END PROCEDURE.  


PROCEDURE aonr_UI :
   FOR EACH aonrtemp:
      /*ENDAST NYA PROJ*/
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = aonrtemp.AONR AND AONRTAB.DELNR = aonrtemp.DELNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE AONRTAB THEN DO:
         RUN aoskap_UI (INPUT aonrtemp.DELNR,INPUT aonrtemp.ORT).
      END.
      IF aonrtemp.DELNR NE 0 THEN DO:
         aonrtemp.DELNR = 0.
         IF SUBSTRING(aonrtemp.ORT,1,3) = "HSP" THEN aonrtemp.ORT = SUBSTRING(aonrtemp.ORT,5).
         ELSE IF SUBSTRING(aonrtemp.ORT,1,3) = "LSP" THEN aonrtemp.ORT = SUBSTRING(aonrtemp.ORT,5).
         ELSE IF SUBSTRING(aonrtemp.ORT,1,3) = "STN" THEN aonrtemp.ORT = SUBSTRING(aonrtemp.ORT,5).
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = aonrtemp.AONR AND AONRTAB.DELNR = aonrtemp.DELNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE AONRTAB THEN DO:
            RUN aoskap_UI (INPUT aonrtemp.DELNR,INPUT aonrtemp.ORT).
         END.
         ELSE DO TRANSACTION:
            FIND CURRENT AONRTAB EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN              
            AONRTAB.STARTDAG = aonrtemp.SIGN 
            AONRTAB.BEREDARE = aonrtemp.SIGN                                
            AONRTAB.ARBANSVARIG = aonrtemp.SIGN. 
         END.
      END.
   END.
END PROCEDURE.
PROCEDURE aoskap_UI :
   DEFINE INPUT PARAMETER dlnrvar  AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER ortvar AS CHARACTER NO-UNDO.
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = aonrtemp.SIGN NO-LOCK NO-ERROR.
   FIND FIRST ANVANDARE WHERE ANVANDARE.PERSONALKOD = aonrtemp.SIGN NO-LOCK NO-ERROR.         
   DO TRANSACTION:
      CREATE AONRTAB.
      aonrrec = RECID(AONRTAB).
      ASSIGN              
      AONRTAB.STARTDAG = aonrtemp.SIGN 
      AONRTAB.PRISTYP = "TOT.PRIS."                
      AONRTAB.ORT = ortvar
      AONRTAB.FASTAAONR = FALSE                         
      AONRTAB.DELNR   = dlnrvar
      AONRTAB.BEREDARE = aonrtemp.SIGN                                
      AONRTAB.ARBANSVARIG = aonrtemp.SIGN 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      AONRTAB.AONR        = aonrtemp.AONR.
      IF AVAILABLE PERSONALTAB THEN DO: 
         ASSIGN
         AONRTAB.UTFARDAT = PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN
         AONRTAB.OMRADE = PERSONALTAB.OMRADE
         AONRTAB.BESTID = PERSONALTAB.OMRADE.
      END.
      ELSE DO:
         AONRTAB.AONRAVDATUM = 01/01/1989. 
      END.
      IF AVAILABLE ANVANDARE THEN DO:
         globanv = ANVANDARE.ANVANDARE.
      END.
      ELSE globanv = "XAL".
      /*SKALL VARA XAL*/
      globanv = "XAL".
      aonrvar = AONRTAB.AONR.
      delnrvar = AONRTAB.DELNR.
      {FORETIDL.I}
   END.
END PROCEDURE.
PROCEDURE in_UI :
   DEFINE INPUT PARAMETER sokvar AS CHARACTER NO-UNDO.
   INPUT FROM VALUE(sokvar).
   REPEAT:
      CREATE aonrin.
      ASSIGN.       
      IMPORT DELIMITER "|" aonrin .        
   END.
END PROCEDURE.

PROCEDURE ut_UI :
   FOR EACH aonrin:   
      IF SUBSTRING(aonrin.AONRCHAR,1,6) = "" THEN.
      ELSE DO:
         CREATE aonrtemp.
         BUFFER-COPY aonrin TO aonrtemp.
         ASSIGN
         aonrtemp.AONR =  SUBSTRING(aonrin.AONRCHAR,1,6).
         IF SUBSTRING(aonrin.AONRCHAR,7,3)  = "" THEN DO:
            ASSIGN
            aonrtemp.DELNR = 0
            aonrtemp.ORT = aonrin.ORT.
         END.
         ELSE IF  SUBSTRING(aonrin.AONRCHAR,7,3)  = "HSP" THEN DO:
            ASSIGN
            aonrtemp.DELNR = 1
            aonrtemp.ORT = SUBSTRING(aonrin.AONRCHAR,7,3) + " " + aonrin.ORT.
         END.
         ELSE IF  SUBSTRING(aonrin.AONRCHAR,7,3)  = "LSP" THEN DO:
            ASSIGN
            aonrtemp.DELNR = 2
            aonrtemp.ORT = SUBSTRING(aonrin.AONRCHAR,7,3) + " " + aonrin.ORT.
         END.
         ELSE IF  SUBSTRING(aonrin.AONRCHAR,7,3)  = "STN" THEN DO:
            ASSIGN
            aonrtemp.DELNR = 3
            aonrtemp.ORT = SUBSTRING(aonrin.AONRCHAR,7,3) + " " + aonrin.ORT.
         END.           
         FIND FIRST eposttemp WHERE eposttemp.EPOST = aonrtemp.EPOST AND 
         LENGTH(eposttemp.MEDD,"CHARACTER") < 30000
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE eposttemp THEN DO:
            CREATE eposttemp.
            eposttemp.EPOST = aonrtemp.EPOST.
            eposttemp.MEDD = STRING(TODAY) + " NYA " + CAPS("projnr") + CHR(10).      
         END.
         eposttemp.MED = eposttemp.MED + aonrtemp.AONR + " " +  
         STRING(aonrtemp.DELNR,Guru.Konstanter:varforetypchar[1]) + " " + aonrtemp.ORT +  CHR(10).         
      END.
   END.

END PROCEDURE.
