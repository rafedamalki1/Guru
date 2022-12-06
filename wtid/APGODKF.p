/*APGODKF.P VIA TIDSEDELPROGRAM FARDIG FOR GODKANNANDE*/

&Scoped-define NEW NEW

{REGVAR.I}
{GODKANUT.I}
{OTIDBEORD.I}
{PERSOTB.I}
DEFINE VARIABLE arregvnr AS INTEGER NO-UNDO.
DEFINE VARIABLE istrans AS LOGICAL INITIAL YES.
DEFINE TEMP-TABLE eposttemp NO-UNDO
   FIELD EPOST AS CHARACTER
   FIELD MEDD AS CHARACTER
   INDEX EPOST EPOST.
DEFINE VARIABLE vpers AS CHARACTER NO-UNDO.
DEFINE VARIABLE otbeordapph AS HANDLE NO-UNDO.
DEFINE VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE VARIABLE skick AS LOGICAL NO-UNDO.
DEFINE VARIABLE efel AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE servervar AS CHARACTER LABEL "Smtp Server" NO-UNDO.
DEFINE VARIABLE franvar AS CHARACTER NO-UNDO.
{SMTPDEF3.I}
FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.
DEBUGGER:SET-BREAK().
Guru.Konstanter:globforetag = FORETAG.FORETAG.
RUN STYRFORE.P (INPUT FORETAG.FORETAG).
IF FORETAG.FORETAG  = "SUND" OR FORETAG.FORETAG = "MISV" THEN DO:
   ASSIGN
   franvar = "NOREPLY"
   servervar = CHR(49) + CHR(55) + CHR(50) + CHR(46) + CHR(49) + CHR(54) + CHR(46) + CHR(50) + CHR(53) + CHR(52) + CHR(46) + CHR(50) + CHR(50) + CHR(50). 
END.
ELSE IF FORETAG.FORETAG = "SNAT" THEN DO:
   {SMTPFRANELPOOL.I}
END.
ELSE DO:
   ASSIGN
   franvar = "elpool.ume@elpool.se"
   servervar = CHR(115) + CHR(109) + CHR(116) + CHR(112) + CHR(46) + CHR(116) + CHR(101) + CHR(108) + CHR(101) + CHR(99) + CHR(111) + CHR(109) + CHR(51) + CHR(46) + CHR(110) + CHR(101) + CHR(116) .
END.
   
FOR EACH appmarkpers USE-INDEX PERSONALKOD NO-LOCK.
   ASSIGN
   regar = YEAR(appmarkpers.DATUM)   
   regmnr = MONTH(appmarkpers.DATUM)
   regdatum = appmarkpers.DATUM.
   IF gvisatidpermanad = TRUE THEN DO:            
      OPEN QUERY tidq FOR EACH TIDREGITAB WHERE 
      TIDREGITAB.PERSONALKOD = appmarkpers.PERSONALKOD AND
      YEAR(TIDREGITAB.DATUM) = regar AND MONTH(TIDREGITAB.DATUM) = regmnr 
      AND TIDREGITAB.GODKAND = "" 
      NO-LOCK.
      GET FIRST tidq NO-LOCK.      
      IF NOT AVAILABLE TIDREGITAB THEN DO:
         RUN god_UI.
         NEXT.      
      END.
      ELSE RUN tidgodk_UI.      
   END.   
   ELSE DO:
      OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = appmarkpers.PERSONALKOD AND
      TIDREGITAB.VECKONUMMER = appmarkpers.VECKONUMMER  AND 
      TIDREGITAB.GODKAND = ""
      USE-INDEX PVNR NO-LOCK.                       
      GET FIRST tidq NO-LOCK.
      IF NOT AVAILABLE TIDREGITAB THEN DO:   
         NEXT.
      END.
      ELSE RUN tidgodk_UI.             
   END.
   CLOSE QUERY tidq.   
END.
IF FORETAG.FORETAG = "csnat" THEN DO:
   RUN kolloverbeord_UI.
END.
       
PROCEDURE god_UI:   
   DO TRANSACTION:               
      FIND FIRST GODKOLL WHERE 
      GODKOLL.PERSONALKOD = appmarkpers.PERSONALKOD AND  
      GODKOLL.DATAR = regar AND GODKOLL.DATMAN = regmnr 
      USE-INDEX PKODAR EXCLUSIVE-LOCK NO-ERROR.             
      IF NOT AVAILABLE GODKOLL THEN DO:
         CREATE GODKOLL.
      END.
      ASSIGN
      GODKOLL.ANVANDARE = globanv + STRING(TODAY) + STRING(TIME,"HH:MM")              
      GODKOLL.PERSONALKOD = appmarkpers.PERSONALKOD
      GODKOLL.DATAR = regar 
      GODKOLL.DATMAN = regmnr.                   
      IF GODKOLL.DATUM <= regdatum OR GODKOLL.DATUM = ? THEN DO:                  
         ASSIGN
         GODKOLL.DATUM = regdatum            
         GODKOLL.VECKONUMMER = regvnr.
      END.               
      IF MONTH(GODKOLL.DATUM) = MONTH(GODKOLL.DATUM + 1) THEN GODKOLL.KLAR = FALSE. 
      ELSE GODKOLL.KLAR = TRUE.    
   END.  
END PROCEDURE.

PROCEDURE tidgodk_UI:   
   IF gvisatidpermanad = TRUE THEN DO:
      DO TRANSACTION:
         GET FIRST tidq EXCLUSIVE-LOCK.         
         IF AVAILABLE TIDREGITAB THEN DO:
            IF TIDREGITAB.DATUM <= regdatum THEN DO:               
               ASSIGN TIDREGITAB.GODKAND = "F".
            END.
         END.
      END.
      DO WHILE AVAILABLE(TIDREGITAB):         
         DO TRANSACTION:
            GET NEXT tidq EXCLUSIVE-LOCK.
            IF AVAILABLE TIDREGITAB THEN DO:
               IF TIDREGITAB.DATUM <= regdatum THEN DO:
                  ASSIGN TIDREGITAB.GODKAND = "F".
               END.
            END.
         END.  
      END.
      RUN god_UI.      
   END.
   ELSE DO: 
      DO TRANSACTION:
         GET FIRST tidq EXCLUSIVE-LOCK.
         IF AVAILABLE TIDREGITAB THEN DO:
            IF TIDREGITAB.DATUM <= regdatum THEN DO:
               ASSIGN TIDREGITAB.GODKAND = "F".
            END.
         END.
      END.
      DO WHILE AVAILABLE(TIDREGITAB):
         DO TRANSACTION:
            GET NEXT tidq EXCLUSIVE-LOCK.
            IF AVAILABLE TIDREGITAB THEN DO:
               IF TIDREGITAB.DATUM <= regdatum THEN DO:
                  ASSIGN TIDREGITAB.GODKAND = "F".
               END.
            END.
         END.  
      END.
   END.        
   /* Det måste vara vecka som hör till regdatum - alltså ej appmarkpers.VECKONUMMER*/
   RUN REGVEC.P.   
   IF YEAR(regdatum) < 2001 THEN arregvnr = regvnr.
   ELSE arregvnr = INTEGER(SUBSTRING(STRING(YEAR(regdatum),"9999"),1,3) + STRING(regvnr,"999")).   
   FIND FIRST VECKOARBAV WHERE 
   VECKOARBAV.PERSONALKOD = appmarkpers.PERSONALKOD AND
   VECKOARBAV.VECKONUMMER = arregvnr 
   USE-INDEX PVNUMMER NO-LOCK NO-ERROR.
   IF NOT AVAILABLE VECKOARBAV THEN DO TRANSACTION:      
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = appmarkpers.PERSONALKOD
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.   
      {RULLVECKO.I}       
      CREATE VECKOARBAV.
      ASSIGN VECKOARBAV.PERSONALKOD = appmarkpers.PERSONALKOD
      VECKOARBAV.VECKONUMMER = arregvnr
      VECKOARBAV.VECKOSCHEMA = rull-veckovar.
   END.
END PROCEDURE.

PROCEDURE kolloverbeord_UI :
   EMPTY TEMP-TABLE persotb NO-ERROR. 
   FOR EACH appmarkpers USE-INDEX PERSONALKOD NO-LOCK,      
   EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = appmarkpers.PERSONALKOD AND TIDREGITAB.GODKAND BEGINS "F" AND TIDREGITAB.TIDLOG = TRUE NO-LOCK.           
      IF SUBSTRING(TIDREGITAB.RESMAL,159,6) NE "" THEN DO:
         CREATE persotb.
         ASSIGN
         persotb.PERSONALKOD = TIDREGITAB.PERSONALKOD
         persotb.DATUM = TIDREGITAB.DATUM
         persotb.START = TIDREGITAB.START
         persotb.SLUT = TIDREGITAB.SLUT
         persotb.TOTALT = TIDREGITAB.TOTALT
         persotb.AONR = TIDREGITAB.AONR
         persotb.DELNR = TIDREGITAB.DELNR
         persotb.GODKAND       = TIDREGITAB.GODKAND
         persotb.OTB = SUBSTRING(TIDREGITAB.RESMAL,159,6)
         persotb.KOMMENTAR = SUBSTRING(TIDREGITAB.RESMAL,1,158).         
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = TIDREGITAB.PERSONALKOD  NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALTAB THEN DO:
            ASSIGN
            persotb.FORNAMN = PERSONALTAB.FORNAMN
            persotb.EFTERNAMN = PERSONALTAB.EFTERNAMN
            persotb.GODKANNARE = PERSONALTAB.TIDSGODK.
         END.
      END.            
   END.

   RUN OVERTIDBRDAPP.P PERSISTENT SET otbeordapph.   
   RUN othmt_UI IN otbeordapph (OUTPUT TABLE otidbeordtemp).
   
   EMPTY TEMP-TABLE eposttemp NO-ERROR.
   
   FOR EACH persotb WHERE /*persotb.OTB = otidbeordtemp.PERSONALKOD*/ USE-INDEX OTB.
      FIND FIRST otidbeordtemp WHERE otidbeordtemp.PERSONALKOD = persotb.OTB NO-LOCK NO-ERROR.
      IF AVAILABLE otidbeordtemp THEN DO: 
         IF otidbeordtemp.PERSONALKOD = persotb.GODKANNARE THEN .
         ELSE DO:   
            FIND FIRST eposttemp WHERE eposttemp.EPOST = otidbeordtemp.EPOST AND 
            LENGTH(eposttemp.MEDD,"CHARACTER") < 30000 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE eposttemp THEN DO:
               CREATE eposttemp.
               eposttemp.EPOST = otidbeordtemp.EPOST.
               eposttemp.MEDD = " Övertid beordrad av: " + otidbeordtemp.FORNAMN + " " + otidbeordtemp.EFTERNAMN + "       gällande körning utförd " + SUBSTRING(VECKONATT.VECKOKORD,2,8)  + CHR(10)
                              + " ========================================================================== " + CHR(10).       
            END.
            eposttemp.MEDD = eposttemp.MEDD + persotb.PERSONALKOD  + " " + STRING(persotb.DATUM,"9999/99/99") + " " + STRING(persotb.START,">9.99")  + " " + STRING(persotb.SLUT,">9.99")  + " " + STRING(persotb.TOTALT,">9.99") +
            " timmar " + persotb.AONR + " " + STRING(persotb.DELNR,">99") + " " + SUBSTRING(persotb.FORNAMN,1,1) + "." + persotb.EFTERNAMN +  CHR(10) + persotb.KOMMENTAR  + CHR(10) + CHR(10).         
         END.
         IF AVAILABLE eposttemp THEN Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + eposttemp.EPOST + "," +  eposttemp.MEDD .
      END.
   END.    
   Guru.GlobalaVariabler:GDPRtyp = "EP". 
   {GDPRLOGGCLIENT.I}   
   RUN emedd_UI.
   
END PROCEDURE.

PROCEDURE emedd_UI.
   DEFINE VARIABLE ctillvar AS CHARACTER LABEL "Kopia" NO-UNDO.
   ctillvar  = "".   
   FOR EACH eposttemp:  
      FIND FIRST otidbeordtemp WHERE otidbeordtemp.EPOST = eposttemp.EPOST  NO-LOCK NO-ERROR.         
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = otidbeordtemp.PERSONALKOD NO-LOCK NO-ERROR.
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
      FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
      FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
      IF AVAILABLE JURPERS THEN DO:
         IF JURPERS.JUDID = "SEAB" THEN ctillvar = "Katharina.Wikstrom@sundsvallenergi.se".         
         IF JURPERS.JUDID = "ELNÄT" THEN ctillvar = "victoria.rosengren@sundsvallelnat.se".
         IF JURPERS.JUDID = "ServaNet" THEN ctillvar = "victoria.rosengren@sundsvallelnat.se".
      END.
      ASSIGN 
      mailhub             = servervar     
      EmailTo             = eposttemp.EPOST 
      EmailFrom           = franvar
      EmailCC             = ctillvar
      Attachmentstyp      = ""
      LocalFiles          = ""
      Subject             = "Övertidslista" 
      Bodysmtp            = eposttemp.MEDD
      MIMEHeader          = "type=text/plain/html:charset=iso-8859-1:filetype=ascii"
      BodyType            = "".
      IF FORETAG.FORETAG  = "sund"  THEN EmailFrom = "webguru@sundsvallenergi.se".
      IF FORETAG.FORETAG = "SNAT" THEN EmailFrom = "@guru.sundsvallelnat.se".      
      IF FORETAG.FORETAG = "MISV" THEN EmailFrom = "webguru@mittsverigevatten.se".
      RUN smtpmail_UI (INPUT FALSE).
      IF oSuccessful = TRUE THEN DO TRANSACTION:
         oSuccessful = FALSE.               
      END.      
      ELSE DO:
         IF FORETAG.FORETAG = "SUND"  THEN DO:
            OUTPUT TO D:\delad\server\pro10s\EXPORT\lon\obeordfel.txt APPEND.
            PUT UNFORMATTED TODAY " " vMessage " " servervar " " eposttemp.EPOST " " EmailFrom SKIP.
         END.
         ELSE IF FORETAG.FORETAG = "SNAT" THEN DO: 
            /*SNATBERGET*/
            OUTPUT TO D:\DELAD\PRO10S\obeordfel.txt APPEND.           
            PUT UNFORMATTED TODAY " " vMessage " " servervar " " eposttemp.EPOST " " EmailFrom SKIP.
         END.
         ELSE IF FORETAG.FORETAG = "MISV" THEN DO:            
            OUTPUT TO D:\elpool\delad\pro10s\EXPORT\lon\obeordfel.txt APPEND.
            
            PUT UNFORMATTED TODAY " " vMessage " " servervar " " eposttemp.EPOST " " EmailFrom SKIP.
         END.
         OUTPUT CLOSE.
      END.
   END.     
END PROCEDURE.  

 
   

