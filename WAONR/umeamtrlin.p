/*UMEAMTRLIN.P KÖRS SEPARAT*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}

DEFINE VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE VARIABLE skick AS LOGICAL NO-UNDO.
DEFINE VARIABLE efel AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE servervar AS CHARACTER LABEL "Smtp Server" NO-UNDO.
DEFINE VARIABLE franvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE tillvar AS CHARACTER NO-UNDO.
ASSIGN
tillvar = "ture.ostberg@umeaenergi.se"
franvar = "guru@umeaenergi.se"
servervar = "oden.umeaenergi.se".
{SMTPDEF3.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
RUN STYRFORE.P (INPUT FORETAG.FORETAG).
DEFINE TEMP-TABLE mtrlin NO-UNDO
   FIELD ART1 AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD BETECK AS CHARACTER
   FIELD ENHET AS CHARACTER
   FIELD NPRIS AS DECIMAL
   FIELD BPRIS AS DECIMAL
   FIELD KATEGORI AS CHARACTER
   FIELD MSTATUS AS INTEGER           /*1 = NY,2 = ÄNDRA,3 = ERSÄTTER 4 = BORTTAG*/
   FIELD ART2 AS CHARACTER
   FIELD ARTIKELGRUPP AS CHARACTER
   INDEX ART1 ART1.
DEFINE TEMP-TABLE mtrltemp NO-UNDO                         
   FIELD ART1 AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD ENHET AS CHARACTER
   FIELD NPRIS AS DECIMAL
   FIELD BPRIS AS DECIMAL
   FIELD KATEGORI AS CHARACTER
   FIELD MSTATUS AS INTEGER           /*1 = NY,2 = ÄNDRA,3 = ERSÄTTER 4 = BORTTAG*/
   FIELD ART2 AS CHARACTER
   FIELD ARTIKELGRUPP AS CHARACTER
   INDEX ART1 ART1.
   
DEFINE TEMP-TABLE eposttemp NO-UNDO
   FIELD EPOST AS CHARACTER
   FIELD MEDD AS CHARACTER
   INDEX EPOST EPOST.
DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE VARIABLE prognamnvar AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE prognamnkopia AS CHARACTER NO-UNDO. 
DEFINE VARIABLE loggfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE filnamnstart AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER NO-UNDO.
{AMERICANEUROPEAN.I}
{NAMNDB.I}
loggfil     = "D:\delad\PRO9S\autotid.txt". 
prognamnvar = "D:\delad\PRO9S\IMPORT\". 
prognamnkopia = "D:\delad\PRO9s\IMKOPIA\".
filnamnstart = "XALART".
kommando = "DIR/a:-d /b " + prognamnvar + filnamnstart + "*.* > " + prognamnvar + "GURUMIN.TXT".   
kommandoprog = prognamnvar + "GURUMIN.TXT".
IF namndb() = "UTBI" THEN RETURN.
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
   PUT "DET FANNS INGEN FIL FRÅN MTRL XAL  " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
   RETURN.
END.
FOR EACH infil:  
   EMPTY TEMP-TABLE mtrlin NO-ERROR. 
   EMPTY TEMP-TABLE mtrltemp NO-ERROR. 
   RUN in_UI (INPUT prognamnvar + infil.PROGNAMN).      
   OS-RENAME VALUE(prognamnvar + infil.PROGNAMN) VALUE(prognamnkopia + infil.PROGNAMN).
   RUN ut_UI.
   RUN mtrl_UI.
END.
RUN emedd_UI.
{EUROPEANAMERICAN.I}
PROCEDURE emedd_UI.
   DEFINE VARIABLE ctillvar   AS CHARACTER LABEL "Kopia" NO-UNDO.
   FOR EACH eposttemp:  
      ASSIGN 
      mailhub             = servervar     
      EmailTo             = eposttemp.EPOST 
      EmailFrom           = franvar
      EmailCC             = "jan.eriksson@umeaenergi.se"
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


PROCEDURE mtrl_UI :
   FOR EACH mtrltemp:
     DO TRANSACTION:
        IF mtrltemp.MSTATUS = 0 OR mtrltemp.MSTATUS = ? THEN mtrltemp.MSTATUS = 2.
        IF mtrltemp.MSTATUS = 1 THEN DO:        
           /*nytt mtrl*/
           FIND FIRST MTRL WHERE MTRL.ENR = mtrltemp.ART1 AND MTRL.LEVKOD = "99" EXCLUSIVE-LOCK NO-ERROR.
           IF NOT AVAILABLE MTRL THEN CREATE MTRL.
           RUN sparmtrl_UI.           
        END.
        ELSE IF mtrltemp.MSTATUS = 2 THEN DO:
           /*ändrad mtrl*/
           FIND FIRST MTRL WHERE MTRL.ENR = mtrltemp.ART1 AND MTRL.LEVKOD = "1" EXCLUSIVE-LOCK NO-ERROR.
           IF NOT AVAILABLE MTRL THEN DO:
              FIND FIRST MTRL WHERE MTRL.ENR = mtrltemp.ART1 AND MTRL.LEVKOD = "99" EXCLUSIVE-LOCK NO-ERROR.
           END.           
           IF NOT AVAILABLE MTRL THEN CREATE MTRL.
           RUN sparmtrl_UI.           
        END.
        ELSE IF mtrltemp.MSTATUS = 3 THEN DO:
           /*ersätter mtrl*/
           FIND FIRST MTRL WHERE MTRL.ENR = mtrltemp.ART2 AND MTRL.LEVKOD = "1" EXCLUSIVE-LOCK NO-ERROR.
           IF NOT AVAILABLE MTRL THEN DO:
              FIND FIRST MTRL WHERE MTRL.ENR = mtrltemp.ART2 AND MTRL.LEVKOD = "99" EXCLUSIVE-LOCK NO-ERROR.
           END.           
           IF NOT AVAILABLE MTRL THEN CREATE MTRL.
           RUN sparmtrl_UI.           
      
        END.
        ELSE IF mtrltemp.MSTATUS = 4 THEN DO:
           /*bort mtrl*/
           FIND FIRST MTRL WHERE MTRL.ENR = mtrltemp.ART1 AND MTRL.LEVKOD = "1" EXCLUSIVE-LOCK NO-ERROR.
           IF NOT AVAILABLE MTRL THEN DO:
              FIND FIRST MTRL WHERE MTRL.ENR = mtrltemp.ART1 AND MTRL.LEVKOD = "99" EXCLUSIVE-LOCK NO-ERROR.
           END.           
           IF AVAILABLE MTRL THEN DO:
              DELETE MTRL.
           END.
           FIND FIRST MTRL WHERE MTRL.ENR = mtrltemp.ART1 AND MTRL.LEVKOD = "5" EXCLUSIVE-LOCK NO-ERROR.
           IF AVAILABLE MTRL THEN DO:
              DELETE MTRL.
           END.
        END.
     END.
     
      
   END.
END PROCEDURE.

PROCEDURE sparmtrl_UI:
   ASSIGN
   MTRL.Benamning = mtrltemp.BENAMNING     
   MTRL.Enr       = mtrltemp.ART1
   MTRL.Enhet     = LC(mtrltemp.ENHET)
   MTRL.BPRIS     = mtrltemp.BPRIS
   MTRL.KALKNR    = 0
   MTRL.NPRIS     = mtrltemp.NPRIS
   MTRL.LEVKOD    = "1".
   {MTRLCREATE.I}
   IF mtrltemp.KATEGORI = "GURU" THEN DO:
      IF mtrltemp.MSTATUS = 3 THEN DO:
         FIND FIRST MTRL WHERE MTRL.ENR = mtrltemp.ART2 AND MTRL.LEVKOD = "5" EXCLUSIVE-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST MTRL WHERE MTRL.ENR = mtrltemp.ART1 AND MTRL.LEVKOD = "5" EXCLUSIVE-LOCK NO-ERROR.
      END.      
      IF NOT AVAILABLE MTRL THEN CREATE MTRL.
      ASSIGN
      MTRL.Benamning = mtrltemp.BENAMNING     
      MTRL.Enr       = mtrltemp.ART1
      MTRL.Enhet     = LC(mtrltemp.ENHET)
      MTRL.BPRIS     = mtrltemp.BPRIS
      MTRL.KALKNR    = 0
      MTRL.NPRIS     = mtrltemp.NPRIS
      MTRL.LEVKOD    = "5".
       {MTRLCREATE.I}
   END.
END PROCEDURE.

PROCEDURE in_UI :
   DEFINE INPUT PARAMETER sokvar AS CHARACTER NO-UNDO.
   INPUT FROM VALUE(sokvar).
   REPEAT:
      CREATE mtrlin.
      ASSIGN.
      IMPORT DELIMITER "|" mtrlin .        
   END.
END PROCEDURE.

PROCEDURE ut_UI :
   DEFINE VARIABLE mtrstatus AS CHARACTER NO-UNDO.
   FOR EACH mtrlin:   
      IF mtrlin.ART1 = "" THEN DELETE mtrlin.
      ELSE DO:
         CREATE mtrltemp.
         BUFFER-COPY mtrlin TO mtrltemp.
         mtrltemp.ENHET = LC(mtrltemp.ENHET).
         ASSIGN 
         mtrltemp.BENAMNING = mtrlin.BENAMNING + " " +  mtrlin.BETECK.          
         FIND FIRST eposttemp WHERE eposttemp.EPOST = tillvar AND 
         LENGTH(eposttemp.MEDD,"CHARACTER") < 30000
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE eposttemp THEN DO:
            CREATE eposttemp.
            eposttemp.EPOST = tillvar.
            eposttemp.MEDD = STRING(TODAY) + " FÖRÄNDRAD MATERIEL" + CHR(10).      
         END.
         mtrstatus = " Ny".
         
         IF mtrltemp.MSTATUS = 1 THEN mtrstatus = " Ny".
         ELSE IF mtrltemp.MSTATUS = 2 THEN mtrstatus = " Ändrad".
         ELSE IF mtrltemp.MSTATUS = 3 THEN mtrstatus = " Ersätter " + mtrltemp.ART2.
         ELSE IF mtrltemp.MSTATUS = 4 THEN mtrstatus = " Tas bort".
         
         eposttemp.MEDD = eposttemp.MEDD + mtrltemp.ART1 + " " + mtrltemp.BENAMNING + mtrstatus + CHR(10).         
      END.
      
   END.

END PROCEDURE.
