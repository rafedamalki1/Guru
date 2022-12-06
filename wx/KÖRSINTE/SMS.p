/*SMS.P*/
   
DEFINE VARIABLE globanv AS CHARACTER NO-UNDO.                           
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE datssling  AS INTEGER LABEL "SLINGA" NO-UNDO. 
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO. 



DEFINE SHARED VARIABLE infil AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE utfil AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE sparfil AS CHARACTER NO-UNDO.

DEFINE VARIABLE datstrakt AS INTEGER FORMAT "9" LABEL "TRAKTAMENTE" NO-UNDO.
DEFINE VARIABLE datsslut  AS INTEGER FORMAT "9999" LABEL "SLUT" NO-UNDO.
DEFINE VARIABLE datsstart AS INTEGER FORMAT "9999" LABEL "START" NO-UNDO.
DEFINE VARIABLE tidsek AS INTEGER NO-UNDO.
DEFINE VARIABLE sokfil AS CHARACTER NO-UNDO.

DEFINE SHARED TEMP-TABLE tidin2
   FIELD TIN AS CHARACTER FORMAT "X(256)".   
      
DEFINE NEW SHARED TEMP-TABLE tidtemp 
    FIELD FORETAG AS CHARACTER
    FIELD PERSONALKOD AS CHARACTER FORMAT "X(5)"
    FIELD AONR AS CHARACTER FORMAT "X(6)"
    FIELD DELNR AS INTEGER FORMAT "999"
    FIELD PRISTYP AS CHARACTER FORMAT "X(9)"     
    FIELD ANSTALLNING AS CHARACTER FORMAT "X(15)"     
    FIELD DATUM AS DATE  
    FIELD TRAKTAMENTE AS INTEGER FORMAT "9"
    FIELD SEKTID AS INTEGER
    FIELD START AS DECIMAL
    FIELD SLUT AS DECIMAL
    FIELD OVERTIDUTTAG AS CHARACTER FORMAT "X(1)"
    FIELD UTRYCKNING AS LOGICAL 
    FIELD PREC AS RECID
    FIELD TELNR AS CHARACTER FORMAT "X(15)"
    INDEX PKOD IS PRIMARY PERSONALKOD DATUM START SLUT AONR DELNR.      

DEFINE NEW SHARED TEMP-TABLE tidtemp2 
    FIELD FORETAG AS CHARACTER
    FIELD PERSONALKOD AS CHARACTER FORMAT "X(5)"
    FIELD AONR AS CHARACTER FORMAT "X(6)"
    FIELD DELNR AS INTEGER FORMAT "999"
    FIELD PRISTYP AS CHARACTER FORMAT "X(9)"     
    FIELD ANSTALLNING AS CHARACTER FORMAT "X(15)"     
    FIELD DATUM AS DATE  
    FIELD TRAKTAMENTE AS INTEGER FORMAT "9"
    FIELD SEKTID AS INTEGER
    FIELD START AS DECIMAL
    FIELD SLUT AS DECIMAL
    FIELD OVERTIDUTTAG AS CHARACTER FORMAT "X(1)"
    FIELD UTRYCKNING AS LOGICAL 
    FIELD PREC AS RECID
    FIELD TELNR AS CHARACTER FORMAT "X(15)"
    INDEX PKOD IS PRIMARY PERSONALKOD DATUM START SLUT AONR DELNR.      


DEFINE SHARED TEMP-TABLE tele_temp      
   FIELD PKOD LIKE TIDREGITAB.PERSONALKOD    
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR  
   FIELD DATUM AS DATE
   FIELD START LIKE TIDREGITAB.START
   FIELD SLUT LIKE TIDREGITAB.SLUT     
   FIELD TELNR LIKE PERSONALTAB.MOBILTEL 
   FIELD FRAGA AS CHARACTER 
   FIELD SVAR AS CHARACTER
   FIELD PRISTYP AS CHARACTER FORMAT "X(9)"
   FIELD ANSTALLNING AS CHARACTER FORMAT "X(15)"
   FIELD UTRYCKNING AS LOGICAL
   FIELD TRAKTAMENTE AS INTEGER FORMAT "9"
   FIELD OVERTIDUTTAG AS CHARACTER FORMAT "X(1)"
   FIELD PREC AS RECID
   FIELD ORGINAL AS LOGICAL 
   FIELD NUMMER  AS INTEGER
   FIELD FORETAG LIKE FORETAG.FORETAG
   FIELD TREC AS RECID
   INDEX FRAGA FRAGA SVAR FORETAG ASCENDING
   INDEX AONR IS PRIMARY AONR DELNR ASCENDING
   INDEX ORG ORGINAL NUMMER ASCENDING.      
   
DEFINE BUFFER telbuff FOR tele_temp.             
{AMERICANEUROPEAN.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
  
ASSIGN
tidsek = TIME  
regdatum = TODAY.
         
 /*NORMAL REGISTRERING*/
FIND FIRST tele_temp WHERE tele_temp.FRAGA = "1" AND
tele_temp.SVAR = "Registrering OK" AND tele_temp.FORETAG = Guru.Konstanter:globforetag
USE-INDEX FRAGA NO-LOCK NO-ERROR.
IF AVAILABLE tele_temp THEN DO:
   RUN normal_UI.
END.  

/*ÖVERTID*/
FIND FIRST tele_temp WHERE tele_temp.FRAGA = "2" AND
tele_temp.SVAR = "Registrering OK" AND tele_temp.FORETAG = Guru.Konstanter:globforetag 
USE-INDEX FRAGA NO-LOCK NO-ERROR.
IF AVAILABLE tele_temp THEN DO:
   RUN over_UI.
END.

/*NORMALTID ANNAT KLOCKSLAG*/
FIND FIRST tele_temp WHERE tele_temp.FRAGA = "3" AND
tele_temp.SVAR = "Registrering OK" AND tele_temp.FORETAG = Guru.Konstanter:globforetag 
USE-INDEX FRAGA NO-LOCK NO-ERROR.
IF AVAILABLE tele_temp THEN DO:
   RUN klock_UI.
END.

/*REGISTRERING FRAMÅT*/
FIND FIRST tele_temp WHERE tele_temp.FRAGA = "4" AND
tele_temp.SVAR = "Registrering OK" AND tele_temp.FORETAG = Guru.Konstanter:globforetag 
USE-INDEX FRAGA NO-LOCK NO-ERROR.
IF AVAILABLE tele_temp THEN DO:
   RUN datum_UI.
END.

/*KOLL VAD SOM ÄR REGISTRERAT*/
FIND FIRST tele_temp WHERE tele_temp.FRAGA = "9" AND
tele_temp.SVAR = "Registrering OK" AND tele_temp.FORETAG = Guru.Konstanter:globforetag 
USE-INDEX FRAGA NO-LOCK NO-ERROR.
IF AVAILABLE tele_temp THEN DO:
   RUN regkoll_UI.
END.

RUN ut_UI.
{EUROPEANAMERICAN.I}
PROCEDURE normal_UI :
   /*NORMALTID*/
   FOR EACH tele_temp WHERE tele_temp.FRAGA = "1" AND
   tele_temp.SVAR = "Registrering OK" AND tele_temp.FORETAG = Guru.Konstanter:globforetag 
   USE-INDEX FRAGA:
      ASSIGN
      datsslut = 0000
      datsstart = 0000
      regdatum = TODAY
      datssling = 0.
      CREATE tidtemp.                                                         
      ASSIGN     
      tidtemp.PERSONALKOD = tele_temp.PKOD 
      tidtemp.AONR = tele_temp.AONR 
      tidtemp.DELNR = tele_temp.DELNR 
      tidtemp.PRISTYP = tele_temp.PRISTYP
      tidtemp.ANSTALLNING = tele_temp.ANSTALLNING      
      tidtemp.DATUM = regdatum   
      tidtemp.TRAKTAMENTE = tele_temp.TRAKTAMENTE 
      tidtemp.SEKTID = tidsek
      tidtemp.START = datsstart / 100  
      tidtemp.SLUT = datsslut / 100
      tidtemp.OVERTIDUTTAG = tele_temp.OVERTIDUTTAG
      tidtemp.UTRYCKNING = tele_temp.UTRYCKNING
      tidtemp.PREC = tele_temp.PREC
      tidtemp.TELNR = tele_temp.TELNR.            
   END.
   globanv = "SMS".
   RUN DBCONECT.P (INPUT globanv).
   FOR EACH tidtemp:
      DELETE tidtemp.
   END.   
END PROCEDURE.

PROCEDURE over_UI :
   /*ÖVERTID*/
   FOR EACH tele_temp WHERE tele_temp.FRAGA = "2" AND
   tele_temp.SVAR = "Registrering OK" AND tele_temp.FORETAG = Guru.Konstanter:globforetag 
   USE-INDEX FRAGA:
      ASSIGN
      datsslut = 0000
      regdatum = TODAY
      datssling = 1.
      CREATE tidtemp.                                                         
      ASSIGN     
      tidtemp.PERSONALKOD = tele_temp.PKOD 
      tidtemp.AONR = tele_temp.AONR 
      tidtemp.DELNR = tele_temp.DELNR 
      tidtemp.PRISTYP = tele_temp.PRISTYP
      tidtemp.ANSTALLNING = tele_temp.ANSTALLNING      
      tidtemp.DATUM = tele_temp.DATUM   
      tidtemp.TRAKTAMENTE = tele_temp.TRAKTAMENTE 
      tidtemp.SEKTID = tidsek
      tidtemp.START = tele_temp.START  
      tidtemp.SLUT = tele_temp.SLUT      
      tidtemp.UTRYCKNING = tele_temp.UTRYCKNING
      tidtemp.PREC = tele_temp.PREC
      tidtemp.TELNR = tele_temp.TELNR. 
      IF tele_temp.OVERTIDUTTAG = "1" THEN tidtemp.OVERTIDUTTAG = "K".
      ELSE IF tele_temp.OVERTIDUTTAG = "0" THEN tidtemp.OVERTIDUTTAG = "Ö".
      ELSE IF tele_temp.OVERTIDUTTAG = "3" THEN tidtemp.OVERTIDUTTAG = "I".
      ELSE IF tele_temp.OVERTIDUTTAG = "2" THEN tidtemp.OVERTIDUTTAG = "L". 
      ELSE IF tele_temp.OVERTIDUTTAG = "" THEN tidtemp.OVERTIDUTTAG = "".           
   END.
   globanv = "SMS".
   RUN DBCONECT.P (INPUT globanv).
   FOR EACH tidtemp:
      DELETE tidtemp.
   END.   
END PROCEDURE.

PROCEDURE klock_UI :
   /*NORMALTID ANNAT KLOCKSLAG*/
   FOR EACH tele_temp WHERE tele_temp.FRAGA = "3" AND
   tele_temp.SVAR = "Registrering OK" AND tele_temp.FORETAG = Guru.Konstanter:globforetag
   USE-INDEX FRAGA:
      ASSIGN
      datsslut = 0000
      regdatum = TODAY
      datssling = 2.
      CREATE tidtemp.                                                         
      ASSIGN     
      tidtemp.PERSONALKOD = tele_temp.PKOD 
      tidtemp.AONR = tele_temp.AONR 
      tidtemp.DELNR = tele_temp.DELNR 
      tidtemp.PRISTYP = tele_temp.PRISTYP
      tidtemp.ANSTALLNING = tele_temp.ANSTALLNING      
      tidtemp.DATUM = regdatum   
      tidtemp.TRAKTAMENTE = tele_temp.TRAKTAMENTE 
      tidtemp.SEKTID = tidsek
      tidtemp.START = tele_temp.START  
      tidtemp.SLUT = datsslut / 100
      tidtemp.OVERTIDUTTAG = tele_temp.OVERTIDUTTAG
      tidtemp.UTRYCKNING = tele_temp.UTRYCKNING
      tidtemp.PREC = tele_temp.PREC
      tidtemp.TELNR = tele_temp.TELNR.            
   END.
   globanv = "SMS".
   RUN DBCONECT.P (INPUT globanv).
   FOR EACH tidtemp:
      DELETE tidtemp.
   END.   
END PROCEDURE.

PROCEDURE datum_UI :
   /*FRAMTID*/
   FOR EACH tele_temp WHERE tele_temp.FRAGA = "4" AND
   tele_temp.SVAR = "Registrering OK" AND tele_temp.FORETAG = Guru.Konstanter:globforetag 
   USE-INDEX FRAGA:
      ASSIGN
      datsslut = 0000
      datsstart = 0000
      regdatum = TODAY
      datssling = 3.
      CREATE tidtemp.                                                         
      ASSIGN     
      tidtemp.PERSONALKOD = tele_temp.PKOD 
      tidtemp.AONR = tele_temp.AONR 
      tidtemp.DELNR = tele_temp.DELNR 
      tidtemp.PRISTYP = tele_temp.PRISTYP
      tidtemp.ANSTALLNING = tele_temp.ANSTALLNING      
      tidtemp.DATUM = tele_temp.DATUM   
      tidtemp.TRAKTAMENTE = tele_temp.TRAKTAMENTE 
      tidtemp.SEKTID = tidsek
      tidtemp.START = datsstart / 100  
      tidtemp.SLUT = datsslut / 100
      tidtemp.OVERTIDUTTAG = tele_temp.OVERTIDUTTAG
      tidtemp.UTRYCKNING = tele_temp.UTRYCKNING
      tidtemp.PREC = tele_temp.PREC
      tidtemp.TELNR = tele_temp.TELNR.            
   END.
   globanv = "SMS".
   RUN DBCONECT.P (INPUT globanv).
   FOR EACH tidtemp:
      DELETE tidtemp.
   END.   
END PROCEDURE.


PROCEDURE regkoll_UI :
   /*Kontroll vad som är registrerat*/
   FOR EACH tele_temp WHERE tele_temp.FRAGA = "9" AND
   tele_temp.SVAR = "Registrering OK" AND tele_temp.FORETAG = Guru.Konstanter:globforetag 
   USE-INDEX FRAGA:
      ASSIGN
      datsslut = 0000
      datsstart = 0000
      regdatum = TODAY
      datssling = 9.
      CREATE tidtemp.                                                         
      ASSIGN     
      tidtemp.PERSONALKOD = tele_temp.PKOD   
      tidtemp.DATUM = tele_temp.DATUM.         
   END.
   globanv = "SMS".
   RUN DBCONECT.P (INPUT globanv).
   FOR EACH tidtemp:
      DELETE tidtemp.
   END.   
END PROCEDURE.

PROCEDURE ut_UI :     
   FOR EACH tele_temp WHERE tele_temp.ORGINAL = TRUE 
   AND tele_temp.FORETAG = Guru.Konstanter:globforetag USE-INDEX ORG:      
      OUTPUT TO VALUE(Guru.Konstanter:wtidvar) APPEND CONVERT TARGET "ibm850" SOURCE "iso8859-1" 
      NO-ECHO.           
      PUT UNFORMATTED        
      SUBSTRING(tele_temp.TELNR,1,12) + CHR(9) + 
      tele_temp.SVAR + CHR(13) /*CHR(10)*/ AT 1.                
      OUTPUT CLOSE.
      FIND FIRST tidin2 WHERE RECID(tidin2) = tele_temp.TREC NO-LOCK NO-ERROR.
      IF AVAILABLE tidin2 THEN DELETE tidin2.
      DELETE tele_temp.
   END.   
   OS-APPEND VALUE(Guru.Konstanter:wtidvar) VALUE(sparfil).
   OS-DELETE VALUE(Guru.Konstanter:wtidvar) NO-ERROR.
   sokfil = SEARCH(utfil).
   IF sokfil = ? THEN DO:
      OS-APPEND VALUE(sparfil) VALUE(utfil). 
      OS-DELETE VALUE(sparfil) NO-ERROR.
   END.   
END PROCEDURE.
