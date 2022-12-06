/*SMSDB.P*/
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE infil AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE utfil AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE sparfil AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE ejconfil AS CHARACTER NO-UNDO.


DEFINE VARIABLE dlcvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(86)" NO-UNDO. 
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE persvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE datstrakt AS INTEGER FORMAT "9" LABEL "TRAKTAMENTE" NO-UNDO.
DEFINE VARIABLE datsslut  AS INTEGER FORMAT "9999" LABEL "SLUT" NO-UNDO.
DEFINE VARIABLE datsstart AS INTEGER FORMAT "9999" LABEL "START" NO-UNDO.
DEFINE VARIABLE melvar AS INTEGER NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO.
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO.
DEFINE VARIABLE pkod AS LOGICAL NO-UNDO.
DEFINE VARIABLE ao AS LOGICAL NO-UNDO.
DEFINE VARIABLE trakt AS LOGICAL NO-UNDO.
DEFINE VARIABLE dat AS LOGICAL NO-UNDO.
DEFINE VARIABLE sttid AS LOGICAL NO-UNDO.
DEFINE VARIABLE sltid AS LOGICAL NO-UNDO.
DEFINE VARIABLE ers AS LOGICAL NO-UNDO.
DEFINE VARIABLE tidsek AS INTEGER NO-UNDO.
DEFINE VARIABLE nr AS INTEGER NO-UNDO.
DEFINE VARIABLE svarlangd AS INTEGER NO-UNDO.
DEFINE VARIABLE flerfel AS LOGICAL NO-UNDO.
DEFINE VARIABLE persfel AS LOGICAL NO-UNDO.
DEFINE VARIABLE aopersfel AS LOGICAL NO-UNDO.
DEFINE VARIABLE sokfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE ascnummer AS INTEGER NO-UNDO.
DEFINE VARIABLE aolangd AS INTEGER NO-UNDO.

DEFINE VARIABLE sparfil2 AS CHARACTER NO-UNDO.

DEFINE SHARED TEMP-TABLE smsaonr
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD UTRYCKNING AS LOGICAL
   FIELD PRISTYP AS CHARACTER
   FIELD TRAKTAMENTE AS INTEGER
   FIELD FORETAG AS CHARACTER
   INDEX AONR IS PRIMARY AONR DELNR FORETAG
   INDEX FORE FORETAG AONR DELNR.
   
DEFINE SHARED TEMP-TABLE smspersonal
   FIELD PERSONALKOD AS CHARACTER   
   FIELD ANSTALLNING AS CHARACTER
   FIELD OVERTIDUTTAG AS CHARACTER
   FIELD PREC AS RECID
   FIELD FORETAG AS CHARACTER
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD FORETAG
   INDEX FORE FORETAG PERSONALKOD.

DEFINE NEW SHARED TEMP-TABLE tele_temp      
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
   
DEFINE TEMP-TABLE tidin
   FIELD TIN AS CHARACTER FORMAT "X(256)".
   
DEFINE NEW SHARED TEMP-TABLE tidin2
   FIELD TIN AS CHARACTER FORMAT "X(256)". 
   
DEFINE BUFFER telbuff FOR tele_temp.   

/*HUVUD PROG.*/
 /*  \\GRANGURU\c$\*/
 {AMERICANEUROPEAN.I}
ASSIGN 
   ejconfil = {GRANSMS.I} + "QUERY\sms_ejcon.txt"
   infil = {GRANSMS.I} + "QUERY\sms_in.txt"
   utfil = {GRANSMS.I} + "RESULT\sms_send.txt"
   sparfil = {GRANSMS.I} + "RESULT\sms_spar.txt"
   sparfil2 = {GRANSMS.I} + "RESULT\sms_spar2.txt"
   dlcvar = "\\GRANGURU\guru_ser\klient\PRO9\dlc\bin\QUOTER.EXE" 
   wtidvar = "\\GRANGURU\guru_ser\klient\PRO9\guru\wtid\smsguru.q".   


FOR EACH tele_temp:
   DELETE tele_temp.
END.
FOR EACH tidin:
   DELETE tidin.
END.
FOR EACH tidin2:
   DELETE tidin2.
END.
 
/*Om det skickats något meddelande från guru*/
sokfil = SEARCH(sparfil2).
IF sokfil = ? THEN musz = musz.
ELSE DO:
   OS-APPEND VALUE(sparfil2) VALUE(sparfil).
   OS-DELETE VALUE(sparfil2) NO-ERROR.
END.

/*ordinarie*/
sokfil = SEARCH(infil).
IF sokfil = ? THEN musz = musz.
ELSE DO:
   OS-COMMAND SILENT VALUE(dlcvar) VALUE(infil) > VALUE(wtidvar).           
   INPUT FROM VALUE(wtidvar) NO-ECHO
   CONVERT TARGET "iso8859-1" SOURCE "ibm850" NO-ECHO.
   /*iso8859-1 swedish-7-bit ibm850"*/
   REPEAT:
      SET words VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 WITH FRAME DDD WIDTH 80.
      CREATE tidin. 
      ASSIGN tidin.TIN = words.
   END.
   INPUT CLOSE.  
   OS-DELETE VALUE(infil) NO-ERROR.
   OS-DELETE VALUE(wtidvar) NO-ERROR.
END.

/*ejcon*/
sokfil = SEARCH(ejconfil).
IF sokfil = ? THEN musz = musz.
ELSE DO:
   OS-COMMAND SILENT VALUE(dlcvar) VALUE(ejconfil) > VALUE(wtidvar).           
   INPUT FROM VALUE(wtidvar) NO-ECHO
   CONVERT TARGET "iso8859-1" SOURCE "ibm850" NO-ECHO.
   /*iso8859-1 swedish-7-bit ibm850"*/
   REPEAT:
      SET words VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 WITH FRAME HHH WIDTH 80.
      CREATE tidin. 
      ASSIGN tidin.TIN = words.
   END.
   INPUT CLOSE.  
   OS-DELETE VALUE(ejconfil) NO-ERROR.
   OS-DELETE VALUE(wtidvar) NO-ERROR.
END.   

/*""*/

FIND FIRST tidin NO-LOCK NO-ERROR.  
IF AVAILABLE tidin THEN DO TRANSACTION:       
   ASSIGN
   langd = LENGTH(tidin.TIN)
   pos1 = 1
   melvar = INDEX(tidin.TIN,CHR(13),pos1).
   IF melvar NE 0 THEN DO:
      DO WHILE melvar < langd:         
         CREATE tidin2.
         ASSIGN
         tidin2.TIN = SUBSTRING(tidin.TIN,pos1,melvar - pos1 + 1).         
         ASSIGN
         pos1 = melvar + 1
         melvar = INDEX(tidin.TIN,CHR(13),pos1).
      END.   
      CREATE tidin2.
      ASSIGN
      tidin2.TIN = SUBSTRING(tidin.TIN,pos1,melvar - pos1 + 1).
   END. 
END. 
REPEAT:  
   FIND NEXT tidin NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tidin THEN LEAVE.
   ELSE DO TRANSACTION:  
      ASSIGN
      langd = LENGTH(tidin.TIN)
      pos1 = 1
      melvar = INDEX(tidin.TIN,CHR(13),pos1).
      IF melvar NE 0 THEN DO:
         DO WHILE melvar < langd:         
            CREATE tidin2.
            ASSIGN
            tidin2.TIN = SUBSTRING(tidin.TIN,pos1,melvar - pos1 + 1).
            ASSIGN
            pos1 = melvar + 1
            melvar = INDEX(tidin.TIN,CHR(13),pos1).
         END.   
         CREATE tidin2.
         ASSIGN
         tidin2.TIN = SUBSTRING(tidin.TIN,pos1,melvar - pos1 + 1).
      END.   
   END.
END. 

FOR EACH tidin2 NO-LOCK:   
   IF SUBSTRING(tidin2.TIN,14,1) = "1" THEN DO:
      RUN normin_UI.
   END.
   ELSE IF SUBSTRING(tidin2.TIN,14,1) = "2" THEN DO:
      RUN overin_UI.
   END.   
   ELSE IF SUBSTRING(tidin2.TIN,14,1) = "3" THEN DO:
      RUN normklin_UI.
   END. 
   ELSE IF SUBSTRING(tidin2.TIN,14,1) = "4" THEN DO:
      RUN framin_UI.
   END.
   ELSE IF SUBSTRING(tidin2.TIN,14,1) = "9" THEN DO:
      RUN kolldag_UI.
   END.
   ELSE DO:
      RUN fel_UI.
   END.       
END.

FOR EACH tele_temp WHERE tele_temp.FRAGA NE ? AND tele_temp.SVAR = ""
USE-INDEX FRAGA: 
   IF tele_temp.FRAGA = "9" THEN DO:
      FIND FIRST smspersonal WHERE smspersonal.PERSONALKOD = 
      tele_temp.PKOD AND smspersonal.FORETAG = tele_temp.FORETAG 
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE smspersonal THEN DO:                  
         ASSIGN
         tele_temp.PREC = smspersonal.PREC
         tele_temp.ANSTALLNING = smspersonal.ANSTALLNING                  
         tele_temp.SVAR = "Registrering OK".
         IF tele_temp.FRAGA NE "2" THEN
         tele_temp.OVERTIDUTTAG = smspersonal.OVERTIDUTTAG.
      END.
      ELSE DO:
         tele_temp.SVAR = "Fel personalkod:" + tele_temp.PKOD.
      END.     
   END.
   ELSE DO:
      FIND FIRST smsaonr WHERE smsaonr.AONR = tele_temp.AONR AND
      smsaonr.DELNR = tele_temp.DELNR AND smsaonr.FORETAG = tele_temp.FORETAG 
      USE-INDEX AONR NO-LOCK NO-ERROR.
      IF AVAILABLE smsaonr THEN DO:     
         ASSIGN
         tele_temp.UTRYCKNING = smsaonr.UTRYCKNING
         tele_temp.PRISTYP = smsaonr.PRISTYP.
         IF tele_temp.TRAKTAMENTE = ? THEN
         tele_temp.TRAKTAMENTE = smsaonr.TRAKTAMENTE.
         FIND FIRST smspersonal WHERE smspersonal.PERSONALKOD = 
         tele_temp.PKOD AND smspersonal.FORETAG = tele_temp.FORETAG 
         USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
         IF AVAILABLE smspersonal THEN DO:                  
            ASSIGN
            tele_temp.PREC = smspersonal.PREC
            tele_temp.ANSTALLNING = smspersonal.ANSTALLNING                  
            tele_temp.SVAR = "Registrering OK".
            IF tele_temp.FRAGA NE "2" THEN
            tele_temp.OVERTIDUTTAG = smspersonal.OVERTIDUTTAG.
         END.
         ELSE DO:
            tele_temp.SVAR = "Fel personalkod:" + tele_temp.PKOD.
         END.     
      END.
      ELSE DO:      
         FIND FIRST smspersonal WHERE smspersonal.PERSONALKOD = 
         tele_temp.PKOD AND smspersonal.FORETAG = tele_temp.FORETAG 
         USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE smspersonal THEN DO:        
            tele_temp.SVAR = "Fel Aonr och personalkod:" + tele_temp.PKOD.                             
         END.
         ELSE DO:
            tele_temp.SVAR = "Fel Aonr".
         END.   
      END.              
   END.
END.

FOR EACH tele_temp WHERE tele_temp.ORGINAL = TRUE USE-INDEX ORG:
   FIND FIRST telbuff WHERE telbuff.NUMMER = tele_temp.NUMMER AND
   telbuff.ORGINAL = FALSE AND telbuff.SVAR NE "Registrering OK" 
   USE-INDEX ORG NO-LOCK NO-ERROR.
   IF AVAILABLE telbuff THEN DO:
      ASSIGN
      persfel = FALSE
      aopersfel = FALSE
      flerfel = FALSE.
      IF tele_temp.SVAR = "Registrering OK"  THEN DO:
         ASSIGN            
         tele_temp.SVAR = "".
         RUN flerkoll_UI.
      END.
      ELSE IF SUBSTRING(tele_temp.SVAR,1,25) = "Fel Aonr och personalkod:" THEN DO:
         svarlangd = LENGTH(tele_temp.SVAR).
         tele_temp.SVAR = SUBSTRING(tele_temp.SVAR,26,svarlangd - 25) + " ".
         RUN flerkoll_UI.
      END.
      ELSE IF tele_temp.SVAR = "Fel Aonr"  THEN DO:
         tele_temp.SVAR = "".
         RUN flerkoll_UI. 
      END. 
      ELSE IF SUBSTRING(tele_temp.SVAR,1,16) = "Fel personalkod:" THEN DO:
         ASSIGN
         svarlangd = LENGTH(tele_temp.SVAR)           
         tele_temp.SVAR = SUBSTRING(tele_temp.SVAR,17,svarlangd - 16) + " ".            
         RUN flerkoll_UI. 
      END.
      ELSE DO:
         FOR EACH telbuff WHERE telbuff.NUMMER = tele_temp.NUMMER AND
         telbuff.ORGINAL = FALSE USE-INDEX ORG:
            DELETE telbuff.            
         END.
      END.                       
   END.   
END.


 
   
FIND FIRST tele_temp WHERE tele_temp.FORETAG = "GRAN" AND
tele_temp.SVAR = "Registrering OK" NO-LOCK NO-ERROR.
IF AVAILABLE tele_temp THEN DO:
   /*IF CONNECTED("GRANSYD") THEN DO: 
 *       DISCONNECT GRANSYD NO-ERROR.
 *    END.*/
   IF CONNECTED("grannord") THEN DO: 
      musz = musz.
   END.  
   ELSE DO:    
        
   END.   
 
   IF CONNECTED("grannord") THEN DO:
      {VERALIAS.I}
      RUN SMS.P.         
      /*DISCONNECT grannord NO-ERROR.*/      
   END.  
END. 


/*FIND FIRST tele_temp WHERE tele_temp.FORETAG = "GSYD" AND
 * tele_temp.SVAR = "Registrering OK" NO-LOCK NO-ERROR.
 * IF AVAILABLE tele_temp THEN DO:     
 *    IF CONNECTED("grannord") THEN DO: 
 *       DISCONNECT grannord NO-ERROR.
 *    END.
 *    IF CONNECTED("GRANSYD") THEN DO: 
 *       musz = musz.
 *    END.
 *    ELSE DO:      
 *        
 *    END.   
 *     
 *    IF CONNECTED("GRANSYD") THEN DO:
 *       {VERALIAS.I}
 *       RUN SMS.P.
 *       DISCONNECT GRANSYD NO-ERROR.      
 *    END.   
 * END.*/  
RUN ut_UI.
RUN ut2_UI.
{EUROPEANAMERICAN.I}
/*SLUT HUVUD PROG.*/


/*PROCEDURER*/  
PROCEDURE flerkoll_UI :
   FOR EACH telbuff WHERE telbuff.NUMMER = tele_temp.NUMMER AND
   telbuff.ORGINAL = FALSE USE-INDEX ORG: 
      IF telbuff.SVAR NE "Registrering OK"  THEN DO:     
         ASSIGN               
         flerfel = TRUE.
         IF SUBSTRING(telbuff.SVAR,1,16) = "Fel personalkod:" THEN DO:
            ASSIGN
            svarlangd = LENGTH(telbuff.SVAR)
            persfel = TRUE
            tele_temp.SVAR = tele_temp.SVAR + SUBSTRING(telbuff.SVAR,17,svarlangd - 16) + " ".
         END.
         ELSE IF SUBSTRING(telbuff.SVAR,1,25) = "Fel Aonr och personalkod:" THEN DO:
            ASSIGN
            svarlangd = LENGTH(telbuff.SVAR)
            aopersfel = TRUE
            tele_temp.SVAR = tele_temp.SVAR + SUBSTRING(telbuff.SVAR,26,svarlangd - 25) + " ".           
         END.        
      END.                 
   END.   
   IF flerfel = TRUE THEN DO:
      FOR EACH telbuff WHERE telbuff.NUMMER = tele_temp.NUMMER AND
      telbuff.ORGINAL = FALSE USE-INDEX ORG:
         DELETE telbuff.            
      END.
      IF aopersfel = TRUE THEN DO:
         tele_temp.SVAR = "Fel Aonr och personalkod:" + tele_temp.SVAR.            
      END.
      ELSE IF persfel = TRUE THEN DO:
         tele_temp.SVAR = "Fel personalkod:" + tele_temp.SVAR.
      END.
      ELSE DO:
         tele_temp.SVAR = "Fel Aonr".
      END.
   END.
   ELSE DO:
      tele_temp.SVAR = "Registrering OK".
   END. 
END PROCEDURE.
PROCEDURE normin_UI :
   RUN siff_UI.
   IF musz = FALSE THEN musz = musz.
   ELSE DO:
      musz = FALSE.
      RETURN.
   END.   
   ASSIGN   
   melvar = INDEX(tidin2.TIN," ",14).
   IF melvar = 0 THEN DO:
      RUN fel_UI.
   END.
   ELSE DO:
      RUN start_UI.
      DO WHILE melvar NE 0:
         RUN sok_UI.                  
         IF melvar NE 0 THEN DO:
            IF pkod = FALSE THEN DO:
               RUN pkod_UI.
            END.   
            ELSE IF ao = FALSE THEN DO:
               RUN ao_UI.
            END.   
            ELSE IF trakt = FALSE THEN DO:
               RUN trakt_UI. 
            END. 
            ELSE DO:               
               RUN nypers_UI.
            END.  
         END.
         ELSE DO:                 
            melvar = INDEX(tidin2.TIN,CHR(13),pos1). 
            IF SUBSTRING(tidin2.TIN,pos1,melvar - pos1) = "" THEN DO:
               melvar = 0.               
            END.
            ELSE DO:
               IF trakt = FALSE THEN DO:
                  RUN trakt_UI. 
               END.
               ELSE DO:                  
                  RUN nypers_UI.
               END.
               melvar = 0.
            END.   
         END.      
      END.   
   END.
END PROCEDURE.  
  
PROCEDURE overin_UI :
   RUN siff_UI.
   IF musz = FALSE THEN musz = musz.
   ELSE DO:
      musz = FALSE.
      RETURN.
   END.
   ASSIGN   
   melvar = INDEX(tidin2.TIN," ",14).
   IF melvar = 0 THEN DO:
      RUN fel_UI.
   END.
   ELSE DO:
      RUN start_UI.
      DO WHILE melvar NE 0:
         RUN sok_UI.                  
         IF melvar NE 0 THEN DO:
            IF pkod = FALSE THEN DO:
               RUN pkod_UI.
            END.   
            ELSE IF ao = FALSE THEN DO:
               RUN ao_UI.
            END.   
            ELSE IF trakt = FALSE THEN DO:
               RUN trakt_UI. 
            END.   
            ELSE IF dat = FALSE THEN DO:
               RUN dat_UI.
            END.   
            ELSE IF sttid = FALSE THEN DO:
               RUN sttid_UI. 
            END.
            ELSE IF sltid = FALSE THEN DO:
               RUN sltid_UI. 
            END.
            ELSE IF ers = FALSE THEN DO:
               RUN ers_UI. 
            END.
            ELSE DO:
               RUN nypers_UI.
            END.
         END.
         ELSE DO:                 
            melvar = INDEX(tidin2.TIN,CHR(13),pos1). 
            IF SUBSTRING(tidin2.TIN,pos1,melvar - pos1) = "" THEN DO:
               melvar = 0.               
            END.
            ELSE DO:
               IF ers = FALSE THEN DO:
                  RUN ers_UI. 
               END. 
               ELSE DO:
                  RUN nypers_UI.
               END.                                            
               melvar = 0.
            END.   
         END.      
      END.   
   END.
END PROCEDURE.  
  
PROCEDURE normklin_UI :
   RUN siff_UI.
   IF musz = FALSE THEN musz = musz.
   ELSE DO:
      musz = FALSE.
      RETURN.
   END.
   ASSIGN   
   melvar = INDEX(tidin2.TIN," ",14).
   IF melvar = 0 THEN DO:
      RUN fel_UI.
   END.
   ELSE DO:
      RUN start_UI.
      DO WHILE melvar NE 0:
         RUN sok_UI.                  
         IF melvar NE 0 THEN DO:
            IF pkod = FALSE THEN DO:
               RUN pkod_UI.
            END.   
            ELSE IF ao = FALSE THEN DO:
               RUN ao_UI.
            END.   
            ELSE IF trakt = FALSE THEN DO:
               RUN trakt_UI. 
            END.  
            ELSE IF sttid = FALSE THEN DO:
               RUN sttid_UI. 
            END.
            ELSE DO:
               RUN nypers_UI.
            END. 
         END.
         ELSE DO:                 
            melvar = INDEX(tidin2.TIN,CHR(13),pos1).  
            IF SUBSTRING(tidin2.TIN,pos1,melvar - pos1) = "" THEN DO:
               melvar = 0.               
            END.
            ELSE DO:                                
               IF sttid = FALSE THEN DO:
                  RUN sttid_UI. 
               END.
               ELSE DO:
                  RUN nypers_UI.
               END.
               melvar = 0.
            END.   
         END.      
      END.   
   END.
END PROCEDURE.

PROCEDURE framin_UI :
   RUN siff_UI.
   IF musz = FALSE THEN musz = musz.
   ELSE DO:
      musz = FALSE.
      RETURN.
   END.
   ASSIGN   
   melvar = INDEX(tidin2.TIN," ",14).
   IF melvar = 0 THEN DO:
      RUN fel_UI.
   END.
   ELSE DO:
      RUN start_UI.
      DO WHILE melvar NE 0:
         RUN sok_UI.                  
         IF melvar NE 0 THEN DO:
            IF pkod = FALSE THEN DO:
               RUN pkod_UI.
            END.   
            ELSE IF ao = FALSE THEN DO:
               RUN ao_UI.
            END.   
            ELSE IF trakt = FALSE THEN DO:
               RUN trakt_UI. 
            END.  
            ELSE IF dat = FALSE THEN DO:
               RUN dat_UI.
            END. 
            ELSE DO:
               RUN nypers_UI.
            END.
         END.
         ELSE DO:                 
            melvar = INDEX(tidin2.TIN,CHR(13),pos1). 
            IF SUBSTRING(tidin2.TIN,pos1,melvar - pos1) = "" THEN DO:
               melvar = 0.               
            END.
            ELSE DO:
               IF dat = FALSE THEN DO:
                  RUN dat_UI.
               END. 
               ELSE DO:
                  RUN nypers_UI.
               END.  
            END.   
            melvar = 0.
         END.      
      END.   
   END.
END PROCEDURE.  
  
PROCEDURE kolldag_UI :   
   RUN siff_UI.
   IF musz = FALSE THEN musz = musz.
   ELSE DO:
      musz = FALSE.
      RETURN.
   END.
   ASSIGN   
   melvar = INDEX(tidin2.TIN," ",14).
   IF melvar = 0 THEN DO:
      RUN fel_UI.
   END.
   ELSE DO:
      RUN start_UI.
      DO WHILE melvar NE 0:
         RUN sok_UI.                  
         IF melvar NE 0 THEN DO:
            IF pkod = FALSE THEN DO:
               RUN pkod_UI.
            END.   
            /*ELSE IF ao = FALSE THEN DO:
               RUN ao_UI.
            END.   
            ELSE IF trakt = FALSE THEN DO:
               RUN trakt_UI. 
            END. */ 
            ELSE IF dat = FALSE THEN DO:
               RUN dat_UI.
            END. 
            /*ELSE DO:
               RUN nypers_UI.
            END.*/
         END.
         ELSE DO:                 
            melvar = INDEX(tidin2.TIN,CHR(13),pos1). 
            IF SUBSTRING(tidin2.TIN,pos1,melvar - pos1) = "" THEN DO:
               melvar = 0.               
            END.
            ELSE DO:
               IF dat = FALSE THEN DO:
                  RUN dat_UI.
               END. 
               /*ELSE DO:
                  RUN nypers_UI.
               END.  */
            END.   
            melvar = 0.
         END.      
      END.   
   END.
END PROCEDURE.  

PROCEDURE siff_UI : 
   ASSIGN
   langd = LENGTH(tidin2.TIN)
   pos1 = 14
   musz = FALSE.  
   DO WHILE pos1 < langd:
      IF musz = FALSE THEN DO:    
         ascnummer = ASC(SUBSTRING(tidin2.TIN,pos1,1)).
         IF ascnummer = 32 THEN musz = FALSE.
         ELSE IF ascnummer >= 48 AND ascnummer <= 57 THEN musz = FALSE.
         ELSE musz = TRUE. 
         pos1 = pos1 + 1.
      END.
      ELSE pos1 = langd.        
   END.                
   IF musz = FALSE THEN musz = musz.
   ELSE DO:
      FIND LAST tele_temp WHERE tele_temp.ORGINAL = TRUE 
      USE-INDEX ORG NO-LOCK NO-ERROR.
      IF AVAILABLE tele_temp THEN DO:
         nr = tele_temp.NUMMER + 1.
      END.
      ELSE DO:
         nr = 1.
      END.    
      CREATE tele_temp.
      ASSIGN             
      tele_temp.TELNR = SUBSTRING(tidin2.TIN,1,12)
      tele_temp.FRAGA = ?
      tele_temp.NUMMER = nr
      tele_temp.ORGINAL = TRUE
      tele_temp.SVAR = "Ett felaktigt tecken har påträffats. Giltiga tecken är siffror och mellanslag."
      tele_temp.TREC = RECID(tidin2).
   END.   
END PROCEDURE.  
  
  
PROCEDURE fel_UI : 
   FIND LAST tele_temp WHERE tele_temp.ORGINAL = TRUE 
   USE-INDEX ORG NO-LOCK NO-ERROR.
   IF AVAILABLE tele_temp THEN DO:
      nr = tele_temp.NUMMER + 1.
   END.
   ELSE DO:
      nr = 1.
   END.    
   CREATE tele_temp.
   ASSIGN             
   tele_temp.TELNR = SUBSTRING(tidin2.TIN,1,12)
   tele_temp.FRAGA = ?
   tele_temp.NUMMER = nr
   tele_temp.ORGINAL = TRUE
   tele_temp.SVAR = "Du har angett ett felaktigt meddelande. Giltiga är 1 (Normaltid), 2 (Övertid), 3 (Normaltid annat klockslag) och 4 (Registrering framåt)."
   tele_temp.TREC = RECID(tidin2).
END PROCEDURE.  
  
PROCEDURE start_UI :        
   ASSIGN
   pkod = FALSE
   ao = FALSE
   trakt = FALSE
   dat = FALSE
   sttid = FALSE
   sltid = FALSE
   ers = FALSE.
   FIND LAST tele_temp WHERE tele_temp.ORGINAL = TRUE 
   USE-INDEX ORG NO-LOCK NO-ERROR.
   IF AVAILABLE tele_temp THEN DO:
      nr = tele_temp.NUMMER + 1.
   END.
   ELSE DO:
      nr = 1.
   END.   
   CREATE tele_temp.
   ASSIGN
   tele_temp.TELNR = SUBSTRING(tidin2.TIN,1,12)
   tele_temp.FRAGA = SUBSTRING(tidin2.TIN,14,1)
   tele_temp.DELNR = 0
   tele_temp.SVAR = ""
   tele_temp.TRAKTAMENTE = ?
   tele_temp.ORGINAL = TRUE
   tele_temp.NUMMER = nr
   tele_temp.TREC = RECID(tidin2).
END PROCEDURE.  
  
PROCEDURE sok_UI :     
   ASSIGN
   pos1 = melvar + 1
   melvar = INDEX(tidin2.TIN," ",pos1).
   IF melvar = pos1 THEN DO:
      DO WHILE melvar = pos1:
         ASSIGN
         pos1 = melvar + 1
         melvar = INDEX(tidin2.TIN," ",pos1).
      END.   
   END.         
END PROCEDURE.

PROCEDURE pkod_UI :     
   ASSIGN               
   persvar = SUBSTRING(tidin2.TIN,pos1,melvar - pos1)
   pkod = TRUE.
   FIND FIRST smspersonal WHERE smspersonal.PERSONALKOD = persvar NO-LOCK NO-ERROR.
   IF AVAILABLE smspersonal THEN DO:
      ASSIGN
      tele_temp.PKOD = persvar
      tele_temp.FORETAG = smspersonal.FORETAG.
   END.
   ELSE DO:
      ASSIGN
      tele_temp.PKOD = persvar.
   END.   
END PROCEDURE.  

PROCEDURE ao_UI :     
   ASSIGN               
   tele_temp.AONR = SUBSTRING(tidin2.TIN,pos1,melvar - pos1)
   ao = TRUE.
   aolangd = LENGTH(tele_temp.AONR).
   IF aolangd > 6 THEN DO:
      ASSIGN
      tele_temp.DELNR = INTEGER(SUBSTRING(tele_temp.AONR,7,aolangd - 6))
      tele_temp.AONR = SUBSTRING(tele_temp.AONR,1,6).
   END.   
END PROCEDURE.

PROCEDURE trakt_UI : 
   trakt = TRUE.
   IF melvar - pos1 = 1 THEN DO:
      IF INTEGER(SUBSTRING(tidin2.TIN,pos1,melvar - pos1)) = 0 OR 
      INTEGER(SUBSTRING(tidin2.TIN,pos1,melvar - pos1)) = 1 THEN DO:   
         ASSIGN               
         tele_temp.TRAKTAMENTE = INTEGER(SUBSTRING(tidin2.TIN,pos1,melvar - pos1)).
      END.
      ELSE DO:
         ASSIGN
         tele_temp.SVAR = "Du har angett felaktigt traktamente:" + SUBSTRING(tidin2.TIN,pos1,melvar - pos1).
      END.   
   END.   
   ELSE DO:
      ASSIGN
      tele_temp.SVAR = "Du har angett felaktigt traktamente:" + SUBSTRING(tidin2.TIN,pos1,melvar - pos1).
   END.
END PROCEDURE.

PROCEDURE sttid_UI : 
   sttid = TRUE. 
   IF melvar - pos1 = 4 THEN DO: 
      IF DECIMAL(INTEGER(SUBSTRING(tidin2.TIN,pos1,2))) >= 0 AND
      DECIMAL(INTEGER(SUBSTRING(tidin2.TIN,pos1,2))) <= 24 THEN DO:
         IF DECIMAL(INTEGER(SUBSTRING(tidin2.TIN,pos1 + 2,2))) >= 0 AND
         DECIMAL(INTEGER(SUBSTRING(tidin2.TIN,pos1 + 2,2))) <= 59 THEN DO:
            ASSIGN               
            tele_temp.START = DECIMAL(INTEGER(SUBSTRING(tidin2.TIN,pos1,melvar - pos1)) / 100).
         END.
         ELSE DO:
            ASSIGN
            tele_temp.SVAR = "Du har angett felaktig starttid:" + SUBSTRING(tidin2.TIN,pos1,melvar - pos1).
         END.   
      END.   
      ELSE DO:
         ASSIGN
         tele_temp.SVAR = "Du har angett felaktig starttid:" + SUBSTRING(tidin2.TIN,pos1,melvar - pos1).
      END.
   END.  
   ELSE DO:
      ASSIGN
      tele_temp.SVAR = "Du har angett felaktig starttid:" + SUBSTRING(tidin2.TIN,pos1,melvar - pos1).
   END. 
END PROCEDURE.

PROCEDURE sltid_UI :
   sltid = TRUE.  
   IF melvar - pos1 = 4 THEN DO:          
      IF DECIMAL(INTEGER(SUBSTRING(tidin2.TIN,pos1,2))) >= 0 AND
      DECIMAL(INTEGER(SUBSTRING(tidin2.TIN,pos1,2))) <= 24 THEN DO: 
         IF DECIMAL(INTEGER(SUBSTRING(tidin2.TIN,pos1 + 2,2))) >= 0 AND
         DECIMAL(INTEGER(SUBSTRING(tidin2.TIN,pos1 + 2,2))) <= 59 THEN DO: 
            ASSIGN               
            tele_temp.SLUT = DECIMAL(INTEGER(SUBSTRING(tidin2.TIN,pos1,melvar - pos1)) / 100).
         END.   
         ELSE DO:
            ASSIGN
            tele_temp.SVAR = "Du har angett felaktig sluttid:" + SUBSTRING(tidin2.TIN,pos1,melvar - pos1).
         END.   
      END.   
      ELSE DO:
         ASSIGN
         tele_temp.SVAR = "Du har angett felaktig sluttid:" + SUBSTRING(tidin2.TIN,pos1,melvar - pos1).
      END.   
   END. 
   ELSE DO:
      ASSIGN
      tele_temp.SVAR = "Du har angett felaktig sluttid:" + SUBSTRING(tidin2.TIN,pos1,melvar - pos1).
   END.     
END PROCEDURE.

PROCEDURE dat_UI :     
   dat = TRUE.
   IF melvar - pos1 = 4 THEN DO:
      IF INTEGER(SUBSTRING(tidin2.TIN,pos1,melvar - pos1)) >= 101 AND
      INTEGER(SUBSTRING(tidin2.TIN,pos1,melvar - pos1)) <= 1231 THEN DO:
         IF INTEGER(SUBSTRING(tidin2.TIN,pos1,2)) > 0 AND
         INTEGER(SUBSTRING(tidin2.TIN,pos1,2)) <= 12 THEN DO:
            IF INTEGER(SUBSTRING(tidin2.TIN,pos1 + 2,2)) > 0 AND
            INTEGER(SUBSTRING(tidin2.TIN,pos1 + 2,2)) <= 31 THEN DO:
               ASSIGN               
               tele_temp.DATUM = 
               DATE(INTEGER(SUBSTRING(tidin2.TIN,pos1,2)),INTEGER(SUBSTRING(tidin2.TIN,pos1 + 2,2)),YEAR(TODAY)). 
            END.   
            ELSE DO:
               ASSIGN
               tele_temp.SVAR = "Du har angett felaktigt datum:" + SUBSTRING(tidin2.TIN,pos1,melvar - pos1).
            END.
         END.   
         ELSE DO:
            ASSIGN
            tele_temp.SVAR = "Du har angett felaktigt datum:" + SUBSTRING(tidin2.TIN,pos1,melvar - pos1).
         END.
      END.   
      ELSE DO:
         ASSIGN
         tele_temp.SVAR = "Du har angett felaktigt datum:" + SUBSTRING(tidin2.TIN,pos1,melvar - pos1).
      END.
   END.
   ELSE DO:
      ASSIGN
      tele_temp.SVAR = "Du har angett felaktigt datum:" + SUBSTRING(tidin2.TIN,pos1,melvar - pos1).
   END.
END PROCEDURE.

PROCEDURE ers_UI :     
   ers = TRUE.
   IF melvar - pos1 = 1 THEN DO:
      IF INTEGER(SUBSTRING(tidin2.TIN,pos1,melvar - pos1)) >= 0 AND
      INTEGER(SUBSTRING(tidin2.TIN,pos1,melvar - pos1)) <= 3 THEN DO:   
         ASSIGN               
         tele_temp.OVERTIDUTTAG = SUBSTRING(tidin2.TIN,pos1,melvar - pos1).
      END.   
      ELSE DO:
         ASSIGN
         tele_temp.SVAR = "Du har angett felaktig ersättning:" + SUBSTRING(tidin2.TIN,pos1,melvar - pos1).
      END.
   END.   
   ELSE DO:
      ASSIGN
      tele_temp.SVAR = "Du har angett felaktig ersättning:" + SUBSTRING(tidin2.TIN,pos1,melvar - pos1).
   END.
END PROCEDURE.

PROCEDURE nypers_UI :       
   FIND LAST tele_temp WHERE tele_temp.ORGINAL = TRUE 
   AND tele_temp.NUMMER = nr USE-INDEX ORG NO-LOCK NO-ERROR.   
   CREATE telbuff.
   ASSIGN              
   telbuff.PKOD = SUBSTRING(tidin2.TIN,pos1,melvar - pos1)
   telbuff.TELNR = tele_temp.TELNR
   telbuff.FRAGA = tele_temp.FRAGA
   telbuff.AO = tele_temp.AO
   telbuff.DATUM = tele_temp.DATUM
   telbuff.START = tele_temp.START
   telbuff.SLUT = tele_temp.SLUT   
   telbuff.TRAKTAMENTE = tele_temp.TRAKTAMENTE
   telbuff.OVERTIDUTTAG = tele_temp.OVERTIDUTTAG
   telbuff.SVAR = ""   
   telbuff.ORGINAL = FALSE
   telbuff.NUMMER = nr
   telbuff.TREC = RECID(tidin2)
   telbuff.FORETAG = tele_temp.FORETAG.
END PROCEDURE.

PROCEDURE ut_UI :
   {AMERICANEUROPEAN.I}     
   FOR EACH tele_temp WHERE tele_temp.ORGINAL = TRUE 
   AND tele_temp.SVAR NE "Registrering OK" USE-INDEX ORG:      
      OUTPUT TO VALUE(wtidvar) APPEND CONVERT TARGET "ibm850" SOURCE "iso8859-1" 
      NO-ECHO.           
      PUT UNFORMATTED        
      SUBSTRING(tele_temp.TELNR,1,12) + CHR(9) + 
      tele_temp.SVAR + CHR(13) AT 1.                
      OUTPUT CLOSE.
      FIND FIRST tidin2 WHERE RECID(tidin2) = tele_temp.TREC NO-LOCK NO-ERROR.
      IF AVAILABLE tidin2 THEN DELETE tidin2.
      DELETE tele_temp.
   END.   
   OS-APPEND VALUE(wtidvar) VALUE(sparfil).
   OS-DELETE VALUE(wtidvar) NO-ERROR.
   sokfil = SEARCH(utfil).
   IF sokfil = ? THEN DO:
      OS-APPEND VALUE(sparfil) VALUE(utfil). 
      OS-DELETE VALUE(sparfil) NO-ERROR.
   END.  
   {EUROPEANAMERICAN.I} 
END PROCEDURE.


PROCEDURE ut2_UI :     
   FOR EACH tidin2: 
      OUTPUT TO VALUE(wtidvar) APPEND CONVERT TARGET "ibm850" SOURCE "iso8859-1" 
      NO-ECHO.           
      PUT UNFORMATTED        
      tidin2.TIN AT 1.                
      OUTPUT CLOSE.
   END.   
   OS-APPEND VALUE(wtidvar) VALUE(ejconfil).
   OS-DELETE VALUE(wtidvar) NO-ERROR.   
END PROCEDURE.
