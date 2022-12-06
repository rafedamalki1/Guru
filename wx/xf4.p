
/*FAOBER.P*/
&Scoped-define NEW NEW                          
{FAKTTEMP.I}
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
/* ************************  Function Implementations ***************** */


FUNCTION klockan100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):
  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.

FUNCTION klockan60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL ):
  RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) / 100) * 60 . 
END FUNCTION.


FUNCTION runda RETURNS DECIMAL
  ( INPUT varedin AS DECIMAL) :
  RETURN ROUND(varedin,0).   /* Function return value. */
END FUNCTION.

/*
DEFINE INPUT PARAMETER globforetag LIKE FORETAG.FORETAG NO-UNDO. 
DEFINE INPUT PARAMETER faktrec AS RECID NO-UNDO.     
DEFINE INPUT PARAMETER FILL-IN-TOMDAT AS DATE NO-UNDO.
DEFINE INPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER delnrvar AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER kollvecko LIKE TIDREGITAB.VECKOKORD NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tidtemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR extrasum.
*/
DEFINE VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO. 
DEFINE VARIABLE faktrec AS RECID NO-UNDO.     
DEFINE VARIABLE FILL-IN-TOMDAT AS DATE NO-UNDO.
DEFINE VARIABLE aonrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE delnrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE kollvecko LIKE TIDREGITAB.VECKOKORD NO-UNDO.
ASSIGN
kollvecko = "w20070313"
delnrvar = ?
globforetag = "elpa"
FILL-IN-TOMDAT = TODAY
aonrvar = ?
delnrvar = ?.
DEFINE NEW SHARED VARIABLE varforetypval AS INTEGER EXTENT 100 NO-UNDO.
DEFINE NEW SHARED VARIABLE varforetypchar AS CHARACTER EXTENT 100 NO-UNDO.     
DEFINE NEW SHARED VARIABLE globanv AS CHARACTER NO-UNDO. 
FIND FIRST FAKTPLAN WHERE FAKTPLAN.faktnr = 90 NO-LOCK NO-ERROR.



/*FAOBERU.I*/
DEFINE VARIABLE dnrhj AS INTEGER NO-UNDO.
DEFINE VARIABLE debkredvar AS INTEGER NO-UNDO.
 DEFINE TEMP-TABLE tidertemp NO-UNDO LIKE TIDFEL.
RUN STYRFORE.P (INPUT globforetag).
IF FAKTPLAN.FAKTTYP = "Löpande utan" THEN kollvecko = "UTAN@".
FIND FIRST FAKTREGLER WHERE FAKTREGLER.FAKTNR = FAKTPLAN.FAKTNR 
USE-INDEX FAKTREGLER NO-LOCK NO-ERROR.
IF delnrvar = ? THEN DO:
   OPEN QUERY faktaonrq FOR EACH FAKTAONR WHERE FAKTAONR.FAKTNR = FAKTPLAN.FAKTNR 
   USE-INDEX FAKTA NO-LOCK.
END.
ELSE DO:
   OPEN QUERY faktaonrq FOR EACH FAKTAONR WHERE FAKTAONR.FAKTNR = FAKTPLAN.FAKTNR 
   AND FAKTAONR.AONR = aonrvar AND FAKTAONR.DELNR = delnrvar
   USE-INDEX FAKTA NO-LOCK.
END.
IF FAKTREGLER.TIMRGL = "INGA" THEN RETURN.
IF kollvecko = "UTAN@" THEN RUN utan_UI.
ELSE RUN overegna_UI.

PROCEDURE overegna_UI :
   GET FIRST faktaonrq NO-LOCK.
   DO WHILE AVAILABLE(FAKTAONR):   
      FIND FIRST FAKTKOLL WHERE FAKTKOLL.FAKTNR = FAKTPLAN.FAKTNR AND      
      FAKTKOLL.AONR = FAKTAONR.AONR AND 
      FAKTKOLL.DELNR = FAKTAONR.DELNR
      USE-INDEX FASTK NO-LOCK NO-ERROR.
      IF FAKTKOLL.VECKOKORD NE "" THEN DO:      
         IF FAKTREGLER.SUMALLAR = TRUE THEN DO:
            IF varforetypval[9] = 1 THEN DO:
               OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
                /*TIDREGITAB.DELNR = FAKTAONR.DELNR AND */
                TIDREGITAB.VECKOKORD >= FAKTKOLL.VECKOKORD 
                AND TIDREGITAB.TIDLOG = TRUE USE-INDEX AONR NO-LOCK.  
            END.
            ELSE DO:
               OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
               TIDREGITAB.DELNR = FAKTAONR.DELNR AND 
               TIDREGITAB.VECKOKORD >= FAKTKOLL.VECKOKORD AND 
               TIDREGITAB.TIDLOG = TRUE USE-INDEX AONR NO-LOCK.  
            END.
            
         END.
         ELSE DO:
            IF varforetypval[9] = 1 THEN DO:
               OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
               /*TIDREGITAB.DELNR = FAKTAONR.DELNR AND */
               TIDREGITAB.VECKOKORD >= FAKTKOLL.VECKOKORD 
               AND TIDREGITAB.TIDLOG = TRUE AND 
               YEAR(TIDREGITAB.DATUM) = YEAR(FILL-IN-TOMDAT)
               USE-INDEX AONR NO-LOCK.  
            END.
            ELSE DO:
               OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
               TIDREGITAB.DELNR = FAKTAONR.DELNR AND 
               TIDREGITAB.VECKOKORD >= FAKTKOLL.VECKOKORD AND 
               TIDREGITAB.TIDLOG = TRUE AND 
               YEAR(TIDREGITAB.DATUM) = YEAR(FILL-IN-TOMDAT)
               USE-INDEX AONR NO-LOCK.
            END.
         END.   
      END.
      ELSE DO:
         IF FAKTREGLER.SUMALLAR = TRUE THEN DO:
            IF varforetypval[9] = 1 THEN DO:
               OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
               /*TIDREGITAB.DELNR = FAKTAONR.DELNR AND */
               TIDREGITAB.VECKOKORD NE "" AND 
               TIDREGITAB.TIDLOG = TRUE USE-INDEX AONR NO-LOCK. 
            END.
            ELSE DO:
               OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
               TIDREGITAB.DELNR = FAKTAONR.DELNR AND TIDREGITAB.VECKOKORD NE "" AND 
               TIDREGITAB.TIDLOG = TRUE USE-INDEX AONR NO-LOCK. 
            END.
         END.
         ELSE DO:
            IF varforetypval[9] = 1 THEN DO:
               OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
               /*TIDREGITAB.DELNR = FAKTAONR.DELNR AND */
               TIDREGITAB.VECKOKORD NE ""  AND 
               TIDREGITAB.TIDLOG = TRUE AND 
               YEAR(TIDREGITAB.DATUM) = YEAR(FILL-IN-TOMDAT)
               USE-INDEX AONR NO-LOCK. 
            END.
            ELSE DO:
               OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
               TIDREGITAB.DELNR = FAKTAONR.DELNR AND 
               TIDREGITAB.VECKOKORD NE ""  AND 
               TIDREGITAB.TIDLOG = TRUE AND 
               YEAR(TIDREGITAB.DATUM) = YEAR(FILL-IN-TOMDAT)
               USE-INDEX AONR NO-LOCK. 
            END.
         END.   
      END.
      GET FIRST tidq NO-LOCK. 
      DO WHILE AVAILABLE(TIDREGITAB):         
         IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN musz = musz.
         ELSE DO:
            CREATE tidertemp.
            BUFFER-COPY TIDREGITAB TO tidertemp.
            IF varforetypval[9] = 1 THEN tidertemp.DELNR = 0.
            tidertemp.DEBET = TRUE.
         END.
         GET NEXT tidq NO-LOCK. 
      END.
      RUN tidfel_UI.
      dnrhj = FAKTAONR.DELNR.
      IF varforetypval[9] = 1 THEN dnrhj = 0.
      FOR EACH tidertemp WHERE tidertemp.AONR = FAKTAONR.AONR AND tidertemp.DELNR = dnrhj:
         IF globforetag = "ESAN" OR globforetag = "ESMA" OR 
         globforetag = "ETA"  OR globforetag = "GRAN" OR globforetag = "GKAL" THEN DO:               
            IF tidertemp.DATUM < 01/01/2000 THEN musz = TRUE.
         END.                                              
         IF musz = TRUE THEN DO: 
            musz = FALSE.
         END.
         ELSE DO: 
            IF tidertemp.FELDATUM = ? THEN DO:
               IF tidertemp.DATUM <= FILL-IN-TOMDAT THEN DO:
                  IF FAKTKOLL.SENASTTID = ? THEN RUN nytid_UI.
                  ELSE IF tidertemp.VECKOKORD = FAKTKOLL.VECKOKORD AND 
                  FAKTKOLL.SENASTTID < tidertemp.DATUM THEN RUN nytid_UI.
                  ELSE IF tidertemp.VECKOKORD > FAKTKOLL.VECKOKORD THEN RUN nytid_UI.                                                                 
               END.
            END.
            ELSE DO:
               IF tidertemp.FELDATUM <= FILL-IN-TOMDAT THEN DO:
                  IF FAKTKOLL.SENASTTID = ? THEN RUN nytid_UI.
                  ELSE IF tidertemp.FELKORD = FAKTKOLL.VECKOKORD AND 
                  FAKTKOLL.SENASTTID < tidertemp.FELDATUM THEN RUN nytid_UI.
                  ELSE IF tidertemp.FELKORD > FAKTKOLL.VECKOKORD THEN RUN nytid_UI.                                                                 
               END.
            END.
         END.           
      END.   
      GET NEXT faktaonrq NO-LOCK.   
   END.        
   RUN bef_UI.
   
END PROCEDURE.


PROCEDURE tidfel_UI :
   IF FAKTKOLL.VECKOKORD NE "" THEN DO:      
      IF FAKTREGLER.SUMALLAR = TRUE THEN DO:
         IF varforetypval[9] = 1 THEN DO:
            OPEN QUERY tidq FOR EACH TIDFEL WHERE TIDFEL.AONR = FAKTAONR.AONR AND 
             /*TIDFEL.DELNR = FAKTAONR.DELNR AND */
             TIDFEL.FELKORD >= FAKTKOLL.VECKOKORD 
             AND TIDFEL.TIDLOG = TRUE USE-INDEX AONR NO-LOCK.  
         END.
         ELSE DO:
            OPEN QUERY tidq FOR EACH TIDFEL WHERE TIDFEL.AONR = FAKTAONR.AONR AND 
            TIDFEL.DELNR = FAKTAONR.DELNR AND 
            TIDFEL.FELKORD >= FAKTKOLL.VECKOKORD AND 
            TIDFEL.TIDLOG = TRUE USE-INDEX AONR NO-LOCK.  
         END.
         
      END.
      ELSE DO:
         IF varforetypval[9] = 1 THEN DO:
            OPEN QUERY tidq FOR EACH TIDFEL WHERE TIDFEL.AONR = FAKTAONR.AONR AND 
            /*TIDFEL.DELNR = FAKTAONR.DELNR AND */
            TIDFEL.FELKORD >= FAKTKOLL.VECKOKORD 
            AND TIDFEL.TIDLOG = TRUE AND 
            YEAR(TIDFEL.DATUM) = YEAR(FILL-IN-TOMDAT)
            USE-INDEX AONR NO-LOCK.  
         END.
         ELSE DO:
            OPEN QUERY tidq FOR EACH TIDFEL WHERE TIDFEL.AONR = FAKTAONR.AONR AND 
            TIDFEL.DELNR = FAKTAONR.DELNR AND 
            TIDFEL.FELKORD >= FAKTKOLL.VECKOKORD AND 
            TIDFEL.TIDLOG = TRUE AND 
            YEAR(TIDFEL.DATUM) = YEAR(FILL-IN-TOMDAT)
            USE-INDEX AONR NO-LOCK.
         END.
      END.   
   END.
   ELSE DO:
      IF FAKTREGLER.SUMALLAR = TRUE THEN DO:
         IF varforetypval[9] = 1 THEN DO:
            OPEN QUERY tidq FOR EACH TIDFEL WHERE TIDFEL.AONR = FAKTAONR.AONR AND 
            /*TIDFEL.DELNR = FAKTAONR.DELNR AND */
            TIDFEL.FELKORD NE "" AND 
            TIDFEL.TIDLOG = TRUE USE-INDEX AONR NO-LOCK. 
         END.
         ELSE DO:
            OPEN QUERY tidq FOR EACH TIDFEL WHERE TIDFEL.AONR = FAKTAONR.AONR AND 
            TIDFEL.DELNR = FAKTAONR.DELNR AND TIDFEL.FELKORD NE "" AND 
            TIDFEL.TIDLOG = TRUE USE-INDEX AONR NO-LOCK. 
         END.
      END.
      ELSE DO:
         IF varforetypval[9] = 1 THEN DO:
            OPEN QUERY tidq FOR EACH TIDFEL WHERE TIDFEL.AONR = FAKTAONR.AONR AND 
            /*TIDFEL.DELNR = FAKTAONR.DELNR AND */
            TIDFEL.FELKORD NE ""  AND 
            TIDFEL.TIDLOG = TRUE AND 
            YEAR(TIDFEL.DATUM) = YEAR(FILL-IN-TOMDAT)
            USE-INDEX AONR NO-LOCK. 
         END.
         ELSE DO:
            OPEN QUERY tidq FOR EACH TIDFEL WHERE TIDFEL.AONR = FAKTAONR.AONR AND 
            TIDFEL.DELNR = FAKTAONR.DELNR AND 
            TIDFEL.FELKORD NE ""  AND 
            TIDFEL.TIDLOG = TRUE AND 
            YEAR(TIDFEL.DATUM) = YEAR(FILL-IN-TOMDAT)
            USE-INDEX AONR NO-LOCK. 
         END.
      END.   
   END.
   GET FIRST tidq NO-LOCK. 
   DO WHILE AVAILABLE(TIDFEL):         
      IF TIDFEL.PRISTYP = "FRÅNVARO." THEN musz = musz.
      ELSE DO:
         CREATE tidertemp.
         BUFFER-COPY TIDFEL TO tidertemp.                     
          IF varforetypval[9] = 1 THEN tidertemp.DELNR = 0.
      END.
      GET NEXT tidq NO-LOCK. 
   END.

END PROCEDURE.

PROCEDURE bef_UI :
   FIND FIRST tidtemp WHERE tidtemp.NYBEF = FALSE USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   REPEAT:      
      IF NOT AVAILABLE tidtemp THEN LEAVE.                 
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = tidtemp.PERSONALKOD
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE PERSONALTAB THEN DO:
         FIND FIRST ANSTFORMTAB WHERE 
         ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
         USE-INDEX ANSTF NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ANSTFORMTAB THEN FIND FIRST ANSTFORMTAB NO-LOCK NO-ERROR.
         FOR EACH tidtemp WHERE tidtemp.PERSONALKOD = PERSONALTAB.PERSONALKOD 
         USE-INDEX PERSONALKOD:
            ASSIGN 
            tidtemp.NYBEF = TRUE
            tidtemp.KOD = ANSTFORMTAB.KOD.
            IF tidtemp.BEFATTNING = "" THEN DO:
               ASSIGN
               tidtemp.PERSMASK = PERSONALTAB.PERSMASK
               tidtemp.BEFATTNING = PERSONALTAB.BEFATTNING.                                    
            END.
            ELSE DO:
               FIND FIRST BEFATTNINGSTAB WHERE BEFATTNINGSTAB.BEFATTNING = tidtemp.BEFATTNING NO-LOCK NO-ERROR.
               IF AVAILABLE BEFATTNINGSTAB THEN tidtemp.PERSMASK = BEFATTNINGSTAB.PERSMASK.
            END.
            ASSIGN
            tidtemp.NAMN = SUBSTRING(PERSONALTAB.FORNAMN,1,1) + "." + PERSONALTAB.EFTERNAMN.   
            tidtemp.TRAAVTAL =  PERSONALTAB.TRAAVTAL.
            /*FAKTFORE*/
            IF globforetag = "GRAN" OR globforetag = "elpa" THEN DO:
               IF tidtemp.PERSMASK = FALSE THEN DELETE tidtemp.
            END.
         END.         
      END.             
      ELSE DO:
         FIND FIRST ANSTFORMTAB NO-LOCK NO-ERROR.
         ASSIGN
         tidtemp.NYBEF = TRUE
         tidtemp.KOD = ANSTFORMTAB.KOD.
         FIND FIRST BEFATTNINGSTAB NO-LOCK NO-ERROR.
         IF tidtemp.BEFATTNING = "" THEN DO:
            ASSIGN
            tidtemp.PERSMASK = TRUE
            tidtemp.BEFATTNING = BEFATTNINGSTAB.BEFATTNING.                                    
         END.
         ASSIGN
         tidtemp.NAMN = "Saknas".         
      END.
      FIND FIRST tidtemp WHERE tidtemp.NYBEF = FALSE USE-INDEX PERSONALKOD NO-ERROR.
   END.   
   IF FAKTREGLER.OVERTIDRGL = "EGNA REGLER" THEN DO:                   
      FOR EACH tidtemp:
         IF tidtemp.PRISTYP = "RESTID..." THEN NEXT. 
         ELSE DO:
            /*
            IF tidtemp.DEBET = FALSE THEN DO:
               RUN kreditpost_UI (INPUT 1).            
            END.
            */
            FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = tidtemp.DATUM AND 
            OVERAVTAB.KOD = tidtemp.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
            IF AVAILABLE OVERAVTAB THEN DO:
               ASSIGN tidtemp.EQDAG = OVERAVTAB.EQDAG.
            END. 
            ELSE DO:
               ASSIGN tidtemp.EQDAG = WEEKDAY(tidtemp.DATUM).
            END.
            RUN overber_UI.         
            IF tidtemp.DEBET = FALSE THEN DO:
               RUN kreditpost_UI (INPUT 2).               
            END.
         END.
      END.       
   END.      
   REPEAT: 
      FIND FIRST extrasum NO-ERROR.
      IF NOT AVAILABLE extrasum THEN LEAVE.
      CREATE tidtemp.
      BUFFER-COPY extrasum TO tidtemp.
      ASSIGN
      tidtemp.NYBEF = TRUE
      tidtemp.PRIS = extrasum.PRISA  
      tidtemp.START = extrasum.START
      tidtemp.SLUT = extrasum.SLUT      
      tidtemp.OKOST = extrasum.OBELOPP
      tidtemp.OTIMMAR = extrasum.OTIMMAR    
      tidtemp.OANT1 = extrasum.OANT1.
      DELETE extrasum.
      /*
      IF tidtemp.DEBET = FALSE THEN DO:
         RUN kreditpost_UI (INPUT 1).
      END.
      */
      FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = tidtemp.DATUM AND 
      OVERAVTAB.KOD = tidtemp.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
      IF AVAILABLE OVERAVTAB THEN DO:
         ASSIGN tidtemp.EQDAG = OVERAVTAB.EQDAG.
      END. 
      ELSE DO:
         ASSIGN tidtemp.EQDAG = WEEKDAY(tidtemp.DATUM).
      END.
      RUN overber_UI.
      IF tidtemp.DEBET = FALSE THEN DO:
         RUN kreditpost_UI (INPUT 2).         
      END.
   END.      
END PROCEDURE.
PROCEDURE kreditpost_UI :
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   IF vad = 1 THEN DO:
      ASSIGN
      tidtemp.LUNCH    =  tidtemp.LUNCH   * -1
      tidtemp.TOTALT   =  tidtemp.TOTALT  * -1
      tidtemp.RESTIM   =  tidtemp.RESTIM  * -1
      tidtemp.OANT1    =  tidtemp.OANT1   * -1
      tidtemp.OTIMMAR  =  tidtemp.OTIMMAR * -1.
   END.
   IF vad = 2 THEN DO:
      ASSIGN
      tidtemp.LUNCH    =  tidtemp.LUNCH   * -1
      tidtemp.TOTALT   =  tidtemp.TOTALT  * -1
      tidtemp.RESTIM   =  tidtemp.RESTIM  * -1
      tidtemp.OANT1    =  tidtemp.OANT1   * -1
      tidtemp.OTIMMAR  =  tidtemp.OTIMMAR * -1
      tidtemp.RESKOSTDEC = tidtemp.RESKOSTDEC * -1 
      tidtemp.KOST       = tidtemp.KOST       * -1 
      tidtemp.OKOST      = tidtemp.OKOST      * -1 .
   END.
END PROCEDURE.

PROCEDURE overber_UI :
   IF tidtemp.EQDAG = 1 THEN DO:  
      FIND FIRST FAKTOVER WHERE FAKTOVER.FAKTNR = FAKTPLAN.FAKTNR AND
      FAKTOVER.BEFATTNING = tidtemp.BEFATTNING AND FAKTOVER.EQDAG = 8 AND
      FAKTOVER.START LE tidtemp.START AND
      FAKTOVER.SLUT > tidtemp.START 
      USE-INDEX FAKTOVER NO-LOCK NO-ERROR.
      RUN overber2_UI.      
   END.   
   ELSE IF tidtemp.EQDAG = 7 THEN DO:
      FIND FIRST FAKTOVER WHERE FAKTOVER.FAKTNR = FAKTPLAN.FAKTNR AND
      FAKTOVER.BEFATTNING = tidtemp.BEFATTNING AND FAKTOVER.EQDAG = 7 AND
      FAKTOVER.START LE tidtemp.START AND
      FAKTOVER.SLUT > tidtemp.START 
      USE-INDEX FAKTOVER NO-LOCK NO-ERROR.
      RUN overber2_UI.            
   END.   
   ELSE DO:            
      FIND FIRST FAKTOVER WHERE FAKTOVER.FAKTNR = FAKTPLAN.FAKTNR AND
      FAKTOVER.BEFATTNING = tidtemp.BEFATTNING AND FAKTOVER.EQDAG = 2 AND
      FAKTOVER.START LE tidtemp.START AND
      FAKTOVER.SLUT > tidtemp.START 
      USE-INDEX FAKTOVER NO-LOCK NO-ERROR.
      RUN overber2_UI.         
   END.                       
END PROCEDURE.

PROCEDURE overber2_UI :
   IF AVAILABLE FAKTOVER THEN DO:
      tidtemp.OTEXTID = FAKTOVER.OTEXTID. 
      IF FAKTOVER.SLUT >= tidtemp.SLUT THEN DO:         
         ASSIGN
         tidtemp.TOTALT = 0
         tidtemp.KOST = 0                               
         tidtemp.OTIMMAR = klockan100(tidtemp.SLUT) - klockan100(tidtemp.START).                  
         IF globforetag = "NORD" OR globforetag = "ESAN" THEN DO:
            IF tidtemp.OANT1 = 0 THEN DO: 
               ASSIGN               
               tidtemp.OPRIS = FAKTOVER.PRISA
               tidtemp.OKOST = tidtemp.OKOST + tidtemp.OTIMMAR * FAKTOVER.PRISA.                                   
            END.           
            ELSE DO:
               ASSIGN
               tidtemp.OPRIS = FAKTOVER.PRISA
               tidtemp.OTIMMAR = tidtemp.OANT1
               tidtemp.OKOST = tidtemp.OKOST + tidtemp.OANT1 * FAKTOVER.PRISA.                                          
            END. 
         END. 
         ELSE DO:     
            ASSIGN            
            tidtemp.OPRIS = FAKTOVER.PRISA
            tidtemp.OKOST = tidtemp.OKOST + tidtemp.OTIMMAR * FAKTOVER.PRISA.                                                             
         END.         
      END. 
      ELSE DO:         
         CREATE extrasum.
         BUFFER-COPY tidtemp TO extrasum.
         ASSIGN
         extrasum.PRISA = tidtemp.PRIS  
         extrasum.TIMMAR = 0
         extrasum.BELOPP = 0         
         extrasum.START = FAKTOVER.SLUT
         extrasum.GSTART = FAKTOVER.SLUT
         extrasum.GSLUT = tidtemp.SLUT         
         extrasum.OBELOPP = 0
         extrasum.OTIMMAR = 0    
         extrasum.OANT1 = tidtemp.OANT1 - tidtemp.OTIMMAR.
         IF extrasum.OANT1 < 0 THEN extrasum.OANT1 = 0.
         ASSIGN
         tidtemp.TOTALT = 0
         tidtemp.KOST = 0             
         tidtemp.SLUT = FAKTOVER.SLUT         
         tidtemp.OTIMMAR = klockan100(FAKTOVER.SLUT) - klockan100(tidtemp.START).                                                                                
         ASSIGN
         tidtemp.OPRIS = FAKTOVER.PRISA
         tidtemp.OKOST = tidtemp.OTIMMAR * FAKTOVER.PRISA.               
         IF globforetag = "NORD" OR globforetag = "ESAN" THEN DO:
            IF tidtemp.OANT1 = 0 THEN DO:               
               musz = musz.
            END.           
            ELSE DO:                 
               tidtemp.OANT1 = tidtemp.OANT1 - tidtemp.OTIMMAR.                                                                           
            END.                       
         END.          
      END.  
   END.   
   ELSE DO:                   
      IF tidtemp.EQDAG = 1 THEN DO:
         FIND FIRST FAKTOVER WHERE FAKTOVER.FAKTNR = FAKTPLAN.FAKTNR AND
         FAKTOVER.BEFATTNING = tidtemp.BEFATTNING AND FAKTOVER.EQDAG = 8 AND
         FAKTOVER.START < tidtemp.SLUT AND
         FAKTOVER.SLUT >= tidtemp.SLUT 
         USE-INDEX FAKTOVER NO-LOCK NO-ERROR.
      END.  
      ELSE IF tidtemp.EQDAG = 7 THEN DO:
         FIND FIRST FAKTOVER WHERE FAKTOVER.FAKTNR = FAKTPLAN.FAKTNR AND
         FAKTOVER.BEFATTNING = tidtemp.BEFATTNING AND FAKTOVER.EQDAG = 7 AND
         FAKTOVER.START < tidtemp.SLUT AND
         FAKTOVER.SLUT >= tidtemp.SLUT 
         USE-INDEX FAKTOVER NO-LOCK NO-ERROR.
      END.
      ELSE DO:                
         FIND FIRST FAKTOVER WHERE FAKTOVER.FAKTNR = FAKTPLAN.FAKTNR AND
         FAKTOVER.BEFATTNING = tidtemp.BEFATTNING AND FAKTOVER.EQDAG = 2 AND
         FAKTOVER.START < tidtemp.SLUT AND FAKTOVER.SLUT >= tidtemp.SLUT 
         USE-INDEX FAKTOVER NO-LOCK NO-ERROR.
      END.
      IF NOT AVAILABLE FAKTOVER THEN DO: 
         ASSIGN                      
         tidtemp.TOTALT = (klockan100(tidtemp.SLUT) - klockan100(tidtemp.START)) - tidtemp.LUNCH.
         tidtemp.KOST = tidtemp.TOTALT * tidtemp.PRIS.         
      END.
      ELSE DO:
         tidtemp.OTEXTID = FAKTOVER.OTEXTID.
         IF FAKTOVER.START <= tidtemp.START THEN DO:            
            ASSIGN
            tidtemp.TOTALT = 0
            tidtemp.KOST = 0 
            tidtemp.OTIMMAR = klockan100(tidtemp.SLUT) - klockan100(tidtemp.START).                                                         
            IF globforetag = "NORD" OR globforetag = "ESAN" THEN DO:
               IF tidtemp.OANT1 = 0 THEN DO:  
                  ASSIGN      
                  tidtemp.OPRIS = FAKTOVER.PRISA            
                  tidtemp.OKOST = tidtemp.OKOST + tidtemp.OTIMMAR * FAKTOVER.PRISA.  
               END.           
               ELSE DO:  
                  ASSIGN
                  tidtemp.OTIMMAR = tidtemp.OANT1
                  tidtemp.OPRIS = FAKTOVER.PRISA
                  tidtemp.OKOST = tidtemp.OKOST + tidtemp.OANT1 * FAKTOVER.PRISA.                                          
               END. 
            END. 
            ELSE DO:
               ASSIGN                  
               tidtemp.OPRIS = FAKTOVER.PRISA
               tidtemp.OKOST = tidtemp.OKOST + tidtemp.OTIMMAR * FAKTOVER.PRISA.   
            END.                                
         END. 
         ELSE DO:   
            CREATE extrasum.
            BUFFER-COPY tidtemp TO extrasum.
            ASSIGN
            extrasum.PRISA = tidtemp.PRIS  
            extrasum.TIMMAR = 0
            extrasum.BELOPP = 0         
            extrasum.START = tidtemp.START
            extrasum.SLUT = FAKTOVER.START
            extrasum.GSTART = tidtemp.START
            extrasum.GSLUT = FAKTOVER.START         
            extrasum.OBELOPP = 0
            extrasum.OTIMMAR = 0    
            extrasum.OANT1 = tidtemp.OANT1 - tidtemp.OTIMMAR.
            IF extrasum.OANT1 < 0 THEN extrasum.OANT1 = 0.  
            ASSIGN              
            tidtemp.START = FAKTOVER.START
            tidtemp.TOTALT = 0
            tidtemp.KOST = 0             
            tidtemp.OTIMMAR = klockan100(tidtemp.SLUT) - klockan100(FAKTOVER.START).             
            ASSIGN
            tidtemp.OPRIS = FAKTOVER.PRISA
            tidtemp.OKOST = tidtemp.OKOST + tidtemp.OTIMMAR * FAKTOVER.PRISA.   
            IF globforetag = "NORD" OR globforetag = "ESAN" THEN DO:
               IF tidtemp.OANT1 = 0 THEN DO:               
                  musz = musz.
               END.           
               ELSE DO:                                              
                  ASSIGN                               
                  tidtemp.OANT1 = tidtemp.OANT1 - tidtemp.OTIMMAR.                                                            
               END. 
            END.                                 
         END.  
      END. 
   END.   
END PROCEDURE.

PROCEDURE nytid_UI :
   IF kollvecko = "UTAN@" THEN musz = musz.
   ELSE IF kollvecko < tidertemp.VECKOKORD THEN kollvecko = tidertemp.VECKOKORD. 
   IF tidertemp.DEBET = TRUE THEN debkredvar = 1.
   ELSE DO:
      debkredvar = -1.
   END.
   IF tidertemp.TIDLOG = TRUE THEN DO:
      IF tidertemp.PRISTYP = "RESTID..." THEN DO:
         CREATE tidtemp.
         ASSIGN
         tidtemp.DEBET = tidertemp.DEBET
         tidtemp.NYBEF = FALSE
         tidtemp.PERSONALKOD = tidertemp.PERSONALKOD
         tidtemp.PRISTYP = tidertemp.PRISTYP
         tidtemp.AONR = tidertemp.AONR
         tidtemp.DELNR = tidertemp.DELNR
         tidtemp.DATUM = tidertemp.DATUM
         tidtemp.RESPRIS = tidertemp.PRIS
         tidtemp.START = tidertemp.START      
         tidtemp.SLUT = tidertemp.SLUT
         tidtemp.BEFATTNING = tidertemp.OVERTIDTILL .
         tidtemp.TOTALT = klockan100(tidertemp.TOTALT).
         ASSIGN  
         tidtemp.RESTIM = klockan100(tidtemp.SLUT) - klockan100(tidtemp.START).
         tidtemp.LUNCH = (klockan100(tidtemp.SLUT) - klockan100(tidtemp.START)) - tidtemp.TOTALT.
         ASSIGN
         tidtemp.RESKOSTDEC = tidtemp.RESTIM * tidtemp.RESPRIS.
         tidtemp.TOTALT = 0.     
   
       /*  tidtemp.START = 0
               tidtemp.SLUT = 0.*/
      END.
      ELSE DO:
         CREATE tidtemp.
         ASSIGN
         tidtemp.DEBET = tidertemp.DEBET
         tidtemp.NYBEF = FALSE
         tidtemp.PERSONALKOD = tidertemp.PERSONALKOD
         tidtemp.PRISTYP = tidertemp.PRISTYP
         tidtemp.AONR = tidertemp.AONR
         tidtemp.DELNR = tidertemp.DELNR
         tidtemp.DATUM = tidertemp.DATUM
         tidtemp.START = tidertemp.START
         tidtemp.SLUT = tidertemp.SLUT
         tidtemp.BEFATTNING = tidertemp.OVERTIDTILL .
         tidtemp.TOTALT = klockan100(tidertemp.TOTALT).
         ASSIGN
         tidtemp.PRIS = tidertemp.PRIS      
         tidtemp.OBER = FALSE
         tidtemp.OANT1 = ((DECIMAL(SUBSTRING(STRING(tidertemp.OANT1,"99.99"),1,2)) + 
                          DECIMAL(SUBSTRING(STRING(tidertemp.OANT1,"99.99"),4,2)) / 60) +
                         (DECIMAL(SUBSTRING(STRING(tidertemp.OANT2,"99.99"),1,2)) + 
                          DECIMAL(SUBSTRING(STRING(tidertemp.OANT2,"99.99"),4,2)) / 60) +
                         (DECIMAL(SUBSTRING(STRING(tidertemp.OANT3,"99.99"),1,2)) + 
                          DECIMAL(SUBSTRING(STRING(tidertemp.OANT3,"99.99"),4,2)) / 60)).
         ASSIGN 
         tidtemp.KOST = tidtemp.TOTALT * tidtemp.PRIS
         tidtemp.LUNCH = (klockan100(tidtemp.SLUT) - klockan100(tidtemp.START)) - tidtemp.TOTALT.       
         IF FAKTREGLER.OVERTIDRGL = "INGA" THEN DO:
            tidtemp.OANT1 = 0.
         END.
         IF tidtemp.OANT1 NE 0 THEN DO: 
            ASSIGN
            tidtemp.OTIMMAR = tidtemp.OANT1
            tidtemp.KOST = 0
            tidtemp.TOTALT = 0
            tidtemp.OKOST = tidtemp.OANT1 * tidtemp.PRIS 
            tidtemp.OPRIS = tidtemp.PRIS.
         END.
         IF FAKTREGLER.OVERTIDRGL = "EGNA REGLER" THEN  tidtemp.OKOST = 0.
      END.
   END.
   IF tidertemp.LONTILLANTAL NE 0 THEN DO:
      IF kollvecko = "UTAN@" AND FAKTREGLER.LONRGL = "ENDAST MILERSÄTTNING" THEN DO:
         ASSIGN
         tidtemp.LONTILLAGG   = tidertemp.LONTILLAGG
         tidtemp.LONTILLANTAL = tidertemp.LONTILLANTAL.                 
      END.
   END.
   IF tidertemp.TRAKTANTAL NE 0 THEN DO:
      IF FAKTREGLER.TRAKTRGL = "INGA" THEN musz = musz.
      ELSE IF kollvecko = "UTAN@" THEN DO:
         ASSIGN
         tidtemp.TRAKTKOD     = tidertemp.TRAKTKOD
         tidtemp.TRAKTANTAL   = tidertemp.TRAKTANTAL.
      END.
   END.
   IF tidtemp.DEBET = FALSE THEN DO:
      RUN kreditpost_UI (INPUT 1).         
   END.
   IF tidtemp.DEBET = FALSE THEN DO:
      RUN kreditpost_UI (INPUT 2).         
   END.
END PROCEDURE.
   DEFINE VARIABLE pkod AS CHARACTER NO-UNDO.
   DEFINE VARIABLE sumanstf AS CHARACTER NO-UNDO.
   DEFINE VARIABLE sumanstraf AS CHARACTER NO-UNDO.
   pkod = "".
   FOR EACH tidtemp USE-INDEX PKOD:         
      CREATE sumtidtemp.
      IF pkod NE tidtemp.PERSONALKOD THEN DO:               
         ASSIGN
         sumanstf   = tidtemp.KOD
         sumanstraf = tidtemp.TRAAVTAL.
         pkod = tidtemp.PERSONALKOD.
      END.
      ASSIGN
      sumtidtemp.ANSF     = sumanstf  
      sumtidtemp.TRAAVTAL = sumanstraf
      sumtidtemp.PERSMASK = tidtemp.PERSMASK
      sumtidtemp.BEFATTNING = tidtemp.BEFATTNING               
      sumtidtemp.NAMN = tidtemp.NAMN.   
      IF tidtemp.PRISTYP = "RESTID..." THEN DO:
         ASSIGN
         sumtidtemp.TRAKTKOD     = tidtemp.TRAKTKOD    
         sumtidtemp.TRAKTANTAL   = tidtemp.TRAKTANTAL  
         sumtidtemp.LONTILLAGG   = tidtemp.LONTILLAGG  
         sumtidtemp.LONTILLANTAL = tidtemp.LONTILLANTAL
         sumtidtemp.OTEXTID = tidtemp.OTEXTID
         sumtidtemp.DATUM = tidtemp.DATUM
         sumtidtemp.GSTART = tidtemp.START
         sumtidtemp.GSLUT = tidtemp.SLUT
         sumtidtemp.START = tidtemp.START
         sumtidtemp.SLUT = tidtemp.SLUT
         sumtidtemp.PERSONALKOD = tidtemp.PERSONALKOD
         sumtidtemp.AONR = tidtemp.AONR
         sumtidtemp.DELNR = tidtemp.DELNR                  
         sumtidtemp.MED = TRUE 
         sumtidtemp.RESPRIS = tidtemp.RESPRIS 
         sumtidtemp.RESKOSTDEC = sumtidtemp.RESKOSTDEC + tidtemp.RESKOSTDEC
         sumtidtemp.RESTIM = sumtidtemp.RESTIM + tidtemp.RESTIM.
      END.
      ELSE DO:      
         ASSIGN
         sumtidtemp.TRAKTKOD     = tidtemp.TRAKTKOD    
         sumtidtemp.TRAKTANTAL   = tidtemp.TRAKTANTAL  
         sumtidtemp.LONTILLAGG   = tidtemp.LONTILLAGG  
         sumtidtemp.LONTILLANTAL = tidtemp.LONTILLANTAL
         sumtidtemp.START = tidtemp.START
         sumtidtemp.SLUT = tidtemp.SLUT
         sumtidtemp.GSTART = tidtemp.START 
         sumtidtemp.GSLUT = tidtemp.SLUT         
         sumtidtemp.OTEXTID = tidtemp.OTEXTID
         sumtidtemp.DATUM = tidtemp.DATUM
         sumtidtemp.PERSONALKOD = tidtemp.PERSONALKOD
         sumtidtemp.AONR = tidtemp.AONR
         sumtidtemp.DELNR = tidtemp.DELNR         
         sumtidtemp.TIMMAR = tidtemp.TOTALT
         sumtidtemp.BELOPP = tidtemp.KOST 
         sumtidtemp.OBELOPP = tidtemp.OKOST
         sumtidtemp.OTIMMAR = tidtemp.OTIMMAR                                        
         sumtidtemp.PRISA = tidtemp.PRIS
         sumtidtemp.OPRIS = tidtemp.OPRIS
         sumtidtemp.LUNCH = tidtemp.LUNCH
         sumtidtemp.MED = TRUE.   
      END.
   END.      
   DEFINE VARIABLE infakplannr AS INTEGER NO-UNDO.
   infakplannr = 90.
   DEFINE TEMP-TABLE kundbeftemp NO-UNDO LIKE FAKTBEF
      FIELD VIBEFATTNING AS CHARACTER.
FOR EACH FAKTBEF WHERE FAKTBEF.FAKTNR = FAKTPLAN.FAKTNR NO-LOCK:
      CREATE kundbeftemp.
      BUFFER-COPY FAKTBEF TO kundbeftemp.
      FOR EACH kundbeftemp,
      EACH BEFATTNINGSTAB WHERE BEFATTNINGSTAB.BEFATTNING = kundbeftemp.BEFATTNING NO-LOCK:
         kundbeftemp.VIBEFATTNING = BEFATTNINGSTAB.NAMN.      
      END.      
   END.   
OPEN QUERY faktbefq FOR EACH kundbeftemp WHERE kundbeftemp.FAKTNR = infakplannr
   NO-LOCK.
   GET FIRST faktbefq NO-LOCK.
   DO WHILE AVAILABLE(kundbeftemp):         
      FOR EACH sumtidtemp WHERE sumtidtemp.BEFATTNING = kundbeftemp.BEFATTNING USE-INDEX BEF.         
         ASSIGN
         sumtidtemp.PERSBEF = sumtidtemp.PERSONALKOD + " " + kundbeftemp.VIBEFATTNING
         sumtidtemp.VIBEFATTNING = kundbeftemp.VIBEFATTNING
         sumtidtemp.RESKOSTDEC = sumtidtemp.RESTIM * kundbeftemp.PRISRES
         sumtidtemp.BELOPP = sumtidtemp.TIMMAR * kundbeftemp.PRISA
         sumtidtemp.RESPRIS = kundbeftemp.PRISRES
         sumtidtemp.PRISA = kundbeftemp.PRISA.
      END.
      GET NEXT faktbefq NO-LOCK.    
   END. 
   pkod = "".

FOR EACH sumtidtemp:
   IF sumtidtemp.datum = 02/07/2007 OR sumtidtemp.datum = 02/15/2007 THEN.
   ELSE DELETE sumtidtemp.
END.

MESSAGE "1" VIEW-AS ALERT-BOX.
FOR EACH sumtidtemp:
   DISPLAY sumtidtemp.datum sumtidtemp.aonr sumtidtemp.belopp sumtidtemp.gstart sumtidtemp.gslut.
END.
DEFINE VARIABLE fnr AS INTEGER NO-UNDO.
/*STIDAPP.P*/
fnr = 90.                    FAKTTID.VECKOKORD
OPEN QUERY stidq FOR EACH sumtidtemp NO-LOCK, 
EACH FAKTTID WHERE FAKTTID.FAKTNR = fnr AND
FAKTTID.VFAKTNR = 0 AND FAKTTID.AONR = sumtidtemp.AONR AND
FAKTTID.DELNR = sumtidtemp.DELNR AND 
FAKTTID.PERSONALKOD = sumtidtemp.PERSONALKOD AND 
FAKTTID.DATUM = sumtidtemp.DATUM AND FAKTTID.GSTART = sumtidtemp.GSTART AND 
FAKTTID.GSLUT = sumtidtemp.GSLUT AND FAKTTID.VECKOKORD = sumtidtemp.VECKOKORD NO-LOCK.
GET FIRST stidq.
DO WHILE AVAILABLE(sumtidtemp):
   ASSIGN
   sumtidtemp.START = FAKTTID.START 
   sumtidtemp.SLUT = FAKTTID.SLUT
   sumtidtemp.OTEXTID = FAKTTID.OTEXTID
   sumtidtemp.PERSMASK = FAKTTID.PERSMASK
   sumtidtemp.BEFATTNING = FAKTTID.BEFATTNING
   sumtidtemp.MED = FAKTTID.MED         
   sumtidtemp.TIMMAR = FAKTTID.TIMMAR
   sumtidtemp.BELOPP = FAKTTID.BELOPP        
   sumtidtemp.OBELOPP = FAKTTID.OBELOPP       
   sumtidtemp.OTIMMAR = FAKTTID.OTIMMAR             
   sumtidtemp.RESPRIS = FAKTTID.RESPRIS
   sumtidtemp.OPRIS = FAKTTID.OPRIS
   sumtidtemp.RESTIM = FAKTTID.DECRESTID
   sumtidtemp.RESKOSTDEC = FAKTTID.RESKOSTDEC
   sumtidtemp.OANT1 = FAKTTID.OANT1.  
   GET NEXT stidq.
END.
CLOSE QUERY stidq.
FOR EACH sumtidtemp:
   IF sumtidtemp.datum = 02/07/2007 OR sumtidtemp.datum = 02/15/2007 THEN.
   ELSE DELETE sumtidtemp.
END.
MESSAGE "2" VIEW-AS ALERT-BOX.
FOR EACH sumtidtemp:
   DISPLAY sumtidtemp.datum sumtidtemp.aonr sumtidtemp.belopp sumtidtemp.gstart sumtidtemp.gslut.
END.
/**GTIDAPP.P*/
OPEN QUERY faktidq FOR EACH FAKTTID WHERE 
FAKTTID.VFAKTNR = 0 AND FAKTTID.FAKTNR = fnr AND
FAKTTID.SENASTFAK = ? NO-LOCK.
GET FIRST faktidq NO-LOCK.
DO WHILE AVAILABLE(FAKTTID):
   FIND FIRST sumtidtemp WHERE 
   sumtidtemp.PERSONALKOD = FAKTTID.PERSONALKOD AND 
   sumtidtemp.AONR = FAKTTID.AONR AND 
   sumtidtemp.DELNR = FAKTTID.DELNR AND
   sumtidtemp.DATUM = FAKTTID.DATUM AND 
   sumtidtemp.START = FAKTTID.START AND 
   sumtidtemp.SLUT = FAKTTID.SLUT AND
   sumtidtemp.GSTART = FAKTTID.GSTART AND 
   sumtidtemp.GSLUT = FAKTTID.GSLUT AND sumtidtemp.VECKOKORD = FAKTTID.VECKOKORD NO-ERROR.
   IF NOT AVAILABLE sumtidtemp THEN CREATE sumtidtemp.      
   ASSIGN                                                  
   sumtidtemp.PERSONALKOD = FAKTTID.PERSONALKOD
   sumtidtemp.NAMN = FAKTTID.NAMN 
   sumtidtemp.AONR = FAKTTID.AONR
   sumtidtemp.DELNR = FAKTTID.DELNR
   sumtidtemp.TIMMAR = FAKTTID.TIMMAR
   sumtidtemp.BELOPP = FAKTTID.BELOPP        
   sumtidtemp.OBELOPP = FAKTTID.OBELOPP 
   sumtidtemp.TBELOPP = FAKTTID.TBELOPP             
   sumtidtemp.OTIMMAR = FAKTTID.OTIMMAR 
   sumtidtemp.LONKOST = FAKTTID.LONKOST                  
   sumtidtemp.PERSMASK = FAKTTID.PERSMASK
   sumtidtemp.BEFATTNING = FAKTTID.BEFATTNING      
   sumtidtemp.PERSMASK = FAKTTID.PERSMASK
   sumtidtemp.TRAKTKOD = FAKTTID.TRAKTKOD
   sumtidtemp.TRAKTANTAL = FAKTTID.TRAKTANTAL  
   sumtidtemp.LONTILLAGG = FAKTTID.LONTILLAGG      
   sumtidtemp.LONTILLANTAL = FAKTTID.LONTILLANTAL 
   sumtidtemp.PRISA = FAKTTID.PRISA 
   sumtidtemp.ENDAGS = FAKTTID.ENDAGS       
   sumtidtemp.MED = FAKTTID.MED      
   sumtidtemp.PRISTYP = FAKTTID.PRISTYP
   sumtidtemp.RESTIM = FAKTTID.DECRESTID
   sumtidtemp.RESPRIS = FAKTTID.RESPRIS
   sumtidtemp.OPRIS = FAKTTID.OPRIS
   sumtidtemp.RESKOSTDEC = FAKTTID.RESKOSTDEC
   sumtidtemp.OTEXTID = FAKTTID.OTEXTID
   sumtidtemp.DATUM = FAKTTID.DATUM
   sumtidtemp.START = FAKTTID.START 
   sumtidtemp.SLUT = FAKTTID.SLUT
   sumtidtemp.GSTART = FAKTTID.GSTART 
   sumtidtemp.GSLUT = FAKTTID.GSLUT
   sumtidtemp.LUNCH = FAKTTID.LUNCH
   sumtidtemp.OANT1 = FAKTTID.OANT1.      
   GET NEXT faktidq NO-LOCK.
END.             
CLOSE QUERY faktidq.
FOR EACH sumtidtemp:
   IF sumtidtemp.datum = 02/07/2007 OR sumtidtemp.datum = 02/15/2007 THEN.
   ELSE DELETE sumtidtemp.
END.
MESSAGE "3" VIEW-AS ALERT-BOX.
FOR EACH sumtidtemp:
   DISPLAY sumtidtemp.datum sumtidtemp.aonr sumtidtemp.belopp sumtidtemp.gstart sumtidtemp.gslut.
END.


