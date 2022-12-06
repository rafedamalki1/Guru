/*FLKOMPAP.P*/
&Scoped-define NEW NEW
{TIDPERS.I}
{TIDUTTTNEW.I}
DEFINE TEMP-TABLE namntemp
   FIELD EFTERNAMN AS CHARACTER
   FIELD FORNAMN AS CHARACTER  
   FIELD NAMN AS CHARACTER  
   FIELD NTEXT AS CHARACTER
   FIELD REGIS AS CHARACTER  
   FIELD DATUM AS DATE
   FIELD ALLVAL AS INTEGER
   FIELD MANADVAR AS INTEGER.     

DEFINE TEMP-TABLE godpers       
   FIELD TIDSGODK AS CHARACTER.
DEFINE TEMP-TABLE omrpers       
   FIELD OMRADE AS CHARACTER.
DEFINE TEMP-TABLE ansvpers       
   FIELD ANSVARIGTIDR AS CHARACTER.


DEFINE TEMP-TABLE ftid
   FIELD PERSONALKOD AS CHARACTER  
   FIELD FORNAMN AS CHARACTER 
   FIELD EFTERNAMN AS CHARACTER 
   FIELD DATUM AS DATE  
   FIELD KNAPP AS CHARACTER
   FIELD TID AS DECIMAL
   FIELD KOM AS LOGICAL
   FIELD GICK AS LOGICAL
   FIELD KORD AS DATE
   FIELD AUTO AS CHARACTER
   FIELD AONR AS CHARACTER
   FIELD ORSAK AS CHARACTER
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD DATUM ASCENDING.

DEFINE INPUT PARAMETER RAD_ALLVAL AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER datar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER manad AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER valaonr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER tomdat AS DATE NO-UNDO.
DEFINE INPUT PARAMETER listtext AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tidpers.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR namntemp.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.

DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.

DEFINE VARIABLE ingsaldo AS DECIMAL FORMAT "-999.99" NO-UNDO.
DEFINE VARIABLE ftot AS DECIMAL FORMAT "-999.99" NO-UNDO.
DEFINE VARIABLE mtot AS DECIMAL FORMAT "-999.99" NO-UNDO.
DEFINE VARIABLE isaldo AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE hjdat AS DATE NO-UNDO.
DEFINE VARIABLE splus AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(140)" NO-UNDO.
DEFINE VARIABLE manadnr AS INTEGER FORMAT "99" NO-UNDO. 
DEFINE VARIABLE vpers AS CHARACTER NO-UNDO.
DEFINE VARIABLE vomr AS CHARACTER NO-UNDO.
DEFINE VARIABLE stdatum AS DATE NO-UNDO.
DEFINE VARIABLE sldatum AS DATE NO-UNDO.
FIND FIRST namntemp NO-ERROR.
RUN tolk_UI.
{GDPRLOGGCLIENT.I}
PROCEDURE tolk_UI :
   IF datar NE 0 THEN RUN skapaflex2_UI. 
   ELSE RUN skapaflex_UI.  
END PROCEDURE.

PROCEDURE huvud_UI :
     /*HUVUD*/ 
   FIND FIRST tidpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.      
   IF RAD_ALLVAL = 2 THEN DO:
      namntemp.REGIS = Guru.Konstanter:gomrk + ":".
      FIND CURRENT omrpers NO-LOCK NO-ERROR.            
      IF AVAILABLE omrpers  THEN DO:
         FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = omrpers.OMRADE USE-INDEX OMR NO-LOCK NO-ERROR.
         ASSIGN
         namntemp.NAMN = OMRADETAB.NAMN.
      END.
   END.
   IF RAD_ALLVAL = 3 THEN DO:
      namntemp.REGIS = "Alla".
   END.
    IF RAD_ALLVAL = 4 THEN DO:
      namntemp.REGIS = "".
   END.
   IF RAD_ALLVAL = 5 THEN namntemp.REGIS = "Markerade enheter".   
   IF RAD_ALLVAL = 6 THEN DO:
      namntemp.REGIS = "Godkännare:".
      FIND CURRENT godpers NO-LOCK NO-ERROR.            
      IF AVAILABLE godpers  THEN DO:         
         FIND FIRST GODKANNARTAB WHERE GODKANNARTAB.PERSONALKOD = godpers.TIDSGODK NO-LOCK NO-ERROR.
         IF AVAILABLE GODKANNARTAB  THEN DO:
            ASSIGN      
            namntemp.FORNAMN = GODKANNARTAB.FORNAMN + " " + GODKANNARTAB.EFTERNAMN + "  " + GODKANNARTAB.PERSONALKOD.  
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + GODKANNARTAB.PERSONALKOD.
         END.
      END.
   END.
   CREATE tidut. 
   IF manad = 1 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,4) = listtext 
      SUBSTRING(tidut.UT,60) = STRING(TODAY)
      SUBSTRING(tidut.UT,70) = STRING(TIME,"HH:MM:SS").
      CREATE tidut. 
      ASSIGN
      SUBSTRING(tidut.UT,4) = "======================================" .
   END.
   ELSE DO:
      ASSIGN
      SUBSTRING(tidut.UT,4) = listtext 
      SUBSTRING(tidut.UT,60) = STRING(TODAY)
      SUBSTRING(tidut.UT,70) = STRING(TIME,"HH:MM:SS").
      CREATE tidut. 
      ASSIGN
      SUBSTRING(tidut.UT,4) = "===============================================" .     
   END.    
   CREATE tidut.    
   ASSIGN
   SUBSTRING(tidut.UT,5) = namntemp.REGIS.
   IF RAD_ALLVAL = 1 THEN ASSIGN SUBSTRING(tidut.UT,25) = namntemp.FORNAMN.
   IF RAD_ALLVAL = 2 THEN ASSIGN SUBSTRING(tidut.UT,25) = namntemp.NAMN.
   IF RAD_ALLVAL = 6 THEN ASSIGN SUBSTRING(tidut.UT,25) = namntemp.FORNAMN.
   CREATE tidut.
   CREATE tidut.    
   ASSIGN
   SUBSTRING(tidut.UT,1) = "Enhet"
   SUBSTRING(tidut.UT,9) = "Förnamn"
   SUBSTRING(tidut.UT,19) = "Efternamn"
   SUBSTRING(tidut.UT,35) = "Datum"
   SUBSTRING(tidut.UT,44) = "Knapp"
   SUBSTRING(tidut.UT,55) = "Tid"
   SUBSTRING(tidut.UT,61) = Guru.Konstanter:gaok.        
   IF valaonr = "GLOM" THEN DO:
         SUBSTRING(tidut.UT,68) = "Orsak".
         SUBSTRING(tidut.UT,88) = "Kommentar".
         str =                                                                                                                            
"=======.=========.===============.========.==========.=====.======.==================.=========================".
   END.   
   ELSE DO:
      ASSIGN
      SUBSTRING(tidut.UT,68) = "Orsak"   
   str =                                                                                                                            
"=======.=========.===============.========.==========.=====.======.====================".
   END.             
   CREATE tidut.                  
   ASSIGN
   SUBSTRING(tidut.UT,1) = str.   
END PROCEDURE.

PROCEDURE skapaflex_UI :
   IF RAD_ALLVAL = 3 OR RAD_ALLVAL = 4 OR RAD_ALLVAL = 5 THEN DO:  
      FIND FIRST FLEXREG USE-INDEX FLEX NO-LOCK NO-ERROR.
      IF manad = 1 THEN DO:         
         IF valaonr = "GLOM" OR valaonr = "Korr" OR valaonr = "PERI" THEN DO:
            OPEN QUERY flgam FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK,
            EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
            AND FLEXTID.AUTO = valaonr
            AND FLEXTID.KORD = 01/01/97 AND FLEXTID.DATUM LE tomdat USE-INDEX KORD NO-LOCK. 
         END.         
         ELSE IF valaonr = "ANNAT" THEN DO:
            OPEN QUERY flgam FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK,
            EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
            AND FLEXTID.KNAPP BEGINS valaonr
            AND FLEXTID.KORD = 01/01/97 AND FLEXTID.DATUM LE tomdat USE-INDEX KORD NO-LOCK. 
         END.
         ELSE DO:         
            OPEN QUERY flgam FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK,
            EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
            AND FLEXTID.AONR = valaonr
            AND FLEXTID.KORD = 01/01/97 AND FLEXTID.DATUM LE tomdat USE-INDEX KORD NO-LOCK.   
         END.
         GET FIRST flgam NO-LOCK.
         DO WHILE AVAILABLE(FLEXTID):        
            CREATE ftid.
            ASSIGN
            ftid.PERSONALKOD = FLEXTID.PERSONALKOD
            ftid .FORNAMN = tidpers.FORNAMN
            ftid.EFTERNAMN = tidpers.EFTERNAMN
            ftid.DATUM = FLEXTID.DATUM
            ftid.KNAPP = FLEXTID.KNAPP
            ftid.TID = FLEXTID.TID      
            ftid.GICK = FLEXTID.GICK          
            ftid.KOM = FLEXTID.KOM
            ftid.KORD = FLEXTID.KORD
            ftid.AUTO = FLEXTID.AUTO
            ftid.AONR = FLEXTID.AONR
            ftid.ORSAK = FLEXTID.ORSAK.
            GET NEXT flgam NO-LOCK.
         END.              
         CLOSE QUERY flgam.    
      END.
      ELSE DO:   
         IF valaonr = "GLOM" OR valaonr = "Korr" OR valaonr = "PERI" THEN DO:
            OPEN QUERY fgam FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK,
            EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
            AND FLEXTID.AUTO = valaonr
            AND FLEXTID.KORD = FLEXREG.SALDOKORD USE-INDEX KORD NO-LOCK. 
         END.
         ELSE IF valaonr = "ANNAT" THEN DO:
            OPEN QUERY fgam FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK,
            EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
            AND FLEXTID.KNAPP BEGINS valaonr
            AND FLEXTID.KORD = FLEXREG.SALDOKORD USE-INDEX KORD NO-LOCK. 
         END.
         ELSE DO:          
            OPEN QUERY fgam FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK,
            EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD AND
            FLEXTID.AONR = valaonr AND
            FLEXTID.KORD = FLEXREG.SALDOKORD USE-INDEX KORD NO-LOCK. 
         END.
         GET FIRST fgam NO-LOCK.
         DO WHILE AVAILABLE(FLEXTID):        
            CREATE ftid.
            ASSIGN
            ftid.PERSONALKOD = FLEXTID.PERSONALKOD
            ftid .FORNAMN = tidpers.FORNAMN
            ftid.EFTERNAMN = tidpers.EFTERNAMN
            ftid.DATUM = FLEXTID.DATUM
            ftid.KNAPP = FLEXTID.KNAPP
            ftid.TID = FLEXTID.TID      
            ftid.GICK = FLEXTID.GICK          
            ftid.KOM = FLEXTID.KOM
            ftid.KORD = FLEXTID.KORD
            ftid.AUTO = FLEXTID.AUTO
            ftid.AONR = FLEXTID.AONR
            ftid.ORSAK = FLEXTID.ORSAK.
            GET NEXT fgam NO-LOCK.
         END.         
         CLOSE QUERY fgam.                 
      END.   
      RUN huvud_UI.  
      RUN skapaut_UI.
   END.
   ELSE DO:      
      FOR EACH tidpers:
         FIND FIRST godpers WHERE godpers.tidsgodk = SUBSTRING(tidpers.ANSVARIGTIDR,7,5)
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE godpers THEN DO:            
            CREATE godpers.
            ASSIGN godpers.TIDSGODK = SUBSTRING(tidpers.ANSVARIGTIDR,7,5).
         END.
         FIND FIRST ansvpers WHERE ansvpers.ansvarigtidr = SUBSTRING(tidpers.ANSVARIGTIDR,1,5)
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ansvpers THEN DO:
            CREATE ansvpers.
            ASSIGN ansvpers.ANSVARIGTIDR = SUBSTRING(tidpers.ANSVARIGTIDR,1,5).
         END.
         FIND FIRST omrpers WHERE omrpers.OMRADE = tidpers.OMRADE NO-LOCK NO-ERROR.
         IF NOT AVAILABLE omrpers THEN DO:
            CREATE omrpers.
            ASSIGN omrpers.OMRADE = tidpers.OMRADE.
         END.
      END.
      IF RAD_ALLVAL = 6 THEN DO:
      
         FIND FIRST godpers NO-LOCK NO-ERROR.
         vpers = godpers.TIDSGODK.
         
         REPEAT:
            FIND FIRST tidpers WHERE SUBSTRING(tidpers.ANSVARIGTIDR,7,5) = godpers.TIDSGODK USE-INDEX PERSONALKOD NO-LOCK NO-ERROR. 
            FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = SUBSTRING(tidpers.ANSVARIGTIDR,7,5)
            USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.                                    
      
            FIND FIRST FLEXREG USE-INDEX FLEX NO-LOCK NO-ERROR.
            IF manad = 1 THEN DO:
               IF valaonr = "GLOM" OR valaonr = "Korr" OR valaonr = "PERI" THEN DO:
                  OPEN QUERY flgam FOR EACH tidpers WHERE SUBSTRING(tidpers.ANSVARIGTIDR,7,5) = vpers USE-INDEX PERSONALKOD NO-LOCK,
                  EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
                  AND FLEXTID.AUTO = valaonr
                  AND FLEXTID.KORD = 01/01/97 AND FLEXTID.DATUM LE tomdat USE-INDEX KORD NO-LOCK. 
               END.
               ELSE IF valaonr = "ANNAT" THEN DO:
                  OPEN QUERY flgam FOR EACH tidpers WHERE SUBSTRING(tidpers.ANSVARIGTIDR,7,5) = vpers USE-INDEX PERSONALKOD NO-LOCK,
                  EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
                  AND FLEXTID.KNAPP BEGINS valaonr
                  AND FLEXTID.KORD = 01/01/97 AND FLEXTID.DATUM LE tomdat USE-INDEX KORD NO-LOCK. 
               END.
               ELSE DO:
                  OPEN QUERY flgam FOR EACH tidpers WHERE SUBSTRING(tidpers.ANSVARIGTIDR,7,5) = vpers USE-INDEX PERSONALKOD NO-LOCK,
                  EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
                  AND FLEXTID.AONR = valaonr
                  AND FLEXTID.KORD = 01/01/97 AND FLEXTID.DATUM LE tomdat USE-INDEX KORD NO-LOCK.   
               END.
               GET FIRST flgam NO-LOCK.
               DO WHILE AVAILABLE(FLEXTID):        
                  CREATE ftid.
                  ASSIGN
                  ftid.PERSONALKOD = FLEXTID.PERSONALKOD
                  ftid .FORNAMN = tidpers.FORNAMN
                  ftid.EFTERNAMN = tidpers.EFTERNAMN
                  ftid.DATUM = FLEXTID.DATUM
                  ftid.KNAPP = FLEXTID.KNAPP
                  ftid.TID = FLEXTID.TID      
                  ftid.GICK = FLEXTID.GICK          
                  ftid.KOM = FLEXTID.KOM
                  ftid.KORD = FLEXTID.KORD
                  ftid.AUTO = FLEXTID.AUTO
                  ftid.AONR = FLEXTID.AONR
                  ftid.ORSAK = FLEXTID.ORSAK.
                  GET NEXT flgam NO-LOCK.
               END.              
               CLOSE QUERY flgam.    
            END.
            ELSE DO:   
               IF valaonr = "GLOM" OR valaonr = "Korr" OR valaonr = "PERI" THEN DO:
                  OPEN QUERY fgam FOR EACH tidpers WHERE SUBSTRING(tidpers.ANSVARIGTIDR,7,5) = vpers USE-INDEX PERSONALKOD NO-LOCK,
                  EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
                  AND FLEXTID.AUTO = valaonr
                  AND FLEXTID.KORD = FLEXREG.SALDOKORD USE-INDEX KORD NO-LOCK. 
               END.
               ELSE IF valaonr = "ANNAT" THEN DO:
                  OPEN QUERY fgam FOR EACH tidpers WHERE SUBSTRING(tidpers.ANSVARIGTIDR,7,5) = vpers USE-INDEX PERSONALKOD NO-LOCK,
                  EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
                  AND FLEXTID.KNAPP BEGINS valaonr
                  AND FLEXTID.KORD = FLEXREG.SALDOKORD USE-INDEX KORD NO-LOCK. 
               END.
               ELSE DO:
                  OPEN QUERY fgam FOR EACH tidpers WHERE SUBSTRING(tidpers.ANSVARIGTIDR,7,5) = vpers USE-INDEX PERSONALKOD NO-LOCK,
                  EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD AND
                  FLEXTID.AONR = valaonr AND
                  FLEXTID.KORD = FLEXREG.SALDOKORD USE-INDEX KORD NO-LOCK. 
               END.
               GET FIRST fgam NO-LOCK.
               DO WHILE AVAILABLE(FLEXTID):        
                  CREATE ftid.
                  ASSIGN
                  ftid.PERSONALKOD = FLEXTID.PERSONALKOD
                  ftid .FORNAMN = tidpers.FORNAMN
                  ftid.EFTERNAMN = tidpers.EFTERNAMN
                  ftid.DATUM = FLEXTID.DATUM
                  ftid.KNAPP = FLEXTID.KNAPP
                  ftid.TID = FLEXTID.TID      
                  ftid.GICK = FLEXTID.GICK          
                  ftid.KOM = FLEXTID.KOM
                  ftid.KORD = FLEXTID.KORD
                  ftid.AUTO = FLEXTID.AUTO
                  ftid.AONR = FLEXTID.AONR
                  ftid.ORSAK = FLEXTID.ORSAK.
                  GET NEXT fgam NO-LOCK.
               END.         
               CLOSE QUERY fgam.                 
            END.                           
            FIND FIRST tidpers WHERE SUBSTRING(tidpers.ANSVARIGTIDR,7,5) = godpers.TIDSGODK USE-INDEX PERSONALKOD NO-LOCK NO-ERROR. 
            FIND FIRST ftid NO-LOCK NO-ERROR.
            IF AVAILABLE ftid THEN DO:            
               CREATE tidut.                           
               RUN huvud_UI.              
               RUN skapaut_UI.
            END.
            FIND NEXT godpers NO-LOCK NO-ERROR.            
            IF NOT AVAILABLE godpers THEN LEAVE.
            ELSE vpers = godpers.TIDSGODK.
         END.    
      END.      
      ELSE IF RAD_ALLVAL = 2 THEN DO:
         FIND FIRST omrpers NO-LOCK NO-ERROR.              
         REPEAT:
            FIND FIRST tidpers WHERE tidpers.OMRADE = omrpers.omrade USE-INDEX PERSONALKOD NO-LOCK NO-ERROR. 
            FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = tidpers.OMRADE
            USE-INDEX OMR NO-LOCK NO-ERROR.
            vomr = OMRADETAB.OMRADE.

      
            FIND FIRST FLEXREG USE-INDEX FLEX NO-LOCK NO-ERROR.
            IF manad = 1 THEN DO:
               IF valaonr = "GLOM" OR valaonr = "Korr" OR valaonr = "PERI" THEN DO:
                  OPEN QUERY flgam FOR EACH tidpers WHERE tidpers.OMRADE = vomr USE-INDEX PERSONALKOD NO-LOCK,
                  EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
                  AND FLEXTID.AUTO = valaonr
                  AND FLEXTID.KORD = 01/01/97 AND FLEXTID.DATUM LE tomdat USE-INDEX KORD NO-LOCK. 
               END.         
               ELSE IF valaonr = "ANNAT" THEN DO:
                  OPEN QUERY flgam FOR EACH tidpers WHERE tidpers.OMRADE = vomr USE-INDEX PERSONALKOD NO-LOCK,
                  EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
                  AND FLEXTID.KNAPP BEGINS valaonr
                  AND FLEXTID.KORD = 01/01/97 AND FLEXTID.DATUM LE tomdat USE-INDEX KORD NO-LOCK. 
               END.
               ELSE DO:         
                  OPEN QUERY flgam FOR EACH tidpers WHERE tidpers.OMRADE = vomr USE-INDEX PERSONALKOD NO-LOCK,
                  EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
                  AND FLEXTID.AONR = valaonr
                  AND FLEXTID.KORD = 01/01/97 AND FLEXTID.DATUM LE tomdat USE-INDEX KORD NO-LOCK.   
               END.                           
               GET FIRST flgam NO-LOCK.
               DO WHILE AVAILABLE(FLEXTID):        
                  CREATE ftid.
                  ASSIGN
                  ftid.PERSONALKOD = FLEXTID.PERSONALKOD
                  ftid .FORNAMN = tidpers.FORNAMN
                  ftid.EFTERNAMN = tidpers.EFTERNAMN
                  ftid.DATUM = FLEXTID.DATUM
                  ftid.KNAPP = FLEXTID.KNAPP
                  ftid.TID = FLEXTID.TID      
                  ftid.GICK = FLEXTID.GICK          
                  ftid.KOM = FLEXTID.KOM
                  ftid.KORD = FLEXTID.KORD
                  ftid.AUTO = FLEXTID.AUTO
                  ftid.AONR = FLEXTID.AONR
                  ftid.ORSAK = FLEXTID.ORSAK.
                  GET NEXT flgam NO-LOCK.
               END.              
               CLOSE QUERY flgam.    
            END.
            ELSE DO:   
               IF valaonr = "GLOM" OR valaonr = "Korr" OR valaonr = "PERI" THEN DO:
                  OPEN QUERY fgam FOR EACH tidpers WHERE tidpers.OMRADE = vomr USE-INDEX PERSONALKOD NO-LOCK,
                  EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
                  AND FLEXTID.AUTO = valaonr
                  AND FLEXTID.KORD = FLEXREG.SALDOKORD USE-INDEX KORD NO-LOCK. 
               END.
               ELSE IF valaonr = "ANNAT" THEN DO:
                  OPEN QUERY fgam FOR EACH tidpers WHERE tidpers.OMRADE = vomr USE-INDEX PERSONALKOD NO-LOCK,
                  EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
                  AND FLEXTID.KNAPP BEGINS valaonr
                  AND FLEXTID.KORD = FLEXREG.SALDOKORD USE-INDEX KORD NO-LOCK. 
               END.
               ELSE DO:          
                  OPEN QUERY fgam FOR EACH tidpers WHERE tidpers.OMRADE = vomr USE-INDEX PERSONALKOD NO-LOCK,
                  EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD AND
                  FLEXTID.AONR = valaonr AND
                  FLEXTID.KORD = FLEXREG.SALDOKORD USE-INDEX KORD NO-LOCK. 
               END.               
               GET FIRST fgam NO-LOCK.
               DO WHILE AVAILABLE(FLEXTID):        
                  CREATE ftid.
                  ASSIGN
                  ftid.PERSONALKOD = FLEXTID.PERSONALKOD
                  ftid .FORNAMN = tidpers.FORNAMN
                  ftid.EFTERNAMN = tidpers.EFTERNAMN
                  ftid.DATUM = FLEXTID.DATUM
                  ftid.KNAPP = FLEXTID.KNAPP
                  ftid.TID = FLEXTID.TID      
                  ftid.GICK = FLEXTID.GICK          
                  ftid.KOM = FLEXTID.KOM
                  ftid.KORD = FLEXTID.KORD
                  ftid.AUTO = FLEXTID.AUTO
                  ftid.AONR = FLEXTID.AONR
                  ftid.ORSAK = FLEXTID.ORSAK.
                  GET NEXT fgam NO-LOCK.
               END.         
               CLOSE QUERY fgam.                 
            END.                                       
            FIND FIRST ftid NO-LOCK NO-ERROR.
            IF AVAILABLE ftid THEN DO:            
               CREATE tidut.                  
               RUN huvud_UI.              
               RUN skapaut_UI.
            END.
            FIND NEXT omrpers NO-LOCK NO-ERROR.            
            IF NOT AVAILABLE omrpers THEN LEAVE.            
            ELSE vomr = tidpers.OMRADE.
         END.    
      END.
   END.   
END PROCEDURE.

PROCEDURE skapaflex2_UI :
   stdatum = DATE(manad,01,datar).    
     
   IF manad = 12 THEN sldatum = DATE(manad,31,datar).
   ELSE sldatum = DATE((manad + 1),01,datar) - 1.
   
   IF RAD_ALLVAL = 3 OR RAD_ALLVAL = 4 OR RAD_ALLVAL = 5 THEN DO:  
      FIND FIRST FLEXREG USE-INDEX FLEX NO-LOCK NO-ERROR.
               
      IF valaonr = "GLOM" OR valaonr = "Korr" OR valaonr = "PERI" THEN DO:
         OPEN QUERY flgam FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK,
         EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
         AND FLEXTID.AUTO = valaonr
         AND FLEXTID.DATUM GE stdatum AND FLEXTID.DATUM lE sldatum USE-INDEX FLEX  NO-LOCK. 
      END.         
      ELSE IF valaonr = "ANNAT" THEN DO:
         OPEN QUERY flgam FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK,
         EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
         AND FLEXTID.KNAPP BEGINS valaonr
         AND FLEXTID.DATUM GE stdatum AND FLEXTID.DATUM lE sldatum USE-INDEX FLEX  NO-LOCK. 
      END.
      ELSE DO:         
         OPEN QUERY flgam FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK,
         EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
         AND FLEXTID.AONR = valaonr
         AND FLEXTID.DATUM GE stdatum AND FLEXTID.DATUM lE sldatum  USE-INDEX FLEX  NO-LOCK.   
      END.
      GET FIRST flgam NO-LOCK.
      DO WHILE AVAILABLE(FLEXTID):        
         CREATE ftid.
         ASSIGN
         ftid.PERSONALKOD = FLEXTID.PERSONALKOD
         ftid .FORNAMN = tidpers.FORNAMN
         ftid.EFTERNAMN = tidpers.EFTERNAMN
         ftid.DATUM = FLEXTID.DATUM
         ftid.KNAPP = FLEXTID.KNAPP
         ftid.TID = FLEXTID.TID      
         ftid.GICK = FLEXTID.GICK          
         ftid.KOM = FLEXTID.KOM
         ftid.KORD = FLEXTID.KORD
         ftid.AUTO = FLEXTID.AUTO
         ftid.AONR = FLEXTID.AONR
         ftid.ORSAK = FLEXTID.ORSAK.
         GET NEXT flgam NO-LOCK.
      END.              
      CLOSE QUERY flgam.    
         
      RUN huvud_UI.  
      RUN skapaut_UI.
   END.
   ELSE DO:      
      FOR EACH tidpers:
         FIND FIRST godpers WHERE godpers.tidsgodk = SUBSTRING(tidpers.ANSVARIGTIDR,7,5)
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE godpers THEN DO:            
            CREATE godpers.
            ASSIGN godpers.TIDSGODK = SUBSTRING(tidpers.ANSVARIGTIDR,7,5).
         END.
   
         FIND FIRST omrpers WHERE omrpers.OMRADE = tidpers.OMRADE NO-LOCK NO-ERROR.
         IF NOT AVAILABLE omrpers THEN DO:
            CREATE omrpers.
            ASSIGN omrpers.OMRADE = tidpers.OMRADE.
         END.
      END.
      IF RAD_ALLVAL = 6 THEN DO:
      
         FIND FIRST godpers NO-LOCK NO-ERROR.
         vpers = godpers.TIDSGODK.
         
         REPEAT:
            FIND FIRST tidpers WHERE SUBSTRING(tidpers.ANSVARIGTIDR,7,5) = godpers.TIDSGODK USE-INDEX PERSONALKOD NO-LOCK NO-ERROR. 
            FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = SUBSTRING(tidpers.ANSVARIGTIDR,7,5)
            USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.                                    
      
            FIND FIRST FLEXREG USE-INDEX FLEX NO-LOCK NO-ERROR.            
            IF valaonr = "GLOM" OR valaonr = "Korr" OR valaonr = "PERI" THEN DO:
               OPEN QUERY flgam FOR EACH tidpers WHERE SUBSTRING(tidpers.ANSVARIGTIDR,7,5) = vpers USE-INDEX PERSONALKOD NO-LOCK,
               EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
               AND FLEXTID.AUTO = valaonr
               AND FLEXTID.DATUM GE stdatum AND FLEXTID.DATUM lE sldatum  USE-INDEX FLEX  NO-LOCK. 
            END.
            ELSE IF valaonr = "ANNAT" THEN DO:
               OPEN QUERY flgam FOR EACH tidpers WHERE SUBSTRING(tidpers.ANSVARIGTIDR,7,5) = vpers USE-INDEX PERSONALKOD NO-LOCK,
               EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
               AND FLEXTID.KNAPP BEGINS valaonr
               AND FLEXTID.DATUM GE stdatum AND FLEXTID.DATUM lE sldatum  USE-INDEX FLEX  NO-LOCK. 
            END.
            ELSE DO:
               OPEN QUERY flgam FOR EACH tidpers WHERE SUBSTRING(tidpers.ANSVARIGTIDR,7,5) = vpers USE-INDEX PERSONALKOD NO-LOCK,
               EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
               AND FLEXTID.AONR = valaonr
               AND FLEXTID.DATUM GE stdatum AND FLEXTID.DATUM lE sldatum  USE-INDEX FLEX  NO-LOCK.   
            END.
            GET FIRST flgam NO-LOCK.
            DO WHILE AVAILABLE(FLEXTID):        
               CREATE ftid.
               ASSIGN
               ftid.PERSONALKOD = FLEXTID.PERSONALKOD
               ftid .FORNAMN = tidpers.FORNAMN
               ftid.EFTERNAMN = tidpers.EFTERNAMN
               ftid.DATUM = FLEXTID.DATUM
               ftid.KNAPP = FLEXTID.KNAPP
               ftid.TID = FLEXTID.TID      
               ftid.GICK = FLEXTID.GICK          
               ftid.KOM = FLEXTID.KOM
               ftid.KORD = FLEXTID.KORD
               ftid.AUTO = FLEXTID.AUTO
               ftid.AONR = FLEXTID.AONR
               ftid.ORSAK = FLEXTID.ORSAK.
               GET NEXT flgam NO-LOCK.
            END.              
            CLOSE QUERY flgam.    
                                       
            FIND FIRST tidpers WHERE SUBSTRING(tidpers.ANSVARIGTIDR,7,5) = godpers.TIDSGODK USE-INDEX PERSONALKOD NO-LOCK NO-ERROR. 
            FIND FIRST ftid NO-LOCK NO-ERROR.
            IF AVAILABLE ftid THEN DO:            
               CREATE tidut.                           
               RUN huvud_UI.              
               RUN skapaut_UI.
            END.
            FIND NEXT godpers NO-LOCK NO-ERROR.            
            IF NOT AVAILABLE godpers THEN LEAVE.
            ELSE vpers = godpers.TIDSGODK.
         END.    
      END.      
      ELSE IF RAD_ALLVAL = 2 THEN DO:
         FIND FIRST omrpers NO-LOCK NO-ERROR.              
         REPEAT:
            FIND FIRST tidpers WHERE tidpers.OMRADE = omrpers.omrade USE-INDEX PERSONALKOD NO-LOCK NO-ERROR. 
            FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = tidpers.OMRADE
            USE-INDEX OMR NO-LOCK NO-ERROR.
            vomr = OMRADETAB.OMRADE.
      
            FIND FIRST FLEXREG USE-INDEX FLEX NO-LOCK NO-ERROR.
            
            IF valaonr = "GLOM" OR valaonr = "Korr" OR valaonr = "PERI" THEN DO:
               OPEN QUERY flgam FOR EACH tidpers WHERE tidpers.OMRADE = vomr USE-INDEX PERSONALKOD NO-LOCK,
               EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
               AND FLEXTID.AUTO = valaonr
               AND FLEXTID.DATUM GE stdatum AND FLEXTID.DATUM lE sldatum  USE-INDEX FLEX  NO-LOCK. 
            END.         
            ELSE IF valaonr = "ANNAT" THEN DO:
               OPEN QUERY flgam FOR EACH tidpers WHERE tidpers.OMRADE = vomr USE-INDEX PERSONALKOD NO-LOCK,
               EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
               AND FLEXTID.KNAPP BEGINS valaonr
               AND FLEXTID.DATUM GE stdatum AND FLEXTID.DATUM lE sldatum  USE-INDEX FLEX  NO-LOCK. 
            END.
            ELSE DO:         
               OPEN QUERY flgam FOR EACH tidpers WHERE tidpers.OMRADE = vomr USE-INDEX PERSONALKOD NO-LOCK,
               EACH FLEXTID WHERE FLEXTID.PERSONALKOD = tidpers.PERSONALKOD 
               AND FLEXTID.AONR = valaonr
               AND FLEXTID.DATUM GE stdatum AND FLEXTID.DATUM lE sldatum  USE-INDEX FLEX  NO-LOCK.   
            END.                           
            GET FIRST flgam NO-LOCK.
            DO WHILE AVAILABLE(FLEXTID):        
               CREATE ftid.
               ASSIGN
               ftid.PERSONALKOD = FLEXTID.PERSONALKOD
               ftid .FORNAMN = tidpers.FORNAMN
               ftid.EFTERNAMN = tidpers.EFTERNAMN
               ftid.DATUM = FLEXTID.DATUM
               ftid.KNAPP = FLEXTID.KNAPP
               ftid.TID = FLEXTID.TID      
               ftid.GICK = FLEXTID.GICK          
               ftid.KOM = FLEXTID.KOM
               ftid.KORD = FLEXTID.KORD
               ftid.AUTO = FLEXTID.AUTO
               ftid.AONR = FLEXTID.AONR
               ftid.ORSAK = FLEXTID.ORSAK.
               GET NEXT flgam NO-LOCK.
            END.              
            CLOSE QUERY flgam.    
            FIND FIRST ftid NO-LOCK NO-ERROR.
            IF AVAILABLE ftid THEN DO:            
               CREATE tidut.                  
               RUN huvud_UI.              
               RUN skapaut_UI.
            END.
            FIND NEXT omrpers NO-LOCK NO-ERROR.            
            IF NOT AVAILABLE omrpers THEN LEAVE.            
            ELSE vomr = tidpers.OMRADE.
         END.    
      END.
   END.   
END PROCEDURE.


PROCEDURE skapaut_UI :
   FOR EACH ftid USE-INDEX PERSONALKOD: 
      CREATE tidut. 
      ASSIGN
      SUBSTRING(tidut.UT,1) = ftid.PERSONALKOD
      SUBSTRING(tidut.UT,9) = ftid.FORNAMN
      SUBSTRING(tidut.UT,19) = ftid.EFTERNAMN
      SUBSTRING(tidut.UT,35) = STRING(ftid.DATUM)          
      SUBSTRING(tidut.UT,44) = SUBSTRING(ftid.KNAPP,1,10)
      SUBSTRING(tidut.UT,55) = STRING(ftid.TID,">9.99")
      SUBSTRING(tidut.UT,61) = SUBSTRING(ftid.AONR,1,6).
      IF valaonr = "GLOM" THEN DO:
         SUBSTRING(tidut.UT,68) = SUBSTRING(ftid.ORSAK,1,18).
         SUBSTRING(tidut.UT,88) = SUBSTRING(ftid.ORSAK,19).
      END.   
      ELSE SUBSTRING(tidut.UT,68) = ftid.ORSAK.  
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + ftid.PERSONALKOD.     
      DELETE ftid.
   END.            
   CREATE tidut. 
   ASSIGN 
   tidut.UT = str.
   CREATE tidut. 
   ASSIGN SUBSTRING(tidut.UT,132) = "$".       
END PROCEDURE.


