/*BERKOLL2.p*/     
DEFINE VARIABLE raknare AS INTEGER NO-UNDO. 

DEFIN QUERY qmtrl FOR MTRLBER.
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
{TIDUTTT.I}

DEFINE INPUT PARAMETER leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tidut.  
  
  
raknare = 0.     
FIND FIRST LEVERANTOR WHERE LEVERANTOR.LEVKOD = leverant NO-LOCK NO-ERROR.                
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
OPEN QUERY qmtrl FOR EACH MTRLBER WHERE MTRLBER.LEVKOD = leverant NO-LOCK.

GET FIRST qmtrl NO-LOCK.
DO WHILE AVAILABLE(MTRLBER):  
   IF AVAILABLE MTRLBER THEN DO:      
      FIND FIRST MTRL WHERE MTRL.LEVKOD = MTRLBER.LEVKOD AND 
      MTRL.KALKNR = 0 AND MTRL.ENR = MTRLBER.ENR USE-INDEX LEV NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MTRL THEN DO:          
         FIND FIRST tidut WHERE  SUBSTRING(tidut.UT,14,11) = MTRLBER.ENR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidut THEN DO:                              
            CREATE tidut.      
            ASSIGN         
            SUBSTRING(tidut.UT,1) = MTRLBER.KTYPKOD
            SUBSTRING(tidut.UT,14) = MTRLBER.ENR     
            SUBSTRING(tidut.UT,26) = MTRLBER.BENAMNING                                     
            SUBSTRING(tidut.UT,68) = MTRLBER.ENHET
            raknare = raknare + 1.
         END.                    
      END.
      ELSE DO TRANSACTION:
         GET CURRENT qmtrl EXCLUSIVE-LOCK.                      
         ASSIGN
         MTRLBER.BENAMNING = MTRL.BENAMNING
         MTRLBER.PRIS = MTRL.NPRIS         
         MTRLBER.ENHET = MTRL.ENHET.
      END.                                      
   END.
   GET CURRENT qmtrl NO-LOCK.
   GET NEXT qmtrl NO-LOCK.
END.
CLOSE QUERY qmtrl.
RELEASE MTRLBER NO-ERROR.

CREATE tidut.
CREATE tidut.
SUBSTRING(tidut.UT,1) = "KABELSKÅP".
CREATE tidut.
CREATE tidut.

OPEN QUERY skapq FOR EACH BERSKAP WHERE BERSKAP.ENR NE "" AND
BERSKAP.LEVKOD = leverant NO-LOCK. 
GET FIRST skapq NO-LOCK.
DO WHILE AVAILABLE(BERSKAP):   
   
   IF AVAILABLE BERSKAP THEN DO:      
      FIND FIRST MTRL WHERE MTRL.LEVKOD = BERSKAP.LEVKOD AND 
      MTRL.KALKNR = 0 AND MTRL.ENR = BERSKAP.ENR USE-INDEX LEV NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MTRL THEN DO:                                                 
         CREATE tidut.      
         ASSIGN         
         SUBSTRING(tidut.UT,1) = BERSKAP.KTYPKOD + " " + BERSKAP.KOD
         SUBSTRING(tidut.UT,14) = BERSKAP.ENR     
         SUBSTRING(tidut.UT,26) = BERSKAP.BENAMNING                                     
         SUBSTRING(tidut.UT,68) = BERSKAP.ENHET
         raknare = raknare + 1.          
      END.
      ELSE DO TRANSACTION:
         GET CURRENT skapq EXCLUSIVE-LOCK.                      
         ASSIGN
         BERSKAP.BENAMNING = MTRL.BENAMNING
         BERSKAP.PRIS = MTRL.NPRIS         
         BERSKAP.ENHET = MTRL.ENHET.
      END.                                      
   END.
   GET CURRENT skapq NO-LOCK.
   GET NEXT skapq NO-LOCK.
END.
CLOSE QUERY skapq.
RELEASE BERSKAP NO-ERROR.

CREATE tidut.
CREATE tidut.
SUBSTRING(tidut.UT,1) = "STOLPAR".
CREATE tidut.
CREATE tidut.

OPEN QUERY stolpq FOR EACH BERSTOLP WHERE BERSTOLP.STOLPE = TRUE AND
BERSTOLP.LEVKOD = leverant NO-LOCK.
GET FIRST stolpq NO-LOCK.
DO WHILE AVAILABLE(BERSTOLP):   
   IF AVAILABLE BERSTOLP THEN DO:      
      FIND FIRST MTRL WHERE MTRL.LEVKOD = BERSTOLP.LEVKOD AND 
      MTRL.KALKNR = 0 AND MTRL.ENR = BERSTOLP.ENR USE-INDEX LEV NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MTRL THEN DO:                                                 
         CREATE tidut.      
         ASSIGN         
         SUBSTRING(tidut.UT,1) = ""
         SUBSTRING(tidut.UT,14) = BERSTOLP.ENR     
         SUBSTRING(tidut.UT,26) = BERSTOLP.BENAMNING                                     
         SUBSTRING(tidut.UT,68) = BERSTOLP.ENHET
         raknare = raknare + 1.                          
      END.
      ELSE DO TRANSACTION:
         GET CURRENT stolpq EXCLUSIVE-LOCK.                      
         ASSIGN
         BERSTOLP.BENAMNING = MTRL.BENAMNING
         BERSTOLP.PRIS = MTRL.NPRIS         
         BERSTOLP.ENHET = MTRL.ENHET.
      END.                                      
   END.
   GET CURRENT stolpq NO-LOCK.
   GET NEXT stolpq NO-LOCK.
END.
CLOSE QUERY stolpq.
RELEASE BERSTOLP NO-ERROR.

CREATE tidut.
CREATE tidut.
SUBSTRING(tidut.UT,1) = "STATIONER".
CREATE tidut.
CREATE tidut.

OPEN QUERY stolpq2 FOR EACH BERSTOLP WHERE BERSTOLP.STOLPE = FALSE AND
BERSTOLP.LEVKOD = leverant NO-LOCK.   
GET FIRST stolpq2 NO-LOCK.
DO WHILE AVAILABLE(BERSTOLP):   
   
   IF AVAILABLE BERSTOLP THEN DO:      
      FIND FIRST MTRL WHERE MTRL.LEVKOD = BERSTOLP.LEVKOD AND 
      MTRL.KALKNR = 0 AND MTRL.ENR = BERSTOLP.ENR USE-INDEX LEV NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MTRL THEN DO:                                                 
         CREATE tidut.      
         ASSIGN         
         SUBSTRING(tidut.UT,1) = ""
         SUBSTRING(tidut.UT,14) = BERSTOLP.ENR     
         SUBSTRING(tidut.UT,26) = BERSTOLP.BENAMNING                                     
         SUBSTRING(tidut.UT,68) = BERSTOLP.ENHET
         raknare = raknare + 1.                          
      END.
      ELSE DO TRANSACTION:
         GET CURRENT  stolpq2 EXCLUSIVE-LOCK.                      
         ASSIGN
         BERSTOLP.BENAMNING = MTRL.BENAMNING
         BERSTOLP.PRIS = MTRL.NPRIS         
         BERSTOLP.ENHET = MTRL.ENHET.
      END.   
   END.
   GET CURRENT  stolpq2 NO-LOCK.
   GET NEXT stolpq2 NO-LOCK.
END.

CLOSE QUERY stolpq2.
RELEASE BERSTOLP NO-ERROR.

CREATE tidut.
CREATE tidut.
SUBSTRING(tidut.UT,1) = "ERSÄTTNINGSLISTAN".
CREATE tidut.
CREATE tidut.



IF raknare = 0 THEN DO:
   CREATE tidut.
   SUBSTRING(tidut.UT,1) = "ALLA ARTIKLAR FÖR LEVERANTÖR " + LEVERANTOR.LEVNAMN + " KOPPLADE TILL KONSTRUKTIONER FINNS I KATALOGEN".
END.   
IF  
Guru.Konstanter:globforetag = "VAST"  THEN DO:
   OUTPUT TO e:\delad\pro9\guru\koll.txt APPEND.
    /*OUTPUT TO C:\PRO10\GURU\koll.txt APPEND.*/
   PUT "BERKOLL2.P mtrlkoll klart. " leverant  SUBSTRING(Guru.Konstanter:globforetag,1,10)  STRING(TODAY) " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END.                        
ELSE IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
   OUTPUT TO d:\elpool\delad\pro9\wrk\koll.txt APPEND.
   PUT "BERKOLL2.P mtrlkoll klart. " leverant  SUBSTRING(Guru.Konstanter:globforetag,1,10)  STRING(TODAY) " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END.
ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
   OUTPUT TO D:\DELAD\klient\PRO9\koll.txt APPEND.
   PUT "BERKOLL2.P mtrlkoll klart. " leverant  SUBSTRING(Guru.Konstanter:globforetag,1,10)  STRING(TODAY) " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END.
ELSE IF Guru.Konstanter:globforetag = "FORS" THEN DO:
   OUTPUT TO C:\DELAD\PRO9\GURU\koll.txt APPEND.
   PUT "BERKOLL2.P mtrlkoll klart. " leverant  SUBSTRING(Guru.Konstanter:globforetag,1,10)  STRING(TODAY) " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END.

