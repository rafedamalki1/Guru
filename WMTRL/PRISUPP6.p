/*PRISUPP6.p UPPDATERAR PRISER FÖR SATSREGISTRET FÖR VALD LEVERANTÖR*/

DEFINE INPUT PARAMETER leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.      
DEFINE INPUT PARAMETER globforetag LIKE FORETAG.FORETAG NO-UNDO.
   
   /*MATERIELSATSER*/
 {AMERICANEUROPEAN.I}  
   OPEN QUERY qmtrl5 FOR EACH SATS WHERE SATS.LEVKOD = leverant NO-LOCK.
   GET FIRST qmtrl5 NO-LOCK.
   DO WHILE AVAILABLE(SATS):      
      IF AVAILABLE SATS THEN DO TRANSACTION:
         GET CURRENT qmtrl5 EXCLUSIVE-LOCK.
         IF SATS.SATS = TRUE THEN DO:            
            FIND FIRST MTRL WHERE MTRL.LEVKOD = leverant AND 
            MTRL.KALKNR = 0 AND MTRL.ENR = SATS.ENR USE-INDEX LEV NO-LOCK NO-ERROR.           
            IF AVAILABLE MTRL THEN DO:                           
               ASSIGN 
               SATS.BENAMNING = MTRL.BENAMNING
               SATS.ENHET = MTRL.ENHET
               SATS.PRIS = MTRL.NPRIS.
            END.                                      
         END.
         ELSE DO:            
            FIND FIRST MTRL WHERE MTRL.LEVKOD = leverant AND 
            MTRL.KALKNR = 0 AND MTRL.ENR = SATS.ENR2 USE-INDEX LEV NO-LOCK NO-ERROR.            
            IF AVAILABLE MTRL THEN DO:                           
               ASSIGN         
               SATS.BENAMNING2 = MTRL.BENAMNING
               SATS.ENHET2 = MTRL.ENHET
               SATS.PRIS2 = MTRL.NPRIS.
            END.
         END.   
      END.
      GET CURRENT qmtrl5 NO-LOCK.
      GET NEXT qmtrl5 NO-LOCK.   
   END.
   CLOSE QUERY qmtrl5.   
   RELEASE SATS NO-ERROR.
   IF  
   globforetag = "VAST"  THEN DO:
     /*OUTPUT TO C:\PRO10\GURU\koll.txt APPEND.*/
      OUTPUT TO e:\delad\pro9\guru\koll.txt APPEND.
      PUT "PRISUPP6.P Koll av sats klart. " leverant  SUBSTRING(globforetag,1,10)  STRING(TODAY) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF globforetag = "FORS" THEN DO:
      OUTPUT TO C:\DELAD\PRO9\GURU\koll.txt APPEND.
      PUT "PRISUPP6.P Koll av sats klart. " leverant  SUBSTRING(globforetag,1,10)  STRING(TODAY) " " STRING(TIME,"HH:MM:SS") SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF globforetag = "GRAN"  THEN DO:
      OUTPUT TO d:\elpool\delad\pro9\wrk\koll.txt APPEND.
      PUT "PRISUPP6.P Koll av sats klart. " leverant  SUBSTRING(globforetag,1,10)  STRING(TODAY) " " STRING(TIME,"HH:MM:SS") SKIP.
      OUTPUT CLOSE.
   END.   
   ELSE IF globforetag = "GKAL" THEN DO:
      OUTPUT TO D:\DELAD\klient\PRO9\koll.txt APPEND.
      PUT "PRISUPP6.P Koll av sats klart. " leverant  SUBSTRING(globforetag,1,10)  STRING(TODAY) " " STRING(TIME,"HH:MM:SS") SKIP.
      OUTPUT CLOSE.
   END.   
   ELSE IF globforetag = "STRA" THEN DO:
      OUTPUT TO E:\DELAD\PRO9\koll.txt APPEND.
      PUT "PRISUPP6.P Koll av sats klart. " leverant  SUBSTRING(globforetag,1,10)  STRING(TODAY) " " STRING(TIME,"HH:MM:SS") SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF globforetag = "BORL" THEN DO:
      OUTPUT TO D:\GURU\PRO9\koll.txt APPEND.
      PUT "PRISUPP6.P Koll av sats klart. " leverant  SUBSTRING(globforetag,1,10)  STRING(TODAY) " " STRING(TIME,"HH:MM:SS") SKIP.
      OUTPUT CLOSE.
   END.
 {EUROPEANAMERICAN.I}    
     
