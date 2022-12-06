/*APANDFBO.P*/
 {ANDGODUT.I}

FOR EACH appmarkpers NO-LOCK:   
   OPEN QUERY tidq FOR EACH 
   TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = appmarkpers.PERSONALKOD AND
   YEAR(TIDREGITAB.DATUM) = regar AND MONTH(TIDREGITAB.DATUM) = regmnr AND
   TIDREGITAB.GODKAND = "F"   
   NO-LOCK.   
   
   GET FIRST tidq NO-LOCK.
   IF NOT AVAILABLE TIDREGITAB THEN DO:
      IF gvisatidpermanad = TRUE THEN RUN god_UI.
      NEXT.
   END.
   ELSE RUN tidgodk_UI.   
END.
PROCEDURE god_UI:   
   DO TRANSACTION:
      FIND FIRST GODKOLL WHERE 
      GODKOLL.PERSONALKOD = appmarkpers.PERSONALKOD AND               
      GODKOLL.DATAR = regar AND GODKOLL.DATMAN = regmnr 
      USE-INDEX PKODAR EXCLUSIVE-LOCK NO-ERROR.          
      IF AVAILABLE GODKOLL THEN DO:
         FIND LAST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = appmarkpers.PERSONALKOD AND
         YEAR(TIDREGITAB.DATUM) = regar AND MONTH(TIDREGITAB.DATUM) = regmnr AND
         TIDREGITAB.GODKAND NE ""
         USE-INDEX PSTART NO-LOCK NO-ERROR.          
         IF AVAILABLE TIDREGITAB THEN DO:
            ASSIGN 
            GODKOLL.DATUM = TIDREGITAB.DATUM
            GODKOLL.KLAR = FALSE.
         END.
         ELSE DO:
            DELETE GODKOLL.
         END.       
      END.      
   END.
END PROCEDURE.

PROCEDURE tidgodk_UI:  
   GET FIRST tidq NO-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB):
      DO TRANSACTION:
         GET CURRENT tidq EXCLUSIVE-LOCK.
         ASSIGN TIDREGITAB.GODKAND = "".                        
      END.
      GET NEXT tidq NO-LOCK.
   END.              
   CLOSE QUERY tidq.           
   IF gvisatidpermanad = TRUE THEN DO:                        
      RUN god_UI.            
   END.
END PROCEDURE.
