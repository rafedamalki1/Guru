/*APPINKOP.P. KÖRS FRÅN BERINKOP.W*/
DEFINE VARIABLE aonrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE datvar AS DATE NO-UNDO.
{BESTMTRL.I}

DEFINE TEMP-TABLE mtrl_temp    
    FIELD ENR LIKE BERMTRL.ENR
    FIELD BENAMNING LIKE BERMTRL.BENAMNING
    FIELD ENHET LIKE BERMTRL.ENHET
    FIELD PRIS LIKE BERMTRL.PRIS       
    FIELD ANTAL LIKE BERMTRL.ANTAL     
    FIELD BESTANT LIKE BERMTRL.BESTANT    
    FIELD LEVKOD LIKE BERMTRL.LEVKOD 
    FIELD BERLEV LIKE BERMTRL.BERLEV
    FIELD DATUM LIKE BERMTRL.DATUM    
    INDEX ENR IS PRIMARY ENR ASCENDING.

DEFINE TEMP-TABLE lin_temp    
    FIELD ENR LIKE BERLINKAB.ENR
    FIELD BENAMNING LIKE BERLINKAB.BENAMNING
    FIELD ENHET LIKE BERLINKAB.ENHET
    FIELD PRIS LIKE BERLINKAB.PRIS       
    FIELD METER LIKE BERLINKAB.METER
    FIELD TOTMETER LIKE BERLINKAB.TOTMETER
    FIELD LEDARE LIKE BERLINKAB.LEDARE
    FIELD UPPLAG LIKE BERLINKAB.UPPLAG             
    FIELD LEVKOD LIKE BERLINKAB.LEVKOD     
    FIELD DATUM LIKE BERLINKAB.DATUM    
    INDEX ENR IS PRIMARY ENR ASCENDING.   

DEFINE BUFFER linbuff FOR lin_temp.

DEFINE INPUT PARAMETER valaonr LIKE AONRTAB.AONR NO-UNDO.
DEFINE INPUT PARAMETER valomrade LIKE AONRTAB.OMRADE NO-UNDO.
DEFINE INPUT PARAMETER globforetag AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR best_mtrl.

   FOR EACH mtrl_temp:
      DELETE mtrl_temp.
   END.
   FOR EACH lin_temp:
      DELETE lin_temp.
   END.
   IF globforetag = "UMEA" THEN DO:
      FIND FIRST BEREDNING WHERE BEREDNING.BERAONR = valaonr AND BEREDNING.OMRADE = valomrade
      NO-LOCK NO-ERROR.
      FIND LAST BERMTRL WHERE BERMTRL.AONR = valaonr AND 
      BERMTRL.OMRADE = valomrade AND BERMTRL.INKOP = FALSE 
      USE-INDEX DATUM NO-LOCK NO-ERROR.
      IF AVAILABLE BERMTRL THEN DO:
         datvar = BERMTRL.DATUM.
      END.
      aonrvar = BEREDNING.AONR.
      FOR EACH BEREDNING WHERE BEREDNING.AONR = aonrvar NO-LOCK: 
         FIND LAST BERMTRL WHERE BERMTRL.AONR = BEREDNING.BERAONR AND 
         BERMTRL.OMRADE = BEREDNING.OMRADE AND BERMTRL.INKOP = FALSE 
         USE-INDEX DATUM NO-LOCK NO-ERROR.
         IF AVAILABLE BERMTRL THEN DO:
            IF BERMTRL.DATUM > datvar THEN datvar = BERMTRL.DATUM.
         END.
      END.
      FOR EACH BEREDNING WHERE BEREDNING.AONR = aonrvar NO-LOCK: 
         valaonr = BEREDNING.BERAONR.
         RUN bermtrl_UI.
         RUN kskydd_UI.
         RUN berlinkab_UI.
      END.
      RUN summa_UI.
      RUN sumlin_UI.
   END.
   ELSE DO:   
      FIND LAST BERMTRL WHERE BERMTRL.AONR = valaonr AND 
      BERMTRL.OMRADE = valomrade AND BERMTRL.INKOP = FALSE 
      USE-INDEX DATUM NO-LOCK NO-ERROR.
      IF AVAILABLE BERMTRL THEN DO:
         datvar = BERMTRL.DATUM.
      END.
      RUN bermtrl_UI.
      RUN kskydd_UI.
      RUN summa_UI.
      RUN berlinkab_UI.
      RUN sumlin_UI.
   END.   

PROCEDURE mtrl_UI :
   CREATE mtrl_temp.
   ASSIGN
   mtrl_temp.ENR = BERMTRL.ENR
   mtrl_temp.BENAMNING = BERMTRL.BENAMNING
   mtrl_temp.ENHET = BERMTRL.ENHET         
   mtrl_temp.PRIS = BERMTRL.PRIS
   mtrl_temp.ANTAL = BERMTRL.ANTAL     
   mtrl_temp.LEVKOD = BERMTRL.LEVKOD
   mtrl_temp.DATUM = datvar.
   IF globforetag = "ELPA" {GLOBVES.I} THEN DO:
      IF BERMTRL.LEVKOD = "12" OR BERMTRL.LEVKOD = "13"  THEN DO: 
         mtrl_temp.LEVKOD = "16".                  
      END.
   END.
   ELSE IF globforetag = "UMEA" THEN DO:
      mtrl_temp.LEVKOD = "1".
   END.
END PROCEDURE.

PROCEDURE skydd_UI :
   CREATE mtrl_temp.
   ASSIGN
   mtrl_temp.ENR = KSKYDD.ENR
   mtrl_temp.BENAMNING = KSKYDD.BENAMNING
   mtrl_temp.ENHET = KSKYDD.ENHET         
   mtrl_temp.PRIS = KSKYDD.PRIS
   mtrl_temp.ANTAL = KSKYDD.ANTAL * KSKYDD.METER    
   mtrl_temp.LEVKOD = KSKYDD.LEVKOD
   mtrl_temp.DATUM = datvar.
   IF globforetag = "ELPA" {GLOBVES.I} THEN DO:
      IF KSKYDD.LEVKOD = "12" OR KSKYDD.LEVKOD = "13"  THEN mtrl_temp.LEVKOD = "16".
   END.
   ELSE IF globforetag = "UMEA" THEN DO:
      mtrl_temp.LEVKOD = "1".
   END.
END PROCEDURE.

PROCEDURE summa_UI : 
   FOR EACH mtrl_temp BREAK BY mtrl_temp.LEVKOD BY mtrl_temp.ENR:      
   ACCUMULATE mtrl_temp.ANTAL (TOTAL BY mtrl_temp.LEVKOD BY mtrl_temp.ENR).       
      IF LAST-OF(mtrl_temp.ENR) THEN DO TRANSACTION:
         CREATE best_mtrl.
         ASSIGN                                 
         best_mtrl.ENR = mtrl_temp.ENR
         best_mtrl.BENAMNING = mtrl_temp.BENAMNING 
         best_mtrl.ENHET = mtrl_temp.ENHET   
         best_mtrl.LEVKOD = mtrl_temp.LEVKOD   
         best_mtrl.PRIS = mtrl_temp.PRIS  
         best_mtrl.OPRIS = mtrl_temp.PRIS
         best_mtrl.DATUM = mtrl_temp.DATUM                         
         best_mtrl.ANTAL = (ACCUM TOTAL BY mtrl_temp.ENR mtrl_temp.ANTAL).         
      END.     
   END.
END PROCEDURE.

PROCEDURE linor_UI :
   CREATE lin_temp.
   ASSIGN
   lin_temp.ENR = BERLINKAB.ENR
   lin_temp.BENAMNING = BERLINKAB.BENAMNING
   lin_temp.ENHET = BERLINKAB.ENHET         
   lin_temp.PRIS = BERLINKAB.PRIS
   lin_temp.METER = BERLINKAB.METER  
   lin_temp.LEDARE = BERLINKAB.LEDARE
   lin_temp.TOTMETER = BERLINKAB.TOTMETER     
   lin_temp.LEVKOD = BERLINKAB.LEVKOD
   lin_temp.UPPLAG = BERLINKAB.UPPLAG
   lin_temp.DATUM = datvar. 
   IF globforetag = "ELPA" {GLOBVES.I} THEN DO:
      IF BERLINKAB.LEVKOD = "12" OR BERLINKAB.LEVKOD = "13"  THEN lin_temp.LEVKOD = "16".
   END.
   ELSE IF globforetag = "UMEA" THEN DO:
      lin_temp.LEVKOD = "1".
   END.
END PROCEDURE.

PROCEDURE sumlin_UI :
   FOR EACH lin_temp WHERE lin_temp.TOTMETER > 0 AND
   lin_temp.UPPLAG = ?:
      DELETE lin_temp.
   END.
   FOR EACH lin_temp WHERE lin_temp.UPPLAG NE ? AND lin_temp.TOTMETER > 0:
      FOR EACH linbuff WHERE linbuff.ENR = lin_temp.ENR AND 
      linbuff.LEVKOD = lin_temp.LEVKOD AND linbuff.TOTMETER = 0:
         DELETE linbuff.
      END.
   END. 
   FOR EACH lin_temp WHERE lin_temp.UPPLAG = ?:
      lin_temp.TOTMETER = lin_temp.METER * lin_temp.LEDARE.
   END.                 
   FOR EACH lin_temp BREAK BY lin_temp.LEVKOD BY lin_temp.ENR:      
   ACCUMULATE lin_temp.TOTMETER (TOTAL BY lin_temp.LEVKOD BY lin_temp.ENR).       
      IF LAST-OF(lin_temp.ENR) THEN DO TRANSACTION:
         CREATE best_mtrl.
         ASSIGN                                 
         best_mtrl.ENR = lin_temp.ENR
         best_mtrl.BENAMNING = lin_temp.BENAMNING 
         best_mtrl.ENHET = lin_temp.ENHET   
         best_mtrl.PRIS = lin_temp.PRIS
         best_mtrl.LEVKOD = lin_temp.LEVKOD   
         best_mtrl.PRIS = lin_temp.PRIS      
         best_mtrl.OPRIS = lin_temp.PRIS
         best_mtrl.DATUM = lin_temp.DATUM                         
         best_mtrl.ANTAL = (ACCUM TOTAL BY lin_temp.ENR lin_temp.TOTMETER).         
      END.     
   END.
END PROCEDURE.
PROCEDURE bermtrl_UI :
   OPEN QUERY mtrlq FOR EACH BERMTRL WHERE BERMTRL.AONR = valaonr AND 
   BERMTRL.OMRADE = valomrade USE-INDEX OMR NO-LOCK.
   GET FIRST mtrlq NO-LOCK.
   DO WHILE AVAILABLE(BERMTRL):
      IF BERMTRL.ANTAL > 0 THEN DO:
         RUN mtrl_UI.
      END.      
      GET NEXT mtrlq NO-LOCK.     
   END.      
   CLOSE QUERY mtrlq.
END PROCEDURE.

PROCEDURE kskydd_UI :
   OPEN QUERY skyddq FOR EACH KSKYDD WHERE KSKYDD.AONR = valaonr AND 
   KSKYDD.OMRADE = valomrade AND KSKYDD.BERED = TRUE USE-INDEX OMR NO-LOCK.
   GET FIRST skyddq NO-LOCK.
   DO WHILE AVAILABLE(KSKYDD):      
      RUN skydd_UI.            
      GET NEXT skyddq NO-LOCK.     
   END.      
   CLOSE QUERY skyddq.
END PROCEDURE.

PROCEDURE berlinkab_UI :
   /*Linor/kablar*/
   OPEN QUERY linq FOR EACH BERLINKAB WHERE BERLINKAB.AONR = valaonr AND 
   BERLINKAB.OMRADE = valomrade AND BERLINKAB.KORTKOD = ? USE-INDEX OMR NO-LOCK.
   GET FIRST linq NO-LOCK.
   DO WHILE AVAILABLE(BERLINKAB):      
      RUN linor_UI.      
      GET NEXT linq NO-LOCK.     
   END.      
   CLOSE QUERY linq.
END PROCEDURE.
