/*OKINKOP.P.*/   
DEFINE VARIABLE aonrvar AS CHARACTER NO-UNDO.
{BESTMTRL.I}
DEFINE INPUT PARAMETER valaonr LIKE AONRTAB.AONR NO-UNDO.
DEFINE INPUT PARAMETER valomrade LIKE AONRTAB.OMRADE NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR best_mtrl.
DEFINE INPUT PARAMETER gammal AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER bestvar AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER globforetag AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER klockansek AS INTEGER NO-UNDO.
/* fler best per dag*/

   IF gammal = TRUE THEN DO:
      IF globforetag = "UMEA" THEN DO:
         FIND FIRST BEREDNING WHERE BEREDNING.BERAONR = valaonr AND BEREDNING.OMRADE = valomrade
         NO-LOCK NO-ERROR.
         aonrvar = BEREDNING.AONR.
         FOR EACH BEREDNING WHERE BEREDNING.AONR = aonrvar NO-LOCK: 
            valaonr = BEREDNING.BERAONR.
            OPEN QUERY mtrlq FOR EACH BERMTRL WHERE BERMTRL.AONR = valaonr AND 
            BERMTRL.OMRADE = valomrade AND BERMTRL.INKOP = TRUE 
            AND BERMTRL.KLAR = FALSE USE-INDEX INKOP NO-LOCK.
            GET FIRST mtrlq EXCLUSIVE-LOCK.
            DO WHILE AVAILABLE(BERMTRL):
               DELETE BERMTRL.       
               GET NEXT mtrlq EXCLUSIVE-LOCK.
            END.      
            CLOSE QUERY mtrlq. 
            RUN skapa_UI.
         END.
      END.
      ELSE DO:      
         OPEN QUERY mtrlq FOR EACH BERMTRL WHERE BERMTRL.AONR = valaonr AND 
         BERMTRL.OMRADE = valomrade AND BERMTRL.INKOP = TRUE 
         AND BERMTRL.KLAR = FALSE USE-INDEX INKOP NO-LOCK.
         GET FIRST mtrlq EXCLUSIVE-LOCK.
         DO WHILE AVAILABLE(BERMTRL):
            DELETE BERMTRL.       
            GET NEXT mtrlq EXCLUSIVE-LOCK.
         END.      
         CLOSE QUERY mtrlq.          
         RUN skapa_UI.
      END.      
   END.
   ELSE DO:
      IF globforetag = "UMEA" THEN DO:
         FIND FIRST BEREDNING WHERE BEREDNING.BERAONR = valaonr AND BEREDNING.OMRADE = valomrade
         NO-LOCK NO-ERROR.
         aonrvar = BEREDNING.AONR.
         FOR EACH BEREDNING WHERE BEREDNING.AONR = aonrvar NO-LOCK: 
            valaonr = BEREDNING.BERAONR.            
            RUN skapa_UI.
         END.
      END.
      ELSE DO: 
         RUN skapa_UI.
      END.
   END.

PROCEDURE skapa_UI :
   FOR EACH best_mtrl WHERE best_mtrl.KLAR = TRUE:
      DELETE best_mtrl.
   END.   
   FOR EACH best_mtrl:
      DO TRANSACTION:
         CREATE BERMTRL.
         ASSIGN 
         BERMTRL.AONR = valaonr         
         BERMTRL.OMRADE = valomrade
         BERMTRL.ENR = best_mtrl.ENR
         BERMTRL.BENAMNING = best_mtrl.BENAMNING
         BERMTRL.ENHET = best_mtrl.ENHET         
         BERMTRL.PRIS = best_mtrl.PRIS
         BERMTRL.OPRIS = best_mtrl.OPRIS
         BERMTRL.ANTAL = best_mtrl.ANTAL           
         BERMTRL.LEVKOD = best_mtrl.LEVKOD
         BERMTRL.BERLEV = best_mtrl.BERLEV
         BERMTRL.DBEST = best_mtrl.DBEST
          /* fler best per dag*/
         BERMTRL.DELNR = klockansek
         BERMTRL.DATUM = best_mtrl.DATUM
         BERMTRL.INKOP = TRUE.
         IF bestvar = TRUE THEN BERMTRL.KLAR = TRUE.
      END.
   END.   
END PROCEDURE.
