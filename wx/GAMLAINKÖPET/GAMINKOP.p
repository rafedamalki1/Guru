/*GAMINKOP.P. KÖRS FRÅN BERINKOP.W*/
{BESTMTRL.I}
DEFINE INPUT PARAMETER valaonr LIKE AONRTAB.AONR NO-UNDO.
DEFINE INPUT PARAMETER valomrade LIKE AONRTAB.OMRADE NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR best_mtrl.   

   OPEN QUERY mtrlq FOR EACH BERMTRL WHERE BERMTRL.AONR = valaonr AND 
   BERMTRL.OMRADE = valomrade AND BERMTRL.INKOP = TRUE USE-INDEX INKOP NO-LOCK.
   GET FIRST mtrlq NO-LOCK.
   DO WHILE AVAILABLE(BERMTRL):
      IF BERMTRL.OFFERT = FALSE THEN DO:
         CREATE best_mtrl.
         ASSIGN
         best_mtrl.ENR = BERMTRL.ENR                        
         best_mtrl.BENAMNING = BERMTRL.BENAMNING            
         best_mtrl.ENHET = BERMTRL.ENHET                    
         best_mtrl.PRIS = BERMTRL.PRIS                      
         best_mtrl.OPRIS = BERMTRL.OPRIS                    
         best_mtrl.ANTAL = BERMTRL.ANTAL                    
         best_mtrl.LEVKOD = BERMTRL.LEVKOD                  
         best_mtrl.BERLEV = BERMTRL.BERLEV                  
         best_mtrl.DBEST = BERMTRL.DBEST                    
         best_mtrl.DATUM = BERMTRL.DATUM   
         best_mtrl.DELNR = BERMTRL.DELNR                      
         best_mtrl.KLAR = BERMTRL.KLAR.                     
      END.                                                  
      GET NEXT mtrlq NO-LOCK.                               
   END.                                                     
   CLOSE QUERY mtrlq.                                       
   
