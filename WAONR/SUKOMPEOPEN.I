/*SUKOMPEOPEN.I*/     


IF uppvaltemp.VISPERAR = FALSE THEN DO:
   
   RUN sop_UI (INPUT 11,INPUT uppvaltemp.DELNRKOLL,INPUT uppvaltemp.VISGODKANDA).
   RUN skapadagdag_UI.    
      
END.
ELSE DO:                     
   RUN sop_UI (INPUT 12,INPUT uppvaltemp.DELNRKOLL,INPUT uppvaltemp.VISGODKANDA).         
   RUN skapadagdag_UI.
   
END.
PROCEDURE sop_UI:
   DEFINE INPUT PARAMETER openvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER delkoll AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER visgvar AS LOGICAL NO-UNDO.
   IF visgvar = FALSE THEN DO:                  
      IF openvar = 11 THEN DO:                  
         OPEN QUERY stq FOR EACH TIDREGITAB WHERE 
         TIDREGITAB.PERSONALKOD = valperstemp.PERSONALKOD AND 
         TIDREGITAB.DATUM >= uppvaltemp.STARTDATUM AND 
         TIDREGITAB.DATUM <= uppvaltemp.SLUTDATUM AND TIDREGITAB.TIDLOG = TRUE
         AND (TIDREGITAB.AONR = "250" OR TIDREGITAB.AONR = "255")  NO-LOCK.                  
      
      END.      
      IF openvar = 12 THEN DO:                  
         OPEN QUERY stq FOR EACH TIDREGITAB WHERE 
         TIDREGITAB.PERSONALKOD = valperstemp.PERSONALKOD AND TIDREGITAB.TIDLOG = TRUE AND (TIDREGITAB.AONR = "250" OR TIDREGITAB.AONR = "255")  NO-LOCK.                     
      END.     
   END.
   ELSE DO:      
      IF openvar = 11 THEN DO:         
         OPEN QUERY stq FOR EACH TIDREGITAB WHERE 
         TIDREGITAB.PERSONALKOD = valperstemp.PERSONALKOD  AND 
         TIDREGITAB.DATUM >= uppvaltemp.STARTDATUM AND 
         TIDREGITAB.DATUM <= uppvaltemp.SLUTDATUM AND TIDREGITAB.VECKOKORD NE "" AND TIDREGITAB.TIDLOG = TRUE AND (TIDREGITAB.AONR = "250" OR TIDREGITAB.AONR = "255") 
         NO-LOCK.                              
      END.
      
      IF openvar = 12 THEN DO:         
         OPEN QUERY stq FOR EACH TIDREGITAB WHERE 
         TIDREGITAB.PERSONALKOD = valperstemp.PERSONALKOD AND TIDREGITAB.VECKOKORD NE "" AND TIDREGITAB.TIDLOG = TRUE AND (TIDREGITAB.AONR = "250" OR TIDREGITAB.AONR = "255") 
         NO-LOCK.            
         
      END.           
   END.
END PROCEDURE.

         
