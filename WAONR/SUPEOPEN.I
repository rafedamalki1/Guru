/*SUPEOPEN.I*/     

IF uppvaltemp.VISPERAR = TRUE THEN DO:                  
   RUN sop_UI (INPUT 1,INPUT uppvaltemp.DELNRKOLL,INPUT uppvaltemp.VISGODKANDA).        
   RUN skapadag_UI.

END.
ELSE IF uppvaltemp.VISPERAR = FALSE THEN DO:
   IF uppvaltemp.ENDBEST = FALSE THEN DO:   
      RUN sop_UI (INPUT 11,INPUT uppvaltemp.DELNRKOLL,INPUT uppvaltemp.VISGODKANDA).
      RUN skapadagdag_UI.    
   END.
   ELSE DO:
      RUN sop_UI (INPUT 31,INPUT uppvaltemp.DELNRKOLL,INPUT uppvaltemp.VISGODKANDA).
      RUN skapadagdag_UI.    
   END.
   
END.
ELSE DO:                  
   IF uppvaltemp.ENDBEST = FALSE THEN DO:   
      RUN sop_UI (INPUT 12,INPUT uppvaltemp.DELNRKOLL,INPUT uppvaltemp.VISGODKANDA).         
      RUN skapadagdag_UI.
   END.
   ELSE DO:
      RUN sop_UI (INPUT 32,INPUT uppvaltemp.DELNRKOLL,INPUT uppvaltemp.VISGODKANDA).         
      RUN skapadagdag_UI.
   END.
END.
PROCEDURE sop_UI:
   DEFINE INPUT PARAMETER openvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER delkoll AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER visgvar AS LOGICAL NO-UNDO.
   IF visgvar = FALSE THEN DO:      
      IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
         OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = valperstemp.PERSONALKOD AND
         TIDREGITAB.LONTILLAGG = "ATL" AND            
         TIDREGITAB.DATUM >= uppvaltemp.STARTDATUM AND 
         TIDREGITAB.DATUM <= uppvaltemp.SLUTDATUM NO-LOCK.                  
      END.            
      IF openvar = 11 THEN DO:                  
         OPEN QUERY stq FOR EACH SUMTIDDAG WHERE 
         SUMTIDDAG.PERSONALKOD = valperstemp.PERSONALKOD AND 
         SUMTIDDAG.DATUM >= uppvaltemp.STARTDATUM AND 
         SUMTIDDAG.DATUM <= uppvaltemp.SLUTDATUM NO-LOCK.                  
      
      END.
      IF openvar = 31 THEN DO:                  
         OPEN QUERY stq FOR EACH SUMTIDDAG WHERE 
         SUMTIDDAG.PERSONALKOD = valperstemp.PERSONALKOD AND 
         SUMTIDDAG.AONR = "160" AND 
         SUMTIDDAG.DATUM >= uppvaltemp.STARTDATUM AND 
         SUMTIDDAG.DATUM <= uppvaltemp.SLUTDATUM NO-LOCK.                  
      
      END.
      IF openvar = 12 THEN DO:                  
         OPEN QUERY stq FOR EACH SUMTIDDAG WHERE 
         SUMTIDDAG.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK.                     
      END.
      IF openvar = 32 THEN DO:                  
         OPEN QUERY stq FOR EACH SUMTIDDAG WHERE 
         SUMTIDDAG.PERSONALKOD = valperstemp.PERSONALKOD
         AND SUMTIDDAG.AONR = "160"  NO-LOCK.                     
      END.
   END.
   ELSE DO:
      IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
         OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = valperstemp.PERSONALKOD AND
         TIDREGITAB.LONTILLAGG = "ATL" AND            
         TIDREGITAB.DATUM >= uppvaltemp.STARTDATUM AND 
         TIDREGITAB.DATUM <= uppvaltemp.SLUTDATUM AND TIDREGITAB.VECKOKORD NE "" NO-LOCK.                  
      END.            
      IF openvar = 11 THEN DO:         
         OPEN QUERY stq FOR EACH SUMTIDDAG WHERE 
         SUMTIDDAG.PERSONALKOD = valperstemp.PERSONALKOD  AND 
         SUMTIDDAG.DATUM >= uppvaltemp.STARTDATUM AND 
         SUMTIDDAG.DATUM <= uppvaltemp.SLUTDATUM AND SUMTIDDAG.VECKOKORD NE ""
         NO-LOCK.                              
      END.
      IF openvar = 31 THEN DO:         
         OPEN QUERY stq FOR EACH SUMTIDDAG WHERE 
         SUMTIDDAG.PERSONALKOD = valperstemp.PERSONALKOD  AND 
         SUMTIDDAG.AONR = "160" AND 
         SUMTIDDAG.DATUM >= uppvaltemp.STARTDATUM AND 
         SUMTIDDAG.DATUM <= uppvaltemp.SLUTDATUM AND SUMTIDDAG.VECKOKORD NE ""
         NO-LOCK.                              
      END.
      IF openvar = 12 THEN DO:         
         OPEN QUERY stq FOR EACH SUMTIDDAG WHERE 
         SUMTIDDAG.PERSONALKOD = valperstemp.PERSONALKOD AND SUMTIDDAG.VECKOKORD NE ""
         NO-LOCK.            
         
      END.      
      IF openvar = 32 THEN DO:         
         OPEN QUERY stq FOR EACH SUMTIDDAG WHERE 
         SUMTIDDAG.AONR = "160" AND 
         SUMTIDDAG.PERSONALKOD = valperstemp.PERSONALKOD AND SUMTIDDAG.VECKOKORD NE ""
         NO-LOCK.            
         
      END.      
   END.
END PROCEDURE.

         
