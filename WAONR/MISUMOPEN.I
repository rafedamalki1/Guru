/*MISUMOPEN.I*/     
IF uppvaltemp.VISGODKANDA = TRUE  THEN DO:
   FIND LAST INTERNFAKTKOLL USE-INDEX VDATUM NO-LOCK NO-ERROR.
   IF AVAILABLE INTERNFAKTKOLL THEN DO:
      uppvaltemp.SLUTDATUM = INTERNFAKTKOLL.VDATUM.
   END.
END.
IF uppvaltemp.VISPERAR = TRUE THEN DO:                  
   /*OLD FÖRUT*/   
END.
ELSE IF uppvaltemp.VISPERAR = FALSE THEN DO:
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
         IF delkoll = TRUE THEN DO:
            OPEN QUERY stq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR AND 
            TIDREGITAB.DATUM >= uppvaltemp.STARTDATUM AND 
            TIDREGITAB.DATUM <= uppvaltemp.SLUTDATUM NO-LOCK.               
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG NE "" AND
               TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR AND 
               TIDREGITAB.DATUM >= uppvaltemp.STARTDATUM AND 
               TIDREGITAB.DATUM <= uppvaltemp.SLUTDATUM NO-LOCK.               
            END.
         END.
         ELSE DO:                  
            OPEN QUERY stq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = valdaao.AONR AND 
            TIDREGITAB.DATUM >= uppvaltemp.STARTDATUM AND 
            TIDREGITAB.DATUM <= uppvaltemp.SLUTDATUM NO-LOCK.               
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG NE "" AND
               TIDREGITAB.AONR = valdaao.AONR AND 
               TIDREGITAB.DATUM >= uppvaltemp.STARTDATUM AND 
               TIDREGITAB.DATUM <= uppvaltemp.SLUTDATUM NO-LOCK.               
            END.
         END.
      END.
      IF openvar = 12 THEN DO:
         IF delkoll = TRUE THEN DO:            
            OPEN QUERY stq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR NO-LOCK.               
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG NE "" AND
               TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR 
               NO-LOCK.               
            END.
         END.
         ELSE DO:                        
            OPEN QUERY stq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR NO-LOCK.               
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG NE "" AND
               TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR NO-LOCK.               
            END.
         END.     
      END.
   END.
   ELSE DO:
      IF openvar = 11 THEN DO:
         IF delkoll = TRUE THEN DO:                   
            OPEN QUERY stq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR AND 
            TIDREGITAB.DATUM >= uppvaltemp.STARTDATUM AND 
            TIDREGITAB.DATUM <= uppvaltemp.SLUTDATUM AND TIDREGITAB.VECKOKORD NE "" NO-LOCK.               
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG NE "" AND
               TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR AND 
               TIDREGITAB.DATUM >= uppvaltemp.STARTDATUM AND 
               TIDREGITAB.DATUM <= uppvaltemp.SLUTDATUM AND TIDREGITAB.VECKOKORD NE "" NO-LOCK.               
            END.
         END.
         ELSE DO:
            OPEN QUERY stq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = valdaao.AONR AND 
            TIDREGITAB.DATUM >= uppvaltemp.STARTDATUM AND 
            TIDREGITAB.DATUM <= uppvaltemp.SLUTDATUM AND TIDREGITAB.VECKOKORD NE "" NO-LOCK.               
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG NE "" AND
               TIDREGITAB.AONR = valdaao.AONR AND 
               TIDREGITAB.DATUM >= uppvaltemp.STARTDATUM AND 
               TIDREGITAB.DATUM <= uppvaltemp.SLUTDATUM AND TIDREGITAB.VECKOKORD NE "" NO-LOCK.               
            END.
         END.
      END.
      IF openvar = 12 THEN DO:
         IF delkoll = TRUE THEN DO:
            OPEN QUERY stq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR AND 
            TIDREGITAB.VECKOKORD NE "" NO-LOCK.               
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG NE "" AND
               TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR AND 
               TIDREGITAB.VECKOKORD NE "" NO-LOCK.               
            END.
         END.
         ELSE DO:                        
            OPEN QUERY stq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = valdaao.AONR AND 
            TIDREGITAB.VECKOKORD NE "" NO-LOCK.               
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG NE "" AND
               TIDREGITAB.AONR = valdaao.AONR AND 
               TIDREGITAB.VECKOKORD NE "" NO-LOCK.               
            END.
         END.     
      END.      
   END.   
END PROCEDURE.

         
