/*SUMOPEN.I*/     
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
   RUN sop_UI (INPUT 21,INPUT uppvaltemp.DELNRKOLL,INPUT uppvaltemp.VISGODKANDA).

   RUN kostreg_UI.
END.
ELSE DO:                  
   RUN sop_UI (INPUT 12,INPUT uppvaltemp.DELNRKOLL,INPUT uppvaltemp.VISGODKANDA).         
   RUN skapadagdag_UI.
   RUN sop_UI (INPUT 22,INPUT uppvaltemp.DELNRKOLL,INPUT uppvaltemp.VISGODKANDA).
   RUN kostreg_UI.
END.

PROCEDURE sop_UI:
   DEFINE INPUT PARAMETER openvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER delkoll AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER visgvar AS LOGICAL NO-UNDO.
   IF visgvar = FALSE THEN DO:      
      IF openvar = 1 THEN DO:
         IF delkoll = TRUE THEN DO:
            OPEN QUERY sq FOR EACH SUMTID WHERE 
            SUMTID.AONR = valdaao.AONR AND SUMTID.DELNR = valdaao.DELNR AND 
            SUMTID.DATUM = uppvaltemp.STARTDATUM NO-LOCK.               
            IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG BEGINS "S" AND
               TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR AND 
               YEAR(TIDREGITAB.DATUM) = YEAR(uppvaltemp.STARTDATUM) NO-LOCK.               
            END.
            IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG NE "" AND
               TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR AND 
               YEAR(TIDREGITAB.DATUM) = YEAR(uppvaltemp.STARTDATUM) NO-LOCK.               
            END.
         END.
         ELSE DO:
            OPEN QUERY sq FOR EACH SUMTID WHERE 
            SUMTID.AONR = valdaao.AONR AND  
            SUMTID.DATUM = uppvaltemp.STARTDATUM NO-LOCK.               
            IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG BEGINS "S" AND
               TIDREGITAB.AONR = valdaao.AONR AND 
               YEAR(TIDREGITAB.DATUM) = YEAR(uppvaltemp.STARTDATUM) NO-LOCK.               
            END.
            IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG NE "" AND
               TIDREGITAB.AONR = valdaao.AONR AND 
               YEAR(TIDREGITAB.DATUM) = YEAR(uppvaltemp.STARTDATUM) NO-LOCK.               
            END.
         END.     
      END.
      IF openvar = 11 THEN DO:
         IF delkoll = TRUE THEN DO:
            OPEN QUERY stq FOR EACH SUMTIDDAG WHERE 
            SUMTIDDAG.AONR = valdaao.AONR AND SUMTIDDAG.DELNR = valdaao.DELNR AND 
            SUMTIDDAG.DATUM >= uppvaltemp.STARTDATUM AND 
            SUMTIDDAG.DATUM <= uppvaltemp.SLUTDATUM NO-LOCK.         
            IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG BEGINS "S" AND
               TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR AND 
               TIDREGITAB.DATUM >= uppvaltemp.STARTDATUM AND 
               TIDREGITAB.DATUM <= uppvaltemp.SLUTDATUM NO-LOCK.               
            END.
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG NE "" AND
               TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR AND 
               TIDREGITAB.DATUM >= uppvaltemp.STARTDATUM AND 
               TIDREGITAB.DATUM <= uppvaltemp.SLUTDATUM NO-LOCK.               
            END.
         END.
         ELSE DO:
            OPEN QUERY stq FOR EACH SUMTIDDAG WHERE 
            SUMTIDDAG.AONR = valdaao.AONR AND  
            SUMTIDDAG.DATUM >= uppvaltemp.STARTDATUM AND 
            SUMTIDDAG.DATUM <= uppvaltemp.SLUTDATUM NO-LOCK.         
            IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG BEGINS "S" AND
               TIDREGITAB.AONR = valdaao.AONR AND 
               TIDREGITAB.DATUM >= uppvaltemp.STARTDATUM AND 
               TIDREGITAB.DATUM <= uppvaltemp.SLUTDATUM NO-LOCK.               
            END.
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
            OPEN QUERY stq FOR EACH SUMTIDDAG WHERE 
            SUMTIDDAG.AONR = valdaao.AONR AND SUMTIDDAG.DELNR = valdaao.DELNR 
            NO-LOCK.
            IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG BEGINS "S" AND
               TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR 
               NO-LOCK.               
            END.
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG NE "" AND
               TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR 
               NO-LOCK.               
            END.
         END.
         ELSE DO:          
            OPEN QUERY stq FOR EACH SUMTIDDAG WHERE 
            SUMTIDDAG.AONR = valdaao.AONR NO-LOCK.
            IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG BEGINS "S" AND
               TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR NO-LOCK.               
            END.
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG NE "" AND
               TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR NO-LOCK.               
            END.
         END.     
      END.
   END.
   ELSE DO:
      IF openvar = 1 THEN DO:
         IF delkoll = TRUE THEN DO:
            OPEN QUERY sq FOR EACH SUMTID WHERE 
            SUMTID.AONR = valdaao.AONR AND SUMTID.DELNR = valdaao.DELNR AND 
            SUMTID.DATUM = uppvaltemp.STARTDATUM AND SUMTID.VECKOKORD NE "" NO-LOCK. 
            IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG BEGINS "S" AND
               TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR AND 
               YEAR(TIDREGITAB.DATUM) = YEAR(uppvaltemp.STARTDATUM) AND TIDREGITAB.VECKOKORD NE "" NO-LOCK.               
            END.
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG NE "" AND
               TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR AND 
               YEAR(TIDREGITAB.DATUM) = YEAR(uppvaltemp.STARTDATUM) AND TIDREGITAB.VECKOKORD NE "" NO-LOCK.               
            END.
         END.
         ELSE DO:
            OPEN QUERY sq FOR EACH SUMTID WHERE 
            SUMTID.AONR = valdaao.AONR AND  
            SUMTID.DATUM = uppvaltemp.STARTDATUM AND SUMTID.VECKOKORD NE ""
            NO-LOCK.               
            IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG BEGINS "S" AND
               TIDREGITAB.AONR = valdaao.AONR AND 
               YEAR(TIDREGITAB.DATUM) = YEAR(uppvaltemp.STARTDATUM) AND TIDREGITAB.VECKOKORD NE "" NO-LOCK.               
            END.
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG NE "" AND
               TIDREGITAB.AONR = valdaao.AONR AND 
               YEAR(TIDREGITAB.DATUM) = YEAR(uppvaltemp.STARTDATUM) AND TIDREGITAB.VECKOKORD NE "" NO-LOCK.               
            END.
         END.     
      END.
      IF openvar = 11 THEN DO:
         IF delkoll = TRUE THEN DO:
            OPEN QUERY stq FOR EACH SUMTIDDAG WHERE 
            SUMTIDDAG.AONR = valdaao.AONR AND SUMTIDDAG.DELNR = valdaao.DELNR AND 
            SUMTIDDAG.DATUM >= uppvaltemp.STARTDATUM AND 
            SUMTIDDAG.DATUM <= uppvaltemp.SLUTDATUM AND SUMTIDDAG.VECKOKORD NE ""
            NO-LOCK.         
            IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG BEGINS "S" AND
               TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR AND 
               TIDREGITAB.DATUM >= uppvaltemp.STARTDATUM AND 
               TIDREGITAB.DATUM <= uppvaltemp.SLUTDATUM AND TIDREGITAB.VECKOKORD NE "" NO-LOCK.               
            END.
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG NE "" AND
               TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR AND 
               TIDREGITAB.DATUM >= uppvaltemp.STARTDATUM AND 
               TIDREGITAB.DATUM <= uppvaltemp.SLUTDATUM AND TIDREGITAB.VECKOKORD NE "" NO-LOCK.               
            END.
         END.
         ELSE DO:
            OPEN QUERY stq FOR EACH SUMTIDDAG WHERE 
            SUMTIDDAG.AONR = valdaao.AONR AND  
            SUMTIDDAG.DATUM >= uppvaltemp.STARTDATUM AND 
            SUMTIDDAG.DATUM <= uppvaltemp.SLUTDATUM AND 
            SUMTIDDAG.VECKOKORD NE ""
            NO-LOCK.         
            IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG BEGINS "S" AND
               TIDREGITAB.AONR = valdaao.AONR AND 
               TIDREGITAB.DATUM >= uppvaltemp.STARTDATUM AND 
               TIDREGITAB.DATUM <= uppvaltemp.SLUTDATUM AND TIDREGITAB.VECKOKORD NE "" NO-LOCK.               
            END.
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
            OPEN QUERY stq FOR EACH SUMTIDDAG WHERE 
            SUMTIDDAG.AONR = valdaao.AONR AND SUMTIDDAG.DELNR = valdaao.DELNR 
            AND SUMTIDDAG.VECKOKORD NE ""
            NO-LOCK.
            IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG BEGINS "S" AND
               TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR AND 
               TIDREGITAB.VECKOKORD NE "" NO-LOCK.               
            END.
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG NE "" AND
               TIDREGITAB.AONR = valdaao.AONR AND TIDREGITAB.DELNR = valdaao.DELNR AND 
               TIDREGITAB.VECKOKORD NE "" NO-LOCK.               
            END.
         END.
         ELSE DO:            
            OPEN QUERY stq FOR EACH SUMTIDDAG WHERE 
            SUMTIDDAG.AONR = valdaao.AONR 
            AND SUMTIDDAG.VECKOKORD NE ""
            NO-LOCK.
            IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG BEGINS "S" AND
               TIDREGITAB.AONR = valdaao.AONR AND 
               TIDREGITAB.VECKOKORD NE "" NO-LOCK.               
            END.
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
               OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.LONTILLAGG NE "" AND
               TIDREGITAB.AONR = valdaao.AONR AND 
               TIDREGITAB.VECKOKORD NE "" NO-LOCK.               
            END.
         END.     
      END.      
   END.
   
   IF openvar = 2 THEN DO:
      IF delkoll = TRUE THEN DO:
         OPEN QUERY kq FOR EACH KOSTREG WHERE 
         KOSTREG.AONR = valdaao.AONR AND KOSTREG.DELNR = valdaao.DELNR AND
         YEAR(KOSTREG.REGDATUM) = YEAR(uppvaltemp.STARTDATUM) NO-LOCK.                        
      END.
      ELSE DO:
         OPEN QUERY kq FOR EACH KOSTREG WHERE 
         KOSTREG.AONR = valdaao.AONR AND 
         YEAR(KOSTREG.REGDATUM) = YEAR(uppvaltemp.STARTDATUM) NO-LOCK.                        
      END.      
   END.
   IF openvar = 21 THEN DO:
      IF delkoll = TRUE THEN DO:
         OPEN QUERY kq FOR EACH KOSTREG WHERE 
         KOSTREG.AONR = valdaao.AONR AND KOSTREG.DELNR = valdaao.DELNR AND
         KOSTREG.REGDATUM >= uppvaltemp.STARTDATUM AND 
         KOSTREG.REGDATUM <= uppvaltemp.SLUTDATUM NO-LOCK.         
      END.
      ELSE DO:
         OPEN QUERY kq FOR EACH KOSTREG WHERE 
         KOSTREG.AONR = valdaao.AONR AND 
         KOSTREG.REGDATUM >= uppvaltemp.STARTDATUM AND 
         KOSTREG.REGDATUM <= uppvaltemp.SLUTDATUM NO-LOCK.         
      END.      
   END.
   IF openvar = 22 THEN DO:
      IF delkoll = TRUE THEN DO:
         OPEN QUERY kq FOR EACH KOSTREG WHERE 
         KOSTREG.AONR = valdaao.AONR AND KOSTREG.DELNR = valdaao.DELNR
         NO-LOCK.         
      END.
      ELSE DO:
         OPEN QUERY kq FOR EACH KOSTREG WHERE 
         KOSTREG.AONR = valdaao.AONR 
         NO-LOCK.
      END.     
   END.
   
   
END PROCEDURE.

         
