
/*------------------------------------------------------------------------
    File        : LOSENREGLERSEK.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed May 04 09:33:04 CEST 2022
    Notes       :
  ----------------------------------------------------------------------*/

PROCEDURE sek_UI :
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE xsektemp NO-ERROR. 
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN SEKSTART.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT "",INPUT Guru.Konstanter:globniv,OUTPUT TABLE xsektemp).
   END.
   ELSE DO: 
      RUN SEKSTART.P 
      (INPUT "",INPUT Guru.Konstanter:globniv,OUTPUT TABLE xsektemp).
   END.   
      
   FOR EACH xsektemp BY xsektemp.MENYVART:
      IF xsektemp.MENYVART = "GURU" OR xsektemp.MENYVART = "ALLA" THEN DO:        
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:hoppsekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END. 
         END.
      END.          
      IF xsektemp.MENYVART = "STOR" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:storsekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END. 
         END.
      END.
      IF xsektemp.MENYVART = "REG"  THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:regsekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END. 
      END.
      IF xsektemp.MENYVART = "PLAN"  THEN DO:    
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:plansekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END. 
         END.
      END.
      IF xsektemp.MENYVART = "PERS" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:persekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END. 
         END.
      END.
      IF xsektemp.MENYVART = "FAKT" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:faktsekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END. 
         END.
      END.
      IF xsektemp.MENYVART = "BULA" THEN DO:               
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:bulasekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END.
      IF xsektemp.MENYVART = "TID" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:tidsekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END.
      IF xsektemp.MENYVART = "TIDB" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:tidbsekvar[i] = xsektemp.SEK[i].
               i = i + 1.
            END.
         END.
      END.
      IF xsektemp.MENYVART = "TIDT" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:tidtsekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END.
      IF xsektemp.MENYVART = "TIDA" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:tidasekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END.  
      IF xsektemp.MENYVART = "TIDS" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:tidssekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END.  
      IF xsektemp.MENYVART = "TADM" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:tadmsekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END.  
      IF xsektemp.MENYVART = "TIDO" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:tidosekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END. 
      IF xsektemp.MENYVART = "TIDR" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:tidrsekvar[i] = xsektemp.SEK[i].
               i = i + 1.
            END.
         END.
      END.
      IF xsektemp.MENYVART = "AONR" THEN DO:
         IF AVAILABLE xsektemp THEN DO:
         i = 1.
            DO WHILE i <= 20:
               Guru.Konstanter:aonrsekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END.
      IF xsektemp.MENYVART = "KALK" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:kalk2sekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END.
      IF xsektemp.MENYVART = "AREN" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:arendesek[i] = xsektemp.SEK[i].  
               i = i + 1.
            END.
         END.
      END.
      IF xsektemp.MENYVART = "MTRL" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:mtrlsekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END.
      IF xsektemp.MENYVART = "BERE" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:beresekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END.
   END.
   /*GURUSUPPORT*/
   IF Guru.Konstanter:globanv = CHR(71) + CHR(85) + CHR(82) + CHR(85) + CHR(83) + CHR(85) + CHR(80) + CHR(80) + CHR(79) + CHR(82) + CHR(84) THEN DO:
      Guru.Konstanter:hoppsekvar = FALSE. 
      Guru.Konstanter:storsekvar = FALSE. 
      Guru.Konstanter:regsekvar = FALSE. 
      Guru.Konstanter:plansekvar = FALSE. 
      Guru.Konstanter:persekvar = FALSE. 
      Guru.Konstanter:faktsekvar = FALSE. 
      Guru.Konstanter:bulasekvar = FALSE. 
      Guru.Konstanter:tidsekvar = FALSE. 
      Guru.Konstanter:tidbsekvar = FALSE.
      Guru.Konstanter:tidtsekvar = FALSE. 
      Guru.Konstanter:tidasekvar = FALSE. 
      Guru.Konstanter:tidssekvar = FALSE. 
      Guru.Konstanter:tadmsekvar = FALSE. 
      Guru.Konstanter:tidosekvar = FALSE. 
      Guru.Konstanter:tidrsekvar = FALSE.
      Guru.Konstanter:aonrsekvar = FALSE. 
      Guru.Konstanter:kalk2sekvar = FALSE. 
      Guru.Konstanter:arendesek = FALSE.  
      Guru.Konstanter:mtrlsekvar = FALSE. 
      Guru.Konstanter:beresekvar = FALSE. 
      Guru.Konstanter:mtrlsekvar[6] = TRUE. /*MTRLPRISER VISAS EJ*/
   END.   
END PROCEDURE.