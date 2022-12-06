   /*NYCOL2.I*/
   DEFINE VARIABLE tempvar2 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE radfast AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER val AS INTEGER NO-UNDO.
   CMB_AVD = INPUT FRAME {&FRAME-NAME} CMB_AVD.            
   CMB_OMR = INPUT FRAME {&FRAME-NAME} CMB_OMR.
   RAD_FAST = INPUT FRAME {&FRAME-NAME} RAD_FAST.
   IF omravdand = 1 THEN val = 2.
   IF omravdand = 2 THEN val = 1.
   IF RAD_FAST = 1 THEN radfast = FALSE.
   IF RAD_FAST = 2 THEN radfast = TRUE.
   IF RAD_FAST = 3 THEN DO:         
   END.
   ELSE DO:   
      IF val = 1 THEN DO:
         IF CMB_OMR = Guru.Konstanter:gomrk + " : alla" THEN DO:                                                
            tempvar2 = "STRING(utsokaonr.AONRAVDATUM) = STRING(91/01/01) AND utsokaonr.FASTAAONR = " + """" + STRING(radfast) + """".           
         END.                                                                                   
         ELSE DO:                                                                               
            FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-LOCK NO-ERROR.
            tempvar2 = "utsokaonr.OMRADE = " + """" + omrtemp.OMRADE + """" + " AND STRING(utsokaonr.AONRAVDATUM) = STRING(91/01/01)" +  
               " AND utsokaonr.FASTAAONR = " + """" + STRING(radfast) + """".
         END.  
      END.
      ELSE IF val = 2 THEN DO:     
         IF CMB_AVD= Guru.Konstanter:gavdk + " : alla" THEN DO:                                                
            tempvar2 = "STRING(utsokaonr.AONRAVDATUM) = STRING(91/01/01) AND utsokaonr.FASTAAONR = " + """" + STRING(radfast) + """".          
         END.                                                                                   
         ELSE DO:                                                                               
            FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNAMN = CMB_AVD NO-LOCK NO-ERROR.
            IF NOT AVAILABLE avdelningtemp THEN DO: 
               tempvar2 = "utsokaonr.AVDELNINGNR = ? AND STRING(utsokaonr.AONRAVDATUM) = STRING(91/01/01)" +  
               " AND utsokaonr.FASTAAONR = " + """" + STRING(radfast) + """".
            END.
            ELSE DO:
               tempvar2 = "utsokaonr.AVDELNINGNR = " + STRING(avdelningtemp.AVDELNINGNR) + " AND STRING(utsokaonr.AONRAVDATUM) = STRING(91/01/01)" +  
               " AND utsokaonr.FASTAAONR = " + """" + STRING(radfast) + """".
            END.
         END.
      END.
      /*Lägg till fasta utan område*/      
      FIND FIRST utsokaonr WHERE utsokaonr.FASTAAONR = TRUE NO-LOCK NO-ERROR.
      IF AVAILABLE utsokaonr THEN DO:
         IF RAD_FAST = 2 THEN tempvar2 = tempvar2 + ' OR utsokaonr.OMRADE = " " '.  
      END.
      ELSE DO:
         IF RAD_FAST = 1 THEN tempvar2 = tempvar2 + ' OR utsokaonr.OMRADE = " " '.  
      END.
      RUN setcolsortvar_UI IN brwproc[1] (INPUT tempvar2).
   END.
   
