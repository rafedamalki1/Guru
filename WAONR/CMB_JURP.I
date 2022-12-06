 /*CMB_JURP.I*/
   
   CMB_AVD:LIST-ITEMS = "". 
   status-ok = CMB_AVD:ADD-FIRST("Alla").
   
   IF CMB_JURP =  "Alla" THEN DO:
      FOR EACH avdtemp:
         status-ok = CMB_AVD:ADD-LAST(avdtemp.AVDELNINGNAMN).
      END. 
   END.
   ELSE DO:
      FIND FIRST jurperstemp WHERE jurperstemp.NAMN = CMB_JURP NO-LOCK NO-ERROR. 
      FOR EACH judavdtemp WHERE judavdtemp.JUDID = jurperstemp.JUDID:
         status-ok = CMB_AVD:ADD-LAST(judavdtemp.AVDELNINGNAMN).         
      END.
   END.
   CMB_AVD:SCREEN-VALUE = "Alla".
   CMB_AVD = "Alla".
   CMB_OMR:LIST-ITEMS = "". 
   status-ok = CMB_OMR:ADD-FIRST("Alla").

   IF CMB_JURP =  "Alla" THEN DO:
      IF Guru.Konstanter:varforetypval[39] = 1 THEN DO:   
         IF ( Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" ) THEN FIND FIRST judavdtemp WHERE STRING(judavdtemp.AVDELNINGNR) BEGINS "1" NO-ERROR.      
         ELSE FIND FIRST judavdtemp NO-ERROR.      
      END.
      IF AVAILABLE judavdtemp AND Guru.Konstanter:varforetypval[39] = 1 THEN DO:    
         FOR EACH omrtemp BY omrtemp.NAMN: 
            FIND FIRST judavdtemp WHERE judavdtemp.AVDELNINGNR = omrtemp.AVDELNINGNR NO-LOCK NO-ERROR.  
            IF AVAILABLE judavdtemp THEN status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).                        
         END.
      END.
      ELSE DO:   
         FOR EACH judavdtemp,         
         EACH omrtemp WHERE omrtemp.AVDELNINGNR = judavdtemp.AVDELNINGNR.
            status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).                        
         END.  
      END.
   END.
   ELSE DO:
      FIND FIRST jurperstemp WHERE jurperstemp.NAMN = CMB_JURP NO-LOCK NO-ERROR.     
      IF Guru.Konstanter:varforetypval[39] = 1 THEN DO:            
         IF ( Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" ) THEN FIND FIRST judavdtemp WHERE judavdtemp.JUDID = jurperstemp.JUDID AND STRING(judavdtemp.AVDELNINGNR) BEGINS "1" NO-ERROR.      
         ELSE FIND FIRST judavdtemp NO-ERROR.      
      END.
      IF AVAILABLE judavdtemp AND Guru.Konstanter:varforetypval[39] = 1 THEN DO:             
         FOR EACH omrtemp BY omrtemp.NAMN: 
            FIND FIRST judavdtemp WHERE judavdtemp.JUDID = jurperstemp.JUDID AND judavdtemp.AVDELNINGNR = omrtemp.AVDELNINGNR NO-LOCK NO-ERROR.  
            IF AVAILABLE judavdtemp THEN status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).                        
         END.
      END.
      ELSE DO:   
         FOR EACH judavdtemp WHERE judavdtemp.JUDID = jurperstemp.JUDID,         
         EACH omrtemp WHERE omrtemp.AVDELNINGNR = judavdtemp.AVDELNINGNR.
            status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).                        
         END.  
      END.
   END.
   CMB_OMR:SCREEN-VALUE = "Alla".
   CMB_OMR = "Alla".  
