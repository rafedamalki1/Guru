 /*CMB_AVDN2.I*/
   CMB_OMR:LIST-ITEMS = "". 
   status-ok = CMB_OMR:ADD-FIRST("Alla").
   IF CMB_AVD:SCREEN-VALUE= Guru.Konstanter:gavdk + " : alla" OR CMB_AVD:SCREEN-VALUE = "Alla" THEN DO:
      FOR EACH judavdtemp:
         FOR EACH omrtemp WHERE omrtemp.AVDELNINGNR = judavdtemp.AVDELNINGNR:
            status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).
         END.
      END.   
      /*
      IF jid = "" THEN DO:      
         FOR EACH omrtemp:
            status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).
         END. 
      END.
      ELSE DO:
         FOR EACH eavdtemp,         
         EACH omrtemp WHERE omrtemp.AVDELNINGNR = eavdtemp.AVDELNINGNR.
            status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).                        
         END.
      END.
      */
   END.
   ELSE DO:
      FIND FIRST avdtemp WHERE avdtemp.AVDELNINGNAMN = CMB_AVD:SCREEN-VALUE NO-LOCK NO-ERROR.         
      FOR EACH omrtemp WHERE omrtemp.AVDELNINGNR = avdtemp.AVDELNINGNR:
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).
      END.      
   END.
   CMB_OMR:SCREEN-VALUE = "Alla".
  
