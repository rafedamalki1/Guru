 /*CMB_AVDJ.I*/
   CMB_OMR:LIST-ITEMS = "". 
   status-ok = CMB_OMR:ADD-FIRST("Alla").
   IF CMB_AVD= Guru.Konstanter:gavdk + " : alla" OR CMB_AVD = "Alla" THEN DO:
      FOR EACH omrtemp:
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).
      END. 
   END.
   ELSE DO:
      FIND FIRST avdtemp WHERE avdtemp.AVDELNINGNAMN = CMB_AVD NO-LOCK NO-ERROR.     

      FOR EACH omrtemp WHERE omrtemp.AVDELNINGNR = avdtemp.AVDELNINGNR:
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).
      END.      
   END.
   CMB_OMR:SCREEN-VALUE = "Alla".
