 /*CMB_AVD2.I*/
    CMB_OMR:LIST-ITEMS = "". 
   status-ok = CMB_OMR:ADD-FIRST(gomrk + " : alla").
   IF CMB_AVD= Guru.Konstanter:gavdk + " : alla" OR CMB_AVD = "Alla" THEN DO:
      FOR EACH judavdtemp:
      FOR EACH omrtemp WHERE omrtemp.AVDELNINGNR = judavdtemp.AVDELNINGNR:
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).
      END. 
   END.
   ELSE DO:
      FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNAMN = CMB_AVD NO-LOCK NO-ERROR.     

      FOR EACH omrtemp WHERE omrtemp.AVDELNINGNR = avdelningtemp.AVDELNINGNR:
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).
      END.      
   END.
   CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
