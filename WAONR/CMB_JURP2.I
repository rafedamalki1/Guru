 /*CMB_JURP2.I*/
   
   CMB_AVD:LIST-ITEMS = "". 
   status-ok = CMB_AVD:ADD-FIRST("Alla").
   
   IF CMB_JURP:SCREEN-VALUE =  "Alla" THEN DO:
      FOR EACH avdtemp:
         status-ok = CMB_AVD:ADD-LAST(avdtemp.AVDELNINGNAMN).
      END. 
   END.
   ELSE DO:
      FIND FIRST jurperstemp WHERE jurperstemp.NAMN = CMB_JURP:SCREEN-VALUE NO-LOCK NO-ERROR. 
      FOR EACH judavdtemp WHERE judavdtemp.JUDID = jurperstemp.JUDID:
         status-ok = CMB_AVD:ADD-LAST(judavdtemp.AVDELNINGNAMN).         
      END.
   END.
   CMB_AVD:SCREEN-VALUE = "Alla".
   CMB_OMR:LIST-ITEMS = "". 
   status-ok = CMB_OMR:ADD-FIRST("Alla").
   IF CMB_JURP:SCREEN-VALUE =  "Alla" THEN DO:
      FOR EACH omrtemp:
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).
      END. 
   END.
   ELSE DO:
      FIND FIRST jurperstemp WHERE jurperstemp.NAMN = CMB_JURP:SCREEN-VALUE NO-LOCK NO-ERROR.     
      FOR EACH judavdtemp WHERE judavdtemp.JUDID = jurperstemp.JUDID,
      EACH omrtemp WHERE omrtemp.AVDELNINGNR = judavdtemp.AVDELNINGNR:
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).
      END.      
   END.
   CMB_OMR:SCREEN-VALUE = "Alla".
   
