 /*CMB_PRIO.I*/
   CMB_PRIO:LIST-ITEMS = "". 
   status-ok = CMB_PRIO:ADD-FIRST("Alla").
   IF Guru.Konstanter:varforetypval[45] = 0 THEN DO:
      FOR EACH priotemp WHERE priotemp.OMRADE = "":         
         status-ok = CMB_PRIO:ADD-LAST(priotemp.P-BENAMNING).         
      END.
   END.   
   ELSE DO:
      IF CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla" OR CMB_OMR:SCREEN-VALUE = "Alla" THEN DO:
         FOR EACH priotemp WHERE priotemp.OMRADE = "":         
            status-ok = CMB_PRIO:ADD-LAST(priotemp.P-BENAMNING).         
         END.            
      END.
      ELSE DO:            
         FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR:SCREEN-VALUE NO-LOCK NO-ERROR.
         FOR EACH priotemp WHERE priotemp.OMRADE = omrtemp.OMRADE:         
            status-ok = CMB_PRIO:ADD-LAST(priotemp.P-BENAMNING).         
         END.
      END.                  
      
   END.   
   CMB_PRIO:SCREEN-VALUE = "Alla".
  
