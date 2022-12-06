/*JURPAVDSTART.I*/
   RUN jurp_UI IN aonrapph (OUTPUT TABLE jurperstemp,OUTPUT TABLE judavdtemp).
   status-ok = CMB_JURP:ADD-FIRST("Alla").
   FOR EACH jurperstemp:
      status-ok = CMB_JURP:ADD-LAST(jurperstemp.NAMN).
   END.
   CMB_JURP:SCREEN-VALUE = "Alla".  
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN AVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (OUTPUT TABLE avdtemp).
   END.
   ELSE DO:
      RUN AVSKAP.P 
      (OUTPUT TABLE avdtemp).                  
   END.     
   status-ok = CMB_AVD:ADD-FIRST("Alla").
   FOR EACH avdtemp:
      status-ok = CMB_AVD:ADD-LAST(avdtemp.AVDELNINGNAMN).
   END.
   CMB_AVD:SCREEN-VALUE = "Alla".  
