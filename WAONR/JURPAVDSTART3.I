/*JURPAVDSTART3.I*/
   DEFINE VARIABLE juidraknare AS INTEGER NO-UNDO.
   DEFINE VARIABLE avdraknare AS INTEGER NO-UNDO. 
   FIND FIRST jurperstemp  NO-LOCK NO-ERROR.
   IF NOT AVAILABLE jurperstemp THEN DO:
      RUN jurp_UI IN aonrapph (INPUT Guru.Konstanter:globanv,OUTPUT TABLE jurperstemp,OUTPUT TABLE judavdtemp).
   END.
   FIND FIRST avdtemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE avdtemp THEN DO:
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN AVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (OUTPUT TABLE avdtemp).
      END.
      ELSE DO:
         RUN AVSKAP.P 
         (OUTPUT TABLE avdtemp).                  
      END.     
   END.
   /*visa bara eget bolags områden*/  
   CMB_OMR:LIST-ITEMS = "". 
   status-ok = CMB_OMR:ADD-FIRST("Alla").
   
   FOR EACH judavdtemp,         
   EACH omrtemp WHERE omrtemp.AVDELNINGNR = judavdtemp.AVDELNINGNR.
      status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).                        
   END.
   CMB_JURP:LIST-ITEMS = "". 
   status-ok = CMB_JURP:ADD-FIRST("Alla").
   juidraknare = 0.
   FOR EACH jurperstemp:
     status-ok = CMB_JURP:ADD-LAST(jurperstemp.NAMN).
     juidraknare = juidraknare + 1.
   END.         
   IF Guru.Konstanter:globjid = "" THEN DO:
      CMB_JURP:SCREEN-VALUE = "Alla".   
   END.    
   ELSE DO:  
      FIND FIRST jurperstemp WHERE jurperstemp.JUDID = Guru.Konstanter:globjid NO-LOCK NO-ERROR.
      IF NOT AVAILABLE jurperstemp THEN DO:
         FIND FIRST jurperstemp NO-LOCK NO-ERROR.
      END.
      IF AVAILABLE jurperstemp THEN DO:
         CMB_JURP:SCREEN-VALUE = jurperstemp.NAMN.
      END.    
      ELSE DO:
         CMB_JURP:SCREEN-VALUE = "".     
      END.
   END.   
   IF juidraknare < 2 THEN CMB_JURP:HIDDEN = TRUE.
   avdraknare = 0.
   CMB_AVD:LIST-ITEMS = "".
   status-ok = CMB_AVD:ADD-FIRST("Alla").
   FOR EACH judavdtemp:
      status-ok = CMB_AVD:ADD-LAST(judavdtemp.AVDELNINGNAMN).
      avdraknare = avdraknare + 1.
   END.
   CMB_AVD:SCREEN-VALUE = "Alla".
   IF avdraknare < 2 THEN CMB_AVD:HIDDEN = TRUE.

   /*visa bara eget bolags områden*/ 
   /*  
   jid = "".
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      EMPTY TEMP-TABLE eavdtemp NO-ERROR.       
      RUN anvavdhmt IN nyttaoapph2 (INPUT Guru.Konstanter:globanv,INPUT "",INPUT "",OUTPUT jid, OUTPUT TABLE eavdtemp ).                       
   END.
   IF jid = "" THEN DO:      
      OPEN QUERY oq FOR EACH omrtemp USE-INDEX OMR NO-LOCK.
      GET FIRST oq NO-LOCK.
      CMB_OMR:LIST-ITEMS = "".
      CMB_OMR:ADD-FIRST("Alla").
      DO WHILE AVAILABLE(omrtemp):         
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).         
         GET NEXT oq NO-LOCK.
      END.
      CMB_JURP:LIST-ITEMS = "". 
      status-ok = CMB_JURP:ADD-FIRST("Alla").
      FOR EACH jurperstemp:
         status-ok = CMB_JURP:ADD-LAST(jurperstemp.NAMN).
      END.
      CMB_JURP:SCREEN-VALUE = "Alla".    
      CMB_AVD:LIST-ITEMS = "". 
      status-ok = CMB_AVD:ADD-FIRST("Alla").
      FOR EACH avdtemp:
         status-ok = CMB_AVD:ADD-LAST(avdtemp.AVDELNINGNAMN).
      END.
      CMB_AVD:SCREEN-VALUE = "Alla".  
   END.
   ELSE DO:
      CMB_OMR:LIST-ITEMS = "".
      CMB_OMR:ADD-FIRST("Alla").
      FOR EACH eavdtemp,         
      EACH omrtemp WHERE omrtemp.AVDELNINGNR = eavdtemp.AVDELNINGNR.
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).                        
      END.
      CMB_JURP:LIST-ITEMS = "". 
      status-ok = CMB_JURP:ADD-FIRST("Alla").
      FOR EACH jurperstemp:
         status-ok = CMB_JURP:ADD-LAST(jurperstemp.NAMN).
      END.            
      FIND FIRST jurperstemp WHERE jurperstemp.JUDID = jid NO-LOCK NO-ERROR.      
      CMB_JURP:SCREEN-VALUE = jurperstemp.NAMN.      
      CMB_AVD:LIST-ITEMS = "".
      status-ok = CMB_AVD:ADD-FIRST("Alla").
      FOR EACH eavdtemp:
         status-ok = CMB_AVD:ADD-LAST(eavdtemp.AVDELNINGNAMN).
      END.
      CMB_AVD:SCREEN-VALUE = "Alla".        
   END.
*/
   
