/*ANVAVDSO.I*/
RUN anvavdhmt IN nyttaoapph (INPUT Guru.Konstanter:globanv,INPUT pkod,INPUT "",OUTPUT jid, OUTPUT TABLE eavdtemp ).                      
IF Guru.Konstanter:varforetypval[39] = 1 THEN DO:   
   IF ( Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" ) THEN FIND FIRST eavdtemp WHERE STRING(eavdtemp.AVDELNINGNR) BEGINS "1" NO-ERROR.      
   ELSE FIND FIRST eavdtemp NO-ERROR.      
END.
IF AVAILABLE eavdtemp AND Guru.Konstanter:varforetypval[39] = 1 THEN DO:    
   FOR EACH omrtemp BY omrtemp.NAMN: 
      FIND FIRST eavdtemp WHERE eavdtemp.AVDELNINGNR = omrtemp.AVDELNINGNR NO-LOCK NO-ERROR.  
      IF AVAILABLE eavdtemp THEN status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).                        
   END.
END.
ELSE DO:   
   FOR EACH eavdtemp,         
   EACH omrtemp WHERE omrtemp.AVDELNINGNR = eavdtemp.AVDELNINGNR.
      status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).                        
   END.  
END.


