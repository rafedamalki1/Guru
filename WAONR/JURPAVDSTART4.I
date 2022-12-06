/*JURPAVDSTART4.I*/
   DEFINE VARIABLE juidraknare AS INTEGER NO-UNDO. 
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
   FOR EACH judavdtemp,         
   EACH omrtemp WHERE omrtemp.AVDELNINGNR = judavdtemp.AVDELNINGNR.
      CREATE omrtemp3.
      BUFFER-COPY omrtemp TO omrtemp3 .     
   END.

   
