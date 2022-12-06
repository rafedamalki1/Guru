/*FLYTTBER.p*/
DEFINE VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE valomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE valaonr2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE valomrade2 AS CHARACTER NO-UNDO.

ASSIGN
valaonr = "110"
valomrade = "TEMV"
valaonr2 = "287"
valomrade2 = "TEMÖ".         
      
   OPEN QUERY kq FOR EACH BERVAL WHERE BERVAL.AONR = valaonr AND
   BERVAL.OMRADE = valomrade NO-LOCK.
   DO TRANSACTION:
      GET FIRST kq EXCLUSIVE-LOCK.
      IF AVAILABLE BERVAL THEN DO:
         ASSIGN
         BERVAL.AONR = valaonr2
         BERVAL.OMRADE = valomrade2.
      END.
   END.   
   REPEAT:
      DO TRANSACTION:
         GET NEXT kq EXCLUSIVE-LOCK.
         IF AVAILABLE BERVAL THEN DO:
            ASSIGN
            BERVAL.AONR = valaonr2
            BERVAL.OMRADE = valomrade2.
         END.
         ELSE LEAVE.
      END.      
   END.   
   CLOSE QUERY kq.   
   
   OPEN QUERY ordq FOR EACH BERORD WHERE BERORD.AONR = valaonr AND
   BERORD.OMRADE = valomrade USE-INDEX ORD NO-LOCK.
   DO TRANSACTION:
      GET FIRST ordq EXCLUSIVE-LOCK.
      IF AVAILABLE BERORD THEN DO:
         ASSIGN
         BERORD.AONR = valaonr2
         BERORD.OMRADE = valomrade2.
      END.
   END.   
   REPEAT:
      DO TRANSACTION:
         GET NEXT ordq EXCLUSIVE-LOCK.
         IF AVAILABLE BERORD THEN DO:
            ASSIGN
            BERORD.AONR = valaonr2
            BERORD.OMRADE = valomrade2.
         END.
         ELSE LEAVE.
      END.      
   END.   
   CLOSE QUERY ordq.   
   
   OPEN QUERY friq FOR EACH FRIKORT WHERE FRIKORT.AONR = valaonr AND
   FRIKORT.OMRADE = valomrade USE-INDEX OMR NO-LOCK.
   DO TRANSACTION:
      GET FIRST friq EXCLUSIVE-LOCK.
      IF AVAILABLE FRIKORT THEN DO:
         ASSIGN
         FRIKORT.AONR = valaonr2
         FRIKORT.OMRADE = valomrade2.
      END.
   END.   
   REPEAT:
      DO TRANSACTION:
         GET NEXT friq EXCLUSIVE-LOCK.
         IF AVAILABLE FRIKORT THEN DO:
            ASSIGN
            FRIKORT.AONR = valaonr2
            FRIKORT.OMRADE = valomrade2.
         END.
         ELSE LEAVE.
      END.      
   END.   
   CLOSE QUERY friq.   
   
   OPEN QUERY berqid FOR EACH BERID WHERE BERID.AONR = valaonr AND
   BERID.OMRADE = valomrade USE-INDEX OMR NO-LOCK.
   DO TRANSACTION:
      GET FIRST berqid EXCLUSIVE-LOCK.
      IF AVAILABLE BERID THEN DO:
         ASSIGN
         BERID.AONR = valaonr2
         BERID.OMRADE = valomrade2.
      END.
   END.   
   REPEAT:
      DO TRANSACTION:
         GET NEXT berqid EXCLUSIVE-LOCK.
         IF AVAILABLE BERID THEN DO:
            ASSIGN
            BERID.AONR = valaonr2
            BERID.OMRADE = valomrade2.
         END.
         ELSE LEAVE.
      END.      
   END.   
   CLOSE QUERY berqid.   
   /*HD HÄR*/
   OPEN QUERY berqkal FOR EACH BERKALK WHERE BERKALK.AONR = valaonr AND
   BERKALK.OMRADE = valomrade USE-INDEX OMR NO-LOCK.
   DO TRANSACTION:
      GET FIRST berqkal EXCLUSIVE-LOCK.
      IF AVAILABLE BERKALK THEN DO:
         ASSIGN
         BERKALK.AONR = valaonr2
         BERKALK.OMRADE = valomrade2.
      END.
   END.   
   REPEAT:
      DO TRANSACTION:
         GET NEXT berqkal EXCLUSIVE-LOCK.
         IF AVAILABLE BERKALK THEN DO:
            ASSIGN
            BERKALK.AONR = valaonr2
            BERKALK.OMRADE = valomrade2.
         END.
         ELSE LEAVE.
      END.      
   END.   
   CLOSE QUERY berqkal.   
       
   OPEN QUERY berqmtrl FOR EACH BERMTRL WHERE BERMTRL.AONR = valaonr AND
   BERMTRL.OMRADE = valomrade USE-INDEX OMR NO-LOCK.
   DO TRANSACTION:
      GET FIRST berqmtrl EXCLUSIVE-LOCK.
      IF AVAILABLE BERMTRL THEN DO:
         ASSIGN
         BERMTRL.AONR = valaonr2
         BERMTRL.OMRADE = valomrade2.
      END.
   END.   
   REPEAT:
      DO TRANSACTION:
         GET NEXT berqmtrl EXCLUSIVE-LOCK.
         IF AVAILABLE BERMTRL THEN DO:
            ASSIGN
            BERMTRL.AONR = valaonr2
            BERMTRL.OMRADE = valomrade2.
         END.
         ELSE LEAVE.
      END.      
   END.   
   CLOSE QUERY berqmtrl.    
      
   OPEN QUERY berqlin FOR EACH BERLINKAB WHERE BERLINKAB.AONR = valaonr AND
   BERLINKAB.OMRADE = valomrade USE-INDEX OMR NO-LOCK.
   DO TRANSACTION:
      GET FIRST berqlin EXCLUSIVE-LOCK.
      IF AVAILABLE BERLINKAB THEN DO:
         ASSIGN
         BERLINKAB.AONR = valaonr2
         BERLINKAB.OMRADE = valomrade2.
      END.
   END.   
   REPEAT:
      DO TRANSACTION:
         GET NEXT berqlin EXCLUSIVE-LOCK.
         IF AVAILABLE BERLINKAB THEN DO:
            ASSIGN
            BERLINKAB.AONR = valaonr2
            BERLINKAB.OMRADE = valomrade2.
         END.
         ELSE LEAVE.
      END.      
   END.   
   CLOSE QUERY berqlin.   
   
   OPEN QUERY berqskydd FOR EACH KSKYDD WHERE KSKYDD.AONR = valaonr AND
   KSKYDD.OMRADE = valomrade AND KSKYDD.BERED = TRUE
   USE-INDEX OMR NO-LOCK.
   DO TRANSACTION:
      GET FIRST berqskydd EXCLUSIVE-LOCK.
      IF AVAILABLE KSKYDD THEN DO:
         ASSIGN
         KSKYDD.AONR = valaonr2
         KSKYDD.OMRADE = valomrade2.
      END.
   END.   
   REPEAT:
      DO TRANSACTION:
         GET NEXT berqskydd EXCLUSIVE-LOCK.
         IF AVAILABLE KSKYDD THEN DO:
            ASSIGN
            KSKYDD.AONR = valaonr2
            KSKYDD.OMRADE = valomrade2.
         END.
         ELSE LEAVE.
      END.      
   END.   
   CLOSE QUERY berqskydd.   
