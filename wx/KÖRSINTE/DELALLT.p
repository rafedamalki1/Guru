/*DELALLT.P PROGRAMMET TAR BORT BEREDNINGSUPPLÄGGET*/

DEFINE VARIABLE globforetag AS CHARACTER NO-UNDO.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
   {muswait.i}   
   OPEN QUERY mtrlq FOR EACH KONSTGRUPP NO-LOCK.
   DO TRANSACTION:
      GET FIRST mtrlq EXCLUSIVE-LOCK.
      IF AVAILABLE KONSTGRUPP THEN DO:
         DELETE KONSTGRUPP.         
      END.  
   END.
   REPEAT:      
      DO TRANSACTION:
         GET NEXT mtrlq EXCLUSIVE-LOCK.
         IF AVAILABLE KONSTGRUPP THEN DO:
            DELETE KONSTGRUPP.
         END.
         ELSE LEAVE.   
      END.
   END.                  
   CLOSE QUERY mtrlq.

   OPEN QUERY aq FOR EACH BBENAMNING NO-LOCK.
   DO TRANSACTION:
      GET FIRST aq EXCLUSIVE-LOCK.
      IF AVAILABLE BBENAMNING THEN DO:
         DELETE BBENAMNING.         
      END.  
   END.
   REPEAT:      
      DO TRANSACTION:
         GET NEXT aq EXCLUSIVE-LOCK.
         IF AVAILABLE BBENAMNING THEN DO:
            DELETE BBENAMNING.
         END.
         ELSE LEAVE.   
      END.
   END.                  
   CLOSE QUERY aq.

   OPEN QUERY cq FOR EACH EDIGRUPP NO-LOCK.
   DO TRANSACTION:
      GET FIRST cq EXCLUSIVE-LOCK.
      IF AVAILABLE EDIGRUPP THEN DO:
         DELETE EDIGRUPP.         
      END.  
   END.
   REPEAT:      
      DO TRANSACTION:
         GET NEXT cq EXCLUSIVE-LOCK.
         IF AVAILABLE EDIGRUPP THEN DO:
            DELETE EDIGRUPP.
         END.
         ELSE LEAVE.   
      END.
   END.                  
   CLOSE QUERY cq.

   OPEN QUERY dq FOR EACH KONSTRUKTION NO-LOCK.
   DO TRANSACTION:
      GET FIRST dq EXCLUSIVE-LOCK.
      IF AVAILABLE KONSTRUKTION THEN DO:
         DELETE KONSTRUKTION.         
      END.  
   END.
   REPEAT:      
      DO TRANSACTION:
         GET NEXT dq EXCLUSIVE-LOCK.
         IF AVAILABLE KONSTRUKTION THEN DO:
            DELETE KONSTRUKTION.
         END.
         ELSE LEAVE.   
      END.
   END.                  
   CLOSE QUERY dq.

   OPEN QUERY eq FOR EACH KONSTVAL NO-LOCK.
   DO TRANSACTION:
      GET FIRST eq EXCLUSIVE-LOCK.
      IF AVAILABLE KONSTVAL THEN DO:
         DELETE KONSTVAL.         
      END.  
   END.
   REPEAT:      
      DO TRANSACTION:
         GET NEXT eq EXCLUSIVE-LOCK.
         IF AVAILABLE KONSTVAL THEN DO:
            DELETE KONSTVAL.
         END.
         ELSE LEAVE.   
      END.
   END.                  
   CLOSE QUERY eq.

   OPEN QUERY fq FOR EACH MTRLBER NO-LOCK.
   DO TRANSACTION:
      GET FIRST fq EXCLUSIVE-LOCK.
      IF AVAILABLE MTRLBER THEN DO:
         DELETE MTRLBER.         
      END.  
   END.
   REPEAT:      
      DO TRANSACTION:
         GET NEXT fq EXCLUSIVE-LOCK.
         IF AVAILABLE MTRLBER THEN DO:
            DELETE MTRLBER.
         END.
         ELSE LEAVE.   
      END.
   END.                  
   CLOSE QUERY fq.

   OPEN QUERY gq FOR EACH BERSTOLP NO-LOCK.
   DO TRANSACTION:
      GET FIRST gq EXCLUSIVE-LOCK.
      IF AVAILABLE BERSTOLP THEN DO:
         DELETE BERSTOLP.         
      END.  
   END.
   REPEAT:      
      DO TRANSACTION:
         GET NEXT gq EXCLUSIVE-LOCK.
         IF AVAILABLE BERSTOLP THEN DO:
            DELETE BERSTOLP.
         END.
         ELSE LEAVE.   
      END.
   END.                  
   CLOSE QUERY gq.

   OPEN QUERY hq FOR EACH BERSKAP NO-LOCK.
   DO TRANSACTION:
      GET FIRST hq EXCLUSIVE-LOCK.
      IF AVAILABLE BERSKAP THEN DO:
         DELETE BERSKAP.         
      END.  
   END.
   REPEAT:      
      DO TRANSACTION:
         GET NEXT hq EXCLUSIVE-LOCK.
         IF AVAILABLE BERSKAP THEN DO:
            DELETE BERSKAP.
         END.
         ELSE LEAVE.   
      END.
   END.                  
   CLOSE QUERY hq.
   
   OPEN QUERY iq FOR EACH KALKBER NO-LOCK.
   DO TRANSACTION:
      GET FIRST iq EXCLUSIVE-LOCK.
      IF AVAILABLE KALKBER THEN DO:
         DELETE KALKBER.         
      END.  
   END.
   REPEAT:      
      DO TRANSACTION:
         GET NEXT iq EXCLUSIVE-LOCK.
         IF AVAILABLE KALKBER THEN DO:
            DELETE KALKBER.
         END.
         ELSE LEAVE.   
      END.
   END.                  
   CLOSE QUERY iq.
   IF  
   globforetag = "VAST"  THEN DO:
      OUTPUT TO e:\delad\pro9\guru\koll.txt APPEND.
      PUT "borttag allt klart. " substring(globforetag,1,10) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      OUTPUT TO d:\elpool\delad\pro9\wrk\koll.txt APPEND.
      PUT "borttag allt klart. " substring(globforetag,1,10) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      OUTPUT TO D:\DELAD\klient\PRO9\koll.txt APPEND.
      PUT "borttag allt klart. " substring(globforetag,1,10) SKIP.
      OUTPUT CLOSE.
   END.

