DEFINE QUERY prisq FOR MTRL.

   OPEN QUERY prisq FOR EACH MTRL WHERE MTRL.LEVKOD = "2" AND
   MTRL.KALKNR = 0 NO-LOCK USE-INDEX LEV.
   DO TRANSACTION:
      GET FIRST prisq EXCLUSIVE-LOCK.
      MTRL.BPRIS = MTRL.NPRIS * 1.2.
   END.   

   DO WHILE AVAILABLE(MTRL):
      DO TRANSACTION:
         GET NEXT prisq EXCLUSIVE-LOCK.
         IF AVAILABLE MTRL THEN
         MTRL.BPRIS = MTRL.NPRIS * 1.2.      
      END.   
   END.
   CLOSE QUERY prisq.   