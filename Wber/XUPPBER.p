DEFINE VARIABLE kalle AS INTEGER NO-UNDO.
DEFINE BUFFER kbuff FOR KONSTVAL.

/* OPEN QUERY gq FOR EACH KONSTGRUPP NO-LOCK. */
/* GET FIRST gq NO-LOCK.                      */
/* DO WHILE AVAILABLE(KONSTGRUPP):            */
   
   FOR EACH KONSTGRUPP WHERE KONSTGRUPP.KONSKOD = 0:
   FIND FIRST BBENAMNING WHERE BBENAMNING.KONSKOD = KONSTGRUPP.KONSKOD
   NO-LOCK NO-ERROR.
   IF BBENAMNING.B2 NE "" THEN DO:
      kalle = 1.
      OPEN QUERY kq FOR EACH KONSTVAL WHERE KONSTVAL.KONSKOD = KONSTGRUPP.KONSKOD AND
      KONSTVAL.BB = BBENAMNING.B2 NO-LOCK BY KONSTVAL.KVALKOD.
      DO TRANSACTION:
         GET FIRST kq EXCLUSIVE-LOCK.
         IF AVAILABLE KONSTVAL THEN DO:
            IF KONSTVAL.ORDNING = 0 OR KONSTVAL.ORDNING = ? THEN DO:            
               KONSTVAL.ORDNING = kalle.
               OPEN QUERY lq FOR EACH kbuff WHERE kbuff.KONSKOD = KONSTGRUPP.KONSKOD AND
               kbuff.BB = BBENAMNING.B2 AND kbuff.KVALKOD = KONSTVAL.KVALKOD NO-LOCK.            
               GET FIRST lq EXCLUSIVE-LOCK.
               IF AVAILABLE KBUFF THEN DO:
                  kbuff.ORDNING = kalle.
               END.            
               REPEAT:               
                  GET NEXT lq EXCLUSIVE-LOCK.
                  IF AVAILABLE kbuff THEN DO:
                     kbuff.ORDNING = kalle.
                  END.
                  ELSE LEAVE.              
               END.
               kalle = kalle + 1.
            END.
         END.
      END.
      REPEAT:
         DO TRANSACTION:
            GET NEXT kq EXCLUSIVE-LOCK.
            IF AVAILABLE KONSTVAL THEN DO:
               IF KONSTVAL.ORDNING = 0 OR KONSTVAL.ORDNING = ? THEN DO:            
                  KONSTVAL.ORDNING = kalle.
                  OPEN QUERY lq FOR EACH kbuff WHERE kbuff.KONSKOD = KONSTGRUPP.KONSKOD AND
                  kbuff.BB = BBENAMNING.B2 AND kbuff.KVALKOD = KONSTVAL.KVALKOD NO-LOCK.               
                  GET FIRST lq EXCLUSIVE-LOCK.
                  IF AVAILABLE KBUFF THEN DO:
                     kbuff.ORDNING = kalle.
                  END.              
                  REPEAT:                  
                     GET NEXT lq EXCLUSIVE-LOCK.
                     IF AVAILABLE kbuff THEN DO:
                        kbuff.ORDNING = kalle.
                     END.
                     ELSE LEAVE.                  
                  END.
                  kalle = kalle + 1.
               END. 
            END.
            ELSE LEAVE.
         END.
      END.
   END.
   IF BBENAMNING.B3 NE "" THEN DO:
      kalle = 1.
      OPEN QUERY kq FOR EACH KONSTVAL WHERE KONSTVAL.KONSKOD = KONSTGRUPP.KONSKOD AND
      KONSTVAL.BB = BBENAMNING.B3 NO-LOCK BY KONSTVAL.KVALKOD.
      DO TRANSACTION:
         GET FIRST kq EXCLUSIVE-LOCK.
         IF AVAILABLE KONSTVAL THEN DO:
            IF KONSTVAL.ORDNING = 0 OR KONSTVAL.ORDNING = ? THEN DO:
               KONSTVAL.ORDNING = kalle.
               OPEN QUERY lq FOR EACH kbuff WHERE kbuff.KONSKOD = KONSTGRUPP.KONSKOD AND
               kbuff.BB = BBENAMNING.B3 AND kbuff.KVALKOD = KONSTVAL.KVALKOD NO-LOCK.            
               GET FIRST lq EXCLUSIVE-LOCK.
               IF AVAILABLE KBUFF THEN DO:
                  kbuff.ORDNING = kalle.
               END.            
               REPEAT:               
                  GET NEXT lq EXCLUSIVE-LOCK.
                  IF AVAILABLE kbuff THEN DO:
                     kbuff.ORDNING = kalle.
                  END.
                  ELSE LEAVE.              
               END.
               kalle = kalle + 1.
            END.
         END.
      END.
      REPEAT:
         DO TRANSACTION:
            GET NEXT kq EXCLUSIVE-LOCK.
            IF AVAILABLE KONSTVAL THEN DO:
               IF KONSTVAL.ORDNING = 0 OR KONSTVAL.ORDNING = ? THEN DO:
                  KONSTVAL.ORDNING = kalle.
                  OPEN QUERY lq FOR EACH kbuff WHERE kbuff.KONSKOD = KONSTGRUPP.KONSKOD AND
                  kbuff.BB = BBENAMNING.B3 AND kbuff.KVALKOD = KONSTVAL.KVALKOD NO-LOCK.               
                  GET FIRST lq EXCLUSIVE-LOCK.
                  IF AVAILABLE KBUFF THEN DO:
                     kbuff.ORDNING = kalle.
                  END.              
                  REPEAT:                  
                     GET NEXT lq EXCLUSIVE-LOCK.
                     IF AVAILABLE kbuff THEN DO:
                        kbuff.ORDNING = kalle.
                     END.
                     ELSE LEAVE.                  
                  END.
                  kalle = kalle + 1.
               END.
            END.
            ELSE LEAVE.
         END.
      END.
   END.
   IF BBENAMNING.B4 NE "" THEN DO:
      kalle = 1.
      OPEN QUERY kq FOR EACH KONSTVAL WHERE KONSTVAL.KONSKOD = KONSTGRUPP.KONSKOD AND
      KONSTVAL.BB = BBENAMNING.B4 NO-LOCK BY KONSTVAL.KVALKOD.
      DO TRANSACTION:
         GET FIRST kq EXCLUSIVE-LOCK.
         IF AVAILABLE KONSTVAL THEN DO:
            IF KONSTVAL.ORDNING = 0 OR KONSTVAL.ORDNING = ? THEN DO:
               KONSTVAL.ORDNING = kalle.
               OPEN QUERY lq FOR EACH kbuff WHERE kbuff.KONSKOD = KONSTGRUPP.KONSKOD AND
               kbuff.BB = BBENAMNING.B4 AND kbuff.KVALKOD = KONSTVAL.KVALKOD NO-LOCK.            
               GET FIRST lq EXCLUSIVE-LOCK.
               IF AVAILABLE KBUFF THEN DO:
                  kbuff.ORDNING = kalle.
               END.            
               REPEAT:               
                  GET NEXT lq EXCLUSIVE-LOCK.
                  IF AVAILABLE kbuff THEN DO:
                     kbuff.ORDNING = kalle.
                  END.
                  ELSE LEAVE.              
               END.
               kalle = kalle + 1.
            END.
         END.
      END.
      REPEAT:
         DO TRANSACTION:
            GET NEXT kq EXCLUSIVE-LOCK.
            IF AVAILABLE KONSTVAL THEN DO:
               IF KONSTVAL.ORDNING = 0 OR KONSTVAL.ORDNING = ? THEN DO:
                  KONSTVAL.ORDNING = kalle.
                  OPEN QUERY lq FOR EACH kbuff WHERE kbuff.KONSKOD = KONSTGRUPP.KONSKOD AND
                  kbuff.BB = BBENAMNING.B4 AND kbuff.KVALKOD = KONSTVAL.KVALKOD NO-LOCK.               
                  GET FIRST lq EXCLUSIVE-LOCK.
                  IF AVAILABLE KBUFF THEN DO:
                     kbuff.ORDNING = kalle.
                  END.              
                  REPEAT:                  
                     GET NEXT lq EXCLUSIVE-LOCK.
                     IF AVAILABLE kbuff THEN DO:
                        kbuff.ORDNING = kalle.
                     END.
                     ELSE LEAVE.                  
                  END.
                  kalle = kalle + 1.
               END.
            END.
            ELSE LEAVE.
         END.
      END.
   END.
   IF BBENAMNING.B5 NE "" THEN DO:
      kalle = 1.
      OPEN QUERY kq FOR EACH KONSTVAL WHERE KONSTVAL.KONSKOD = KONSTGRUPP.KONSKOD AND
      KONSTVAL.BB = BBENAMNING.B5 NO-LOCK BY KONSTVAL.KVALKOD.
      DO TRANSACTION:
         GET FIRST kq EXCLUSIVE-LOCK.
         IF AVAILABLE KONSTVAL THEN DO:
            IF KONSTVAL.ORDNING = 0 OR KONSTVAL.ORDNING = ? THEN DO:
               KONSTVAL.ORDNING = kalle.
               OPEN QUERY lq FOR EACH kbuff WHERE kbuff.KONSKOD = KONSTGRUPP.KONSKOD AND
               kbuff.BB = BBENAMNING.B5 AND kbuff.KVALKOD = KONSTVAL.KVALKOD NO-LOCK.            
               GET FIRST lq EXCLUSIVE-LOCK.
               IF AVAILABLE KBUFF THEN DO:
                  kbuff.ORDNING = kalle.
               END.            
               REPEAT:               
                  GET NEXT lq EXCLUSIVE-LOCK.
                  IF AVAILABLE kbuff THEN DO:
                     kbuff.ORDNING = kalle.
                  END.
                  ELSE LEAVE.              
               END.
               kalle = kalle + 1.
            END.
         END.
      END.
      REPEAT:
         DO TRANSACTION:
            GET NEXT kq EXCLUSIVE-LOCK.
            IF AVAILABLE KONSTVAL THEN DO:
               IF KONSTVAL.ORDNING = 0 OR KONSTVAL.ORDNING = ? THEN DO:
                  KONSTVAL.ORDNING = kalle.
                  OPEN QUERY lq FOR EACH kbuff WHERE kbuff.KONSKOD = KONSTGRUPP.KONSKOD AND
                  kbuff.BB = BBENAMNING.B5 AND kbuff.KVALKOD = KONSTVAL.KVALKOD NO-LOCK.               
                  GET FIRST lq EXCLUSIVE-LOCK.
                  IF AVAILABLE KBUFF THEN DO:
                     kbuff.ORDNING = kalle.
                  END.              
                  REPEAT:                  
                     GET NEXT lq EXCLUSIVE-LOCK.
                     IF AVAILABLE kbuff THEN DO:
                        kbuff.ORDNING = kalle.
                     END.
                     ELSE LEAVE.                  
                  END.
                  kalle = kalle + 1.
               END.
            END.
            ELSE LEAVE.
         END.
      END.
   END.
   IF BBENAMNING.B6 NE "" THEN DO:
      kalle = 1.
      OPEN QUERY kq FOR EACH KONSTVAL WHERE KONSTVAL.KONSKOD = KONSTGRUPP.KONSKOD AND
      KONSTVAL.BB = BBENAMNING.B6 NO-LOCK BY KONSTVAL.KVALKOD.
      DO TRANSACTION:
         GET FIRST kq EXCLUSIVE-LOCK.
         IF AVAILABLE KONSTVAL THEN DO:
            IF KONSTVAL.ORDNING = 0 OR KONSTVAL.ORDNING = ? THEN DO:
               KONSTVAL.ORDNING = kalle.
               OPEN QUERY lq FOR EACH kbuff WHERE kbuff.KONSKOD = KONSTGRUPP.KONSKOD AND
               kbuff.BB = BBENAMNING.B6 AND kbuff.KVALKOD = KONSTVAL.KVALKOD NO-LOCK.            
               GET FIRST lq EXCLUSIVE-LOCK.
               IF AVAILABLE KBUFF THEN DO:
                  kbuff.ORDNING = kalle.
               END.            
               REPEAT:               
                  GET NEXT lq EXCLUSIVE-LOCK.
                  IF AVAILABLE kbuff THEN DO:
                     kbuff.ORDNING = kalle.
                  END.
                  ELSE LEAVE.              
               END.
               kalle = kalle + 1.
            END.
         END.
      END.
      REPEAT:
         DO TRANSACTION:
            GET NEXT kq EXCLUSIVE-LOCK.
            IF AVAILABLE KONSTVAL THEN DO:
               IF KONSTVAL.ORDNING = 0 OR KONSTVAL.ORDNING = ? THEN DO:
                  KONSTVAL.ORDNING = kalle.
                  OPEN QUERY lq FOR EACH kbuff WHERE kbuff.KONSKOD = KONSTGRUPP.KONSKOD AND
                  kbuff.BB = BBENAMNING.B6 AND kbuff.KVALKOD = KONSTVAL.KVALKOD NO-LOCK.               
                  GET FIRST lq EXCLUSIVE-LOCK.
                  IF AVAILABLE KBUFF THEN DO:
                     kbuff.ORDNING = kalle.
                  END.              
                  REPEAT:                  
                     GET NEXT lq EXCLUSIVE-LOCK.
                     IF AVAILABLE kbuff THEN DO:
                        kbuff.ORDNING = kalle.
                     END.
                     ELSE LEAVE.                  
                  END.
                  kalle = kalle + 1.
               END.
            END.
            ELSE LEAVE.
         END.
      END.
   END.
      END.
/*    GET NEXT gq NO-LOCK. */
/* END.                    */
