
/* FLYTTAR EBR KATALOG FRÅN ETT KATALOGÅR            */
/* TILL ETT ANNAT. ANVÄNDS FÖR GRANINGE              */

DEFINE BUFFER p1buff FOR P1.
DEFINE BUFFER p2buff FOR P2.
DEFINE BUFFER p3buff FOR P3.
DEFINE BUFFER p5buff FOR P5.
DEFINE BUFFER lop1buff FOR LOP1.
DEFINE BUFFER lop2buff FOR LOP2.
DEFINE BUFFER lop3buff FOR LOP3.
DEFINE BUFFER lop5buff FOR LOP5.
DEFINE BUFFER kalkbuff FOR KALKBEF.
DEFINE BUFFER ebrbuff FOR EBRPRIS.
DEFINE BUFFER sebrbuff FOR sEBRPRIS.
DEFINE BUFFER frekbuff FOR FREKVENS.

DEFINE VARIABLE arvar LIKE P1.KATAR NO-UNDO.
DEFINE VARIABLE arvar2 LIKE P1.KATAR NO-UNDO.
DEFINE VARIABLE region AS LOGICAL NO-UNDO.

   ASSIGN        
   arvar = 2016 
   arvar2 = 2017.
 
   OPEN QUERY kq FOR EACH P1 WHERE P1.KATAR = arvar
   NO-LOCK.
   GET FIRST kq NO-LOCK.
   DO WHILE AVAILABLE(P1):
      IF SUBSTRING(P1.ARBKOD,2,1) = " " THEN.
      ELSE DO:
         DO TRANSACTION:
            CREATE p1buff.
            BUFFER-COPY P1 TO p1buff.
            ASSIGN
            p1buff.KATAR = arvar2.
         END.
      END.   
      GET NEXT kq NO-LOCK.
   END.
   CLOSE QUERY kq.
   
   OPEN QUERY kq2 FOR EACH LOP1 WHERE LOP1.KATAR = arvar
   NO-LOCK.
   GET FIRST kq2 NO-LOCK.
   DO WHILE AVAILABLE(LOP1):
      IF SUBSTRING(LOP1.ARBKOD,2,1) = " " THEN.
      ELSE DO:
         DO TRANSACTION:
            CREATE lop1buff.
            BUFFER-COPY LOP1 TO lop1buff.
            ASSIGN
            lop1buff.KATAR = arvar2.         
         END.         
      END.   
      GET NEXT kq2 NO-LOCK.
   END.   
   CLOSE QUERY kq2.

   OPEN QUERY kq3 FOR EACH P2 WHERE P2.KATAR = arvar
   NO-LOCK.
   GET FIRST kq3 NO-LOCK.
   DO WHILE AVAILABLE(P2):
      IF SUBSTRING(P2.ARBKOD,1,1) = " " THEN.
      ELSE DO:
         DO TRANSACTION:
            CREATE P2buff.
            BUFFER-COPY P2 TO P2buff.
            ASSIGN
            P2buff.KATAR = arvar2.
         END.
      END.   
      GET NEXT kq3 NO-LOCK.
   END.
   CLOSE QUERY kq3.
   OPEN QUERY kq4 FOR EACH LOP2 WHERE LOP2.KATAR = arvar
   NO-LOCK.
   GET FIRST kq4 NO-LOCK.
   DO WHILE AVAILABLE(LOP2):
      IF SUBSTRING(LOP2.ARBKOD,1,1) = " " THEN.
      ELSE DO:
         DO TRANSACTION:
            CREATE loP2buff.
            BUFFER-COPY LOP2 TO loP2buff.
            ASSIGN
            loP2buff.KATAR = arvar2.         
         END.
      END.
      GET NEXT kq4 NO-LOCK.
   END.
   CLOSE QUERY kq4.

   OPEN QUERY kq5 FOR EACH P3 WHERE P3.KATAR = arvar
   NO-LOCK.
   GET FIRST kq5 NO-LOCK.
   DO WHILE AVAILABLE(P3):
      region = FALSE.
      IF SUBSTRING(P3.ARBKOD,1,1) <= "9" THEN DO:
         IF INTEGER(P3.ARBKOD) < 800 THEN region = TRUE.      
      END.
      IF SUBSTRING(P3.ARBKOD,1,1) = "R" THEN region = TRUE.
      IF region = FALSE THEN DO:
         DO TRANSACTION:
            CREATE P3buff.
            BUFFER-COPY P3 TO P3buff.
            ASSIGN
            P3buff.KATAR = arvar2.
         END.
      END.   
      GET NEXT kq5 NO-LOCK.
   END.
   CLOSE QUERY kq5.
   OPEN QUERY kq6 FOR EACH LOP3 WHERE LOP3.KATAR = arvar
   NO-LOCK.
   GET FIRST kq6 NO-LOCK.
   DO WHILE AVAILABLE(LOP3):
      region = FALSE.
      IF SUBSTRING(LOP3.ARBKOD,1,1) <= "9" THEN DO:
         IF INTEGER(LOP3.ARBKOD) < 800 THEN region = TRUE.      
      END.
      IF SUBSTRING(LOP3.ARBKOD,1,1) = "R" THEN region = TRUE.
      IF region = FALSE THEN DO:
         DO TRANSACTION:
            CREATE loP3buff.
            BUFFER-COPY LOP3 TO loP3buff.
            ASSIGN
            loP3buff.KATAR = arvar2.
            
         END.
      END.   
      GET NEXT kq6 NO-LOCK.
   END.
   CLOSE QUERY kq6.


   FIND FIRST EBRPRIS WHERE EBRPRIS.ARTAL = arvar NO-LOCK NO-ERROR.
   DO TRANSACTION:
      CREATE ebrbuff.
      BUFFER-COPY EBRPRIS TO ebrbuff.
      ASSIGN
      ebrbuff.ARTAL = arvar2.
   END.

   FIND FIRST KALKBEF WHERE KALKBEF.KATAR = arvar AND KALKBEF.KALKNR = 0
   NO-LOCK NO-ERROR.
   DO TRANSACTION:
      CREATE kalkbuff.
      BUFFER-COPY KALKBEF TO kalkbuff.
      ASSIGN
      kalkbuff.KATAR = arvar2.
  END.                                                                 

   OPEN QUERY kq7 FOR EACH FREKVENS WHERE FREKVENS.KATAR = arvar
   NO-LOCK.
   GET FIRST kq7 NO-LOCK.
   DO WHILE AVAILABLE(FREKVENS):      
      IF SUBSTRING(FREKVENS.ARBKOD,2,1) = " " THEN.
      ELSE IF SUBSTRING(FREKVENS.ARBKOD,1,1) = " " THEN.
      ELSE IF SUBSTRING(FREKVENS.ARBKOD,1,1) = "R" THEN.         
      ELSE IF SUBSTRING(FREKVENS.ARBKOD,1,1) <= "9" AND INTEGER(FREKVENS.ARBKOD) > 500 AND INTEGER(FREKVENS.ARBKOD) < 800 THEN.
      ELSE DO:            
         DO TRANSACTION:
            CREATE frekbuff.
            BUFFER-COPY FREKVENS TO frekbuff.
            ASSIGN
            frekbuff.KATAR = arvar2.
            
         END.
      END.
      GET NEXT kq7 NO-LOCK.   
   END.
   CLOSE QUERY kq7.

   FIND FIRST SEBRPRIS WHERE SEBRPRIS.ARTAL = arvar NO-LOCK NO-ERROR.
   IF AVAILABLE SEBRPRIS THEN DO:   
      DO TRANSACTION:
         CREATE sebrbuff.
         BUFFER-COPY SEBRPRIS TO sebrbuff.
         ASSIGN
         sebrbuff.ARTAL = arvar2.
      END.
   END.
   
   OPEN QUERY kq8 FOR EACH P5 WHERE P5.KATAR = arvar
   NO-LOCK.
   GET FIRST kq8 NO-LOCK.
   DO WHILE AVAILABLE(P5):
      DO TRANSACTION:
         CREATE P5buff.
         BUFFER-COPY P5 TO P5buff.
         ASSIGN
         P5buff.KATAR = arvar2.
      END.
      GET NEXT kq8 NO-LOCK.
   END.
   CLOSE QUERY kq8.
   OPEN QUERY kq9 FOR EACH LOP5 WHERE LOP5.KATAR = arvar
   NO-LOCK.
   GET FIRST kq9 NO-LOCK.
   DO WHILE AVAILABLE(LOP5):
      DO TRANSACTION:
         CREATE loP5buff.
         BUFFER-COPY LOP5 TO loP5buff.
         ASSIGN
         loP5buff.KATAR = arvar2.
         GET NEXT kq9 NO-LOCK.
      END.
   END.
   CLOSE QUERY kq9.
