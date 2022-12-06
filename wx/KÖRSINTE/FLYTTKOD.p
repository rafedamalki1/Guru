/* FLYTTAR EN ARBETSKOD MED LÖPNR FRÅN ETT KATALOGÅR */
/* TILL ETT ANNAT.                                   */

DEFINE BUFFER p1buff FOR P1.
DEFINE BUFFER p2buff FOR P2.
DEFINE BUFFER p3buff FOR P3.
DEFINE BUFFER lop1buff FOR LOP1.
DEFINE BUFFER lop2buff FOR LOP2.
DEFINE BUFFER lop3buff FOR LOP3.
DEFINE VARIABLE korr AS DECIMAL NO-UNDO.
DEFINE VARIABLE delningsvar AS INTEGER NO-UNDO. /*sätts till 1 om F9 värde = 0 och 100 om F9 värde = 1*/
DEFINE INPUT PARAMETER typvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER arvar LIKE P1.KATAR NO-UNDO.
DEFINE INPUT PARAMETER arvar2 LIKE P1.KATAR NO-UNDO.
DEFINE INPUT PARAMETER arbvar AS CHARACTER NO-UNDO.

/* ASSIGN        */
/* arvar = 2002  */
/* arvar2 = 2003 */
/* typvar = 2    */
/* arbvar = "".  */

IF typvar = 1 THEN DO:
   OPEN QUERY kq FOR EACH P1 WHERE P1.KATAR = arvar AND 
   P1.ARBKOD = arbvar NO-LOCK.
   GET FIRST kq NO-LOCK.
   DO WHILE AVAILABLE(P1):
      DO TRANSACTION:      
         CREATE p1buff.
         BUFFER-COPY P1 TO p1buff.
         ASSIGN         
         p1buff.KATAR = arvar2.
         OPEN QUERY kq4 FOR EACH LOP1 WHERE LOP1.KATAR = arvar AND
         LOP1.ARBKOD = P1.ARBKOD NO-LOCK.
         GET FIRST kq4 NO-LOCK.
         DO WHILE AVAILABLE(LOP1):
            CREATE lop1buff.
            BUFFER-COPY LOP1 TO lop1buff.
            ASSIGN         
            lop1buff.KATAR = arvar2.            
            GET NEXT kq4 NO-LOCK.
         END.
         CLOSE QUERY kq4.
      END.
      GET NEXT kq NO-LOCK.
   END.
   CLOSE QUERY kq.
END.
ELSE IF typvar = 2 THEN DO:
    
   FIND FIRST ebrpris WHERE ebrpris.artal = arvar2 NO-LOCK NO-ERROR.
   IF arbvar = "SKJ" OR arbvar = "SKL" OR arbvar = "EJK" OR arbvar = "ELL" OR arbvar = "EUH" OR arbvar = "EAF" OR arbvar = "VND" THEN DO:      
      OPEN QUERY kq1 FOR EACH P2 WHERE P2.KATAR = arvar AND 
      P2.ARBKOD = arbvar NO-LOCK.
      GET FIRST kq1 NO-LOCK.
      DO WHILE AVAILABLE(P2):
         DO TRANSACTION:      
            CREATE p2buff.
            BUFFER-COPY P2 TO p2buff.
            ASSIGN                  
            p2buff.KATAR = arvar2.
            OPEN QUERY kq5 FOR EACH LOP2 WHERE LOP2.KATAR = arvar AND
            LOP2.ARBKOD = P2.ARBKOD NO-LOCK.
            GET FIRST kq5 NO-LOCK.
            DO WHILE AVAILABLE(LOP2):
               /* behåll det gamla montörspriset på skl och skj*/
               /* behåll det gamla montörspriset på EJK ELL EU*/
               IF LOP2.F9 = 0 THEN delningsvar = 1.
               ELSE delningsvar = 100.
               korr = (ebrpris.mont * lop2.f1  / delningsvar) + (ebrpris.mont * lop2.f2 / delningsvar) - lop2.arbete.
               CREATE lop2buff.
               BUFFER-COPY LOP2 TO lop2buff.
               lop2buff.arbete = (ebrpris.mont * lop2.f1 / delningsvar) + (ebrpris.mont * lop2.f2 / delningsvar).       
               lop2buff.KATAR = arvar2.                                       
               IF korr LE lop2buff.MATERIEL THEN DO:
                  lop2buff.MATERIEL = lop2buff.MATERIEL - korr.                   
               END.
               ELSE DO:                  
                  lop2buff.UTRUSTKOST = lop2buff.UTRUSTKOST - korr + lop2buff.MATERIEL.
                  lop2buff.MATERIEL = 0.
                  IF lop2buff.UTRUSTKOST NE 0 THEN lop2buff.FAST = TRUE.
               END.
               GET NEXT kq5 NO-LOCK.
            END.
            CLOSE QUERY kq5.
         END.
         GET NEXT kq1 NO-LOCK.
      END.
      CLOSE QUERY kq1.
   END.
   ELSE DO:         
      OPEN QUERY kq1 FOR EACH P2 WHERE P2.KATAR = arvar AND 
      P2.ARBKOD = arbvar NO-LOCK.
      GET FIRST kq1 NO-LOCK.
      DO WHILE AVAILABLE(P2):
         DO TRANSACTION:      
            CREATE p2buff.
            BUFFER-COPY P2 TO p2buff.
            ASSIGN                  
            p2buff.KATAR = arvar2.
            OPEN QUERY kq5 FOR EACH LOP2 WHERE LOP2.KATAR = arvar AND
            LOP2.ARBKOD = P2.ARBKOD NO-LOCK.
            GET FIRST kq5 NO-LOCK.
            DO WHILE AVAILABLE(LOP2):
               IF LOP2.F9 = 0 THEN delningsvar = 1.
               ELSE delningsvar = 100.
               CREATE lop2buff.
               BUFFER-COPY LOP2 TO lop2buff.
               ASSIGN         
               lop2buff.arbete = (ebrpris.mont * lop2.f1 / delningsvar ) + (ebrpris.mont * lop2.f2 / delningsvar ).       
               lop2buff.KATAR = arvar2.                        
               GET NEXT kq5 NO-LOCK.
            END.
            CLOSE QUERY kq5.
         END.
         GET NEXT kq1 NO-LOCK.
      END.
      CLOSE QUERY kq1.
   END.
END.
ELSE DO:
   OPEN QUERY kq2 FOR EACH P3 WHERE P3.KATAR = arvar AND 
   P3.ARBKOD = arbvar NO-LOCK.
   GET FIRST kq2 NO-LOCK.
   DO WHILE AVAILABLE(P3):
      DO TRANSACTION:      
         CREATE p3buff.
         BUFFER-COPY P3 TO p3buff.
         ASSIGN         
         p3buff.KATAR = arvar2.                     
         OPEN QUERY kq6 FOR EACH LOP3 WHERE LOP3.KATAR = arvar AND
         LOP3.ARBKOD = P3.ARBKOD NO-LOCK.
         GET FIRST kq6 NO-LOCK.
         DO WHILE AVAILABLE(LOP3):
            CREATE lop3buff.
            BUFFER-COPY LOP3 TO lop3buff.
            ASSIGN         
            lop3buff.KATAR = arvar2.                        
            GET NEXT kq6 NO-LOCK.
         END.
         CLOSE QUERY kq6.
      END.
      GET NEXT kq2 NO-LOCK.
   END.
   CLOSE QUERY kq2.
END.


