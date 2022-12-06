/*esrensaanv.p*/
DEFINE VARIABLE vem AS CHARACTER NO-UNDO.
DEFINE VARIABLE pkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.

DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}

DEFINE BUFFER persbuff FOR PERSONALTAB.

DEFINE TEMP-TABLE tidinah
   FIELD ANVANDARE          AS CHARACTER
   FIELD NAMN               AS CHARACTER       
   FIELD PERSONALKOD        AS CHARACTER       
   FIELD EXET               AS CHARACTER.       

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.

DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .

DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.   
  
{muswait.i} 

ASSIGN
/*filnamn = "\\server04\d\elpool\elpnj\esgraninge\ANV�NDARE1.skv".*/
filnamn = "/u01/guru/wrk/ANV�NDARE1.skv".
EMPTY TEMP-TABLE intid NO-ERROR.
EMPTY TEMP-TABLE tidinah NO-ERROR.

INPUT FROM VALUE(filnamn) NO-ECHO.
REPEAT:
   DO TRANSACTION: 
      CREATE tidinah.
      ASSIGN.
      IMPORT DELIMITER ";" tidinah   NO-ERROR.
   END.               
END.   
FOR EACH tidinah WHERE tidinah.EXET = "X":
   ASSIGN
   vem = tidinah.ANVANDARE
   pkod = tidinah.PERSONALKOD. 
   FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = vem NO-LOCK NO-ERROR.
   DEFINE QUERY persekq FOR PERSEK.
   OPEN QUERY persekq FOR EACH PERSEK WHERE PERSEK.ANVANDARE = ANVANDARE.ANVANDARE
   USE-INDEX PERSEK NO-LOCK. 
   DO TRANSACTION:       
      GET FIRST persekq EXCLUSIVE-LOCK.
      IF AVAILABLE PERSEK THEN DELETE PERSEK.    
   END.
   REPEAT:  
      DO TRANSACTION:
         GET NEXT persekq EXCLUSIVE-LOCK.
         IF AVAILABLE PERSEK THEN DELETE PERSEK.    
         ELSE LEAVE.      
      END.         
   END.   
   DEFINE QUERY tidsekq FOR TIDSEK.
   OPEN QUERY tidsekq FOR EACH TIDSEK WHERE TIDSEK.ANVANDARE = ANVANDARE.ANVANDARE
   USE-INDEX TIDSEK NO-LOCK. 
   DO TRANSACTION:       
      GET FIRST tidsekq EXCLUSIVE-LOCK.
      IF AVAILABLE TIDSEK THEN DELETE TIDSEK.    
   END.
   REPEAT:  
      DO TRANSACTION:
         GET NEXT tidsekq EXCLUSIVE-LOCK.         
         IF AVAILABLE TIDSEK THEN DELETE TIDSEK.    
         ELSE LEAVE.      
      END.         
   END.
   OPEN QUERY omrsekq FOR EACH OMRSEK WHERE OMRSEK.ANVANDARE = ANVANDARE.ANVANDARE
   NO-LOCK. 
   DO TRANSACTION:       
      GET FIRST omrsekq EXCLUSIVE-LOCK.
      IF AVAILABLE OMRSEK THEN DELETE OMRSEK.    
   END.
   REPEAT:  
      DO TRANSACTION:
         GET NEXT omrsekq EXCLUSIVE-LOCK.         
         IF AVAILABLE OMRSEK THEN DELETE OMRSEK.    
         ELSE LEAVE.      
      END.         
   END.
   OPEN QUERY bsekq FOR EACH BOLAGSEK WHERE BOLAGSEK.ANVANDARE = ANVANDARE.ANVANDARE
   NO-LOCK. 
   DO TRANSACTION:       
      GET FIRST bsekq EXCLUSIVE-LOCK.
      IF AVAILABLE BOLAGSEK THEN DELETE BOLAGSEK.    
   END.
   REPEAT:  
      DO TRANSACTION:
         GET NEXT bsekq EXCLUSIVE-LOCK.         
         IF AVAILABLE BOLAGSEK THEN DELETE BOLAGSEK.    
         ELSE LEAVE.      
      END.         
   END.                                                                         
   DO TRANSACTION:
      FIND CURRENT ANVANDARE EXCLUSIVE-LOCK NO-ERROR.
      DELETE ANVANDARE.      
   END.
   RUN bort_UI.
END.

PROCEDURE bort_UI:
   DO TRANSACTION:
      FIND FIRST ANVANDARE WHERE ANVANDARE.PERSONALKOD = pkod EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE ANVANDARE THEN DO:
         ANVANDARE.PERSONALKOD = "".
      END.
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod EXCLUSIVE-LOCK NO-ERROR.  
      OPEN QUERY timq
      FOR EACH TIMKOSTNADSTAB WHERE TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
      USE-INDEX PRISPERS NO-LOCK.
      GET FIRST timq EXCLUSIVE-LOCK.
      DO WHILE AVAILABLE(TIMKOSTNADSTAB):    
         DELETE TIMKOSTNADSTAB.
         GET NEXT timq EXCLUSIVE-LOCK. 
      END.                    
      CLOSE QUERY timq.
      OPEN QUERY ptimq
      FOR EACH PERSONALPRIS WHERE PERSONALPRIS.PERSONALKOD = PERSONALTAB.PERSONALKOD
      NO-LOCK.
      GET FIRST ptimq EXCLUSIVE-LOCK.
      DO WHILE AVAILABLE(PERSONALPRIS):    
         DELETE PERSONALPRIS.
         GET NEXT ptimq EXCLUSIVE-LOCK. 
      END.                    
      CLOSE QUERY ptimq.
      OPEN QUERY psekq
      FOR EACH PERSEK WHERE PERSEK.PERSONALKOD = PERSONALTAB.PERSONALKOD 
      USE-INDEX PERSONALKOD NO-LOCK.
      GET FIRST psekq EXCLUSIVE-LOCK.
      DO WHILE AVAILABLE(PERSEK):     
         DELETE PERSEK.
         GET NEXT psekq EXCLUSIVE-LOCK. 
      END.   
      CLOSE QUERY psekq.
      OPEN QUERY tsekq
      FOR EACH TIDSEK WHERE TIDSEK.PERSONALKOD = PERSONALTAB.PERSONALKOD 
      USE-INDEX PERSONALKOD NO-LOCK.
      GET FIRST tsekq EXCLUSIVE-LOCK.
      DO WHILE AVAILABLE(TIDSEK):         
         DELETE TIDSEK. 
         GET NEXT tsekq EXCLUSIVE-LOCK.                     
      END.   
      CLOSE QUERY tsekq.                                 
      FIND FIRST persbuff WHERE persbuff.ANSVARIGTIDR = PERSONALTAB.ANSVARIGTIDR AND
      RECID(persbuff) NE RECID(PERSONALTAB)
      USE-INDEX ANSVT NO-LOCK NO-ERROR.                                                          
      IF NOT AVAILABLE persbuff THEN DO:
         FIND FIRST ANSVARIGTAB WHERE ANSVARIGTAB.PERSONALKOD = PERSONALTAB.ANSVARIGTIDR
         USE-INDEX ANSVG EXCLUSIVE-LOCK NO-ERROR.     
         IF AVAILABLE ANSVARIGTAB THEN DO:
            DELETE ANSVARIGTAB.
         END. 
      END.      
      FIND FIRST persbuff WHERE persbuff.TIDSGODK = PERSONALTAB.TIDSGODK AND
      RECID(persbuff) NE RECID(PERSONALTAB)
      USE-INDEX GODK NO-LOCK NO-ERROR.           
      IF NOT AVAILABLE persbuff THEN DO:
         FIND FIRST GODKANNARTAB WHERE GODKANNARTAB.PERSONALKOD = PERSONALTAB.TIDSGODK 
         USE-INDEX GODK EXCLUSIVE-LOCK NO-ERROR.     
         IF AVAILABLE GODKANNARTAB THEN DO:
            DELETE GODKANNARTAB.
         END.
      END.   
      FIND FIRST BEREDAONR WHERE BEREDAONR.PERSONALKOD = PERSONALTAB.PERSONALKOD 
      USE-INDEX PERSONALKOD EXCLUSIVE-LOCK NO-ERROR. 
      IF AVAILABLE BEREDAONR THEN DO:
         DELETE BEREDAONR.
      END.   
      FIND FIRST ANSVAONR WHERE ANSVAONR.PERSONALKOD = PERSONALTAB.PERSONALKOD 
      USE-INDEX PERSONALKOD EXCLUSIVE-LOCK NO-ERROR.   
      IF AVAILABLE ANSVAONR THEN DO:
         DELETE ANSVAONR.
      END.
      FIND FIRST EXTRADATA WHERE EXTRADATA.HUVUDCH = PERSONALTAB.PERSONALKOD
      AND EXTRADATA.PROGRAM = "OTBEORD" USE-INDEX HCH EXCLUSIVE-LOCK NO-ERROR.   
      IF AVAILABLE EXTRADATA THEN DO:
         DELETE EXTRADATA.
      END.
      OPEN QUERY ppq FOR EACH PERSONALPROJEKT WHERE 
      PERSONALPROJEKT.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK.
      GET FIRST ppq EXCLUSIVE-LOCK.
      DO WHILE AVAILABLE(PERSONALPROJEKT):
         DELETE PERSONALPROJEKT.
         GET NEXT ppq EXCLUSIVE-LOCK.
      END.   
      CREATE BORTPERS.
      ASSIGN
      BORTPERS.ANSTALLNING = PERSONALTAB.ANSTALLNING   
      BORTPERS.PERSONALKOD = PERSONALTAB.PERSONALKOD 
      BORTPERS.PERSONNUMMER = PERSONALTAB.PERSONNUMMER
      BORTPERS.FORNAMN = PERSONALTAB.FORNAMN
      BORTPERS.EFTERNAMN = PERSONALTAB.EFTERNAMN
      BORTPERS.OMRADE = PERSONALTAB.OMRADE
      BORTPERS.PERSMASK = PERSONALTAB.PERSMASK
      BORTPERS.DATUM = TODAY.           
      DELETE PERSONALTAB.  
   END.
   RUN bortfaktor (INPUT pkod).
END PROCEDURE.

PROCEDURE bortfaktor:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "PFAKTOR"                   
   inextradatatemp.HUVUDCH = pkod.
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod NO-LOCK NO-ERROR.
   IF NOT AVAILABLE PERSONALTAB THEN DO:
      RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).        
   END.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END PROCEDURE.
