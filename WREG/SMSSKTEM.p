/*SMSSKTEM.P*/
   
DEFINE SHARED TEMP-TABLE smsaonr
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD UTRYCKNING AS LOGICAL
   FIELD PRISTYP AS CHARACTER
   FIELD TRAKTAMENTE AS INTEGER
   FIELD FORETAG AS CHARACTER
   INDEX AONR IS PRIMARY AONR DELNR FORETAG
   INDEX FORE FORETAG AONR DELNR.
   
DEFINE SHARED TEMP-TABLE smspersonal
   FIELD PERSONALKOD AS CHARACTER  
   FIELD ANSTALLNING AS CHARACTER
   FIELD OVERTIDUTTAG AS CHARACTER
   FIELD PREC AS RECID 
   FIELD FORETAG AS CHARACTER
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD FORETAG
   INDEX FORE FORETAG PERSONALKOD.

   
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
{AMERICANEUROPEAN.I}
OUTPUT TO "c:\temp\niklas.txt" APPEND.
   PUT "4" SKIP.
   OUTPUT CLOSE.
      OPEN QUERY smspq FOR EACH smspersonal WHERE smspersonal.FORETAG = FORETAG.FORETAG 
USE-INDEX FORE NO-LOCK.
DO TRANSACTION:
   GET FIRST smspq EXCLUSIVE-LOCK.
   IF AVAILABLE smspersonal THEN DELETE smspersonal.           
END.
REPEAT:   
   DO TRANSACTION:      
      GET NEXT smspq EXCLUSIVE-LOCK.             
      IF AVAILABLE smspersonal THEN DELETE smspersonal.
      ELSE LEAVE.           
   END.      
END.
OUTPUT TO "c:\temp\niklas.txt" APPEND.
   PUT "5" SKIP.
   OUTPUT CLOSE.
OPEN QUERY smsaq FOR EACH smsaonr WHERE smsaonr.FORETAG = FORETAG.FORETAG 
USE-INDEX FORE NO-LOCK.
DO TRANSACTION:
   GET FIRST smsaq EXCLUSIVE-LOCK.
   IF AVAILABLE smsaonr THEN DELETE smsaonr.           
END.
REPEAT:   
   DO TRANSACTION:      
      GET NEXT smsaq EXCLUSIVE-LOCK.             
      IF AVAILABLE smsaonr THEN DELETE smsaonr.
      ELSE LEAVE.           
   END.      
END.
OUTPUT TO "c:\temp\niklas.txt" APPEND.
   PUT "6" SKIP.
   OUTPUT CLOSE.
OPEN QUERY persq FOR EACH PERSONALTAB WHERE PERSONALTAB.AKTIV = TRUE 
USE-INDEX PERSONALKOD NO-LOCK.
GET FIRST persq NO-LOCK.
DO WHILE AVAILABLE(PERSONALTAB):                      
   CREATE smspersonal.
   ASSIGN          
   smspersonal.PERSONALKOD = PERSONALTAB.PERSONALKOD
   smspersonal.ANSTALLNING = PERSONALTAB.ANSTALLNING
   smspersonal.OVERTIDUTTAG = PERSONALTAB.OVERTIDUTTAG
   smspersonal.PREC = RECID(PERSONALTAB)
   smspersonal.FORETAG = FORETAG.FORETAG.      
   GET NEXT persq NO-LOCK.
END.    
OUTPUT TO "c:\temp\niklas.txt" APPEND.
   PUT "7" SKIP.
   OUTPUT CLOSE.
OPEN QUERY aonrq FOR EACH AONRTAB WHERE AONRTAB.AONRAVDATUM = 01/01/91 
USE-INDEX AONR NO-LOCK.
GET FIRST aonrq NO-LOCK.
DO WHILE AVAILABLE(AONRTAB):                      
   CREATE smsaonr.
   ASSIGN  
   smsaonr.AONR = AONRTAB.AONR
   smsaonr.DELNR = AONRTAB.DELNR 
   smsaonr.UTRYCKNING = AONRTAB.UTRYCKNING
   smsaonr.PRISTYP = AONRTAB.PRISTYP
   smsaonr.TRAKTAMENTE = AONRTAB.TRAKTAMENTE      
   smsaonr.FORETAG = FORETAG.FORETAG.
   GET NEXT aonrq NO-LOCK.
END.
OUTPUT TO "c:\temp\niklas.txt" APPEND.
   PUT "8" SKIP.
   OUTPUT CLOSE.
DO TRANSACTION:
   FIND FIRST FORETAG USE-INDEX FORETAG EXCLUSIVE-LOCK NO-ERROR.
   ASSIGN FORETAG.PROGRAM = STRING(TODAY) + " SMSSTYR " + STRING(TIME,"HH:MM").
END.  
{EUROPEANAMERICAN.I}
  