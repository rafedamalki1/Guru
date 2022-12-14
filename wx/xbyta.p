DEF VAR aonrvar LIKE AONRTAB.AONR NO-UNDO.
DEF TEMP-TABLE ptemp
FIELD PERSONALKOD LIKE PERSONALTAB.PERSONALKOD
FIELD KOD LIKE ANSTFORMTAB.KOD
INDEX PKOD IS PRIMARY PERSONALKOD.
OPEN QUERY stq FOR EACH SUMTID WHERE 
SUMTID.AONR = "130310" USE-INDEX AONR NO-LOCK.
GET FIRST stq NO-LOCK.
DO WHILE AVAILABLE(SUMTID):
   FIND FIRST ptemp where ptemp.PERSONALKOD = SUMTID.PERSONALKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ptemp THEN DO:
      CREATE ptemp.
      ptemp.PERSONALKOD = SUMTID.PERSONALKOD.
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = ptemp.PERSONALKOD 
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.      
      IF AVAILABLE PERSONALTAB THEN DO:
         FIND FIRST ANSTFORMTAB WHERE
         ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
         USE-INDEX ANSTF NO-LOCK NO-ERROR.
         ptemp.KOD = ANSTFORMTAB.KOD.
      END.
      ELSE DO:
         DISPLAY ptemp.PERSONALKOD WITH FRAME CC DOWN.
         UPDATE ptemp.KOD WITH FRAME CC DOWN.
         DOWN 1 WITH FRAME CC.         
      END.
   END.
   GET NEXT stq NO-LOCK.
END. 
OPEN QUERY persq FOR EACH ptemp NO-LOCK.
GET FIRST persq NO-LOCK.
DO WHILE AVAILABLE(ptemp):
   IF ptemp.KOD = "E" OR ptemp.KOD = "K" THEN aonrvar = "130300".   
   ELSE aonrvar = "130302".
   OPEN QUERY stq FOR EACH SUMTID WHERE SUMTID.PERSONALKOD = ptemp.PERSONALKOD AND
   SUMTID.AONR = "130310" USE-INDEX PERSONALKOD NO-LOCK.
   DO TRANSACTION:
      GET FIRST stq EXCLUSIVE-LOCK.
      IF AVAILABLE SUMTID THEN DO:
         ASSIGN SUMTID.AONR = aonrvar.         
      END. 
   END.   
   DO WHILE AVAILABLE(SUMTID):
      DO TRANSACTION:
        GET NEXT stq EXCLUSIVE-LOCK.
         IF AVAILABLE SUMTID THEN DO:
            ASSIGN SUMTID.AONR = aonrvar.         
         END.
      END.
   END.      
   OPEN QUERY st2q FOR EACH SUMTIDDAG WHERE SUMTIDDAG.PERSONALKOD = ptemp.PERSONALKOD AND
   SUMTIDDAG.AONR = "130310" USE-INDEX PERSONALKOD NO-LOCK.
   DO TRANSACTION:
      GET FIRST st2q EXCLUSIVE-LOCK.
      IF AVAILABLE SUMTIDDAG THEN DO:
         ASSIGN SUMTIDDAG.AONR = aonrvar.         
      END. 
   END.   
   DO WHILE AVAILABLE(SUMTIDDAG):
      DO TRANSACTION:
        GET NEXT st2q EXCLUSIVE-LOCK.
         IF AVAILABLE SUMTIDDAG THEN DO:
            ASSIGN SUMTIDDAG.AONR = aonrvar.         
         END.
      END.
   END.
   GET NEXT persq NO-LOCK.
END.   
