OUTPUT TO PRINTER.
OPEN QUERY pq FOR EACH PERSONALTAB WHERE PERSONALTAB.PERSMASK = TRUE
USE-INDEX PERSONALKOD NO-LOCK.
GET FIRST pq NO-LOCK.
DO WHILE AVAILABLE(PERSONALTAB):
   FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.
   OPEN QUERY TQ FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
   AND VECKOKORD = "" AND TIDREGITAB.TIDLOG = TRUE USE-INDEX PKOD NO-LOCK.
   GET FIRST Tq NO-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB):
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = TIDREGITAB.AONR AND 
      AONRTAB.DELNR = TIDREGITAB.DELNR NO-LOCK NO-ERROR.
      IF AVAILABLE AONRTAB THEN DO:
         IF SUBSTRING(STRING(AONRTAB.ELVOMRKOD,"999999999"),1,1) = "1" THEN DO:
            IF ANSTFORMTAB.KOD BEGINS "T" THEN DO:
               PUT "Endast koll" PERSONALTAB.PERSONALKOD TIDREGITAB.DATUM TIDREGITAB.AONR 
               TIDREGITAB.DELNR.
               put SKIP.             
            END.   
         END.
         ELSE IF SUBSTRING(STRING(AONRTAB.ELVOMRKOD,"999999999"),1,1) = "2"  THEN DO:
            IF ANSTFORMTAB.KOD BEGINS "K" THEN DO:
                PUT "Endast tj�" PERSONALTAB.PERSONALKOD TIDREGITAB.DATUM TIDREGITAB.AONR 
                TIDREGITAB.DELNR.
                put skip.
            END.   
         END. 
         ELSE IF SUBSTRING(STRING(AONRTAB.ELVOMRKOD,"999999999"),1,1) = "3"  THEN DO:
            PUT "Ingen tid" PERSONALTAB.PERSONALKOD TIDREGITAB.DATUM TIDREGITAB.AONR 
            TIDREGITAB.DELNR.
            put skip.   
         END.
      END.
      GET NEXT TQ NO-LOCK.
   END.
   GET NEXT PQ NO-LOCK.
END.                