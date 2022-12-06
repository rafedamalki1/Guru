/*TIDHMT.P*/
&Scoped-define NEW NEW
{TIDALLT.I}
DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER brwbdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER brwavdatum AS DATE NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR overtemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tidallt.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod NO-LOCK NO-ERROR.
FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
USE-INDEX ANSTF NO-LOCK NO-ERROR.
RUN vad_UI.
RUN bef_UI.
Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
Guru.GlobalaVariabler:GDPRtyp = "TI".
{GDPRLOGGCLIENT.I}
PROCEDURE bef_UI:
   OPEN QUERY tbq FOR EACH tidallt,
   EACH BEFATTNING WHERE BEFATTNING.BEFATTNING = tidallt.OVERTIDTILL NO-LOCK.
   GET FIRST tbq NO-LOCK.
   DO WHILE AVAILABLE(BEFATTNING):
      tidallt.VIBEFATTNING = BEFATTNING.NAMN.
      GET NEXT tbq NO-LOCK.
   END.
END PROCEDURE.
PROCEDURE vad_UI:
   IF vadgora = 1 THEN DO:
      /*HÄMTAR ÖVERTID EFTER ÄNDRING*/
      EMPTY TEMP-TABLE overtemp NO-ERROR.       
      OPEN QUERY oq FOR EACH TIDREGITAB WHERE 
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM >= brwbdatum AND TIDREGITAB.DATUM <= brwavdatum AND 
      TIDREGITAB.OKOD1 NE "" USE-INDEX PSTART NO-LOCK.
      GET FIRST oq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):
         RUN over_UI.
         GET NEXT oq NO-LOCK.
      END. 
   END.
   IF vadgora = 2 OR vadgora = 4 THEN DO:
      /*FÖRST GÅNGEN*/
      EMPTY TEMP-TABLE tidallt NO-ERROR. 
      EMPTY TEMP-TABLE overtemp NO-ERROR.       
      OPEN QUERY tq FOR EACH TIDREGITAB WHERE 
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      YEAR(TIDREGITAB.DATUM) = YEAR(brwbdatum) AND
      MONTH(TIDREGITAB.DATUM) = MONTH(brwbdatum). 
      GET FIRST tq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):         
         musz = FALSE.
         IF vadgora = 4 THEN DO:
            IF TIDREGITAB.VECKOKORD = "" THEN musz = TRUE.
         END.
         IF musz = FALSE THEN DO:
            IF TIDREGITAB.BEREDSKAP NE "" THEN DO:
               IF TIDREGITAB.LONTILLAGG NE "" THEN DO:
                  CREATE tidallt.
                  BUFFER-COPY TIDREGITAB EXCEPT 
                  TIDREGITAB.BEREDSKAP TIDREGITAB.BERANTAL TIDREGITAB.BEREDSKAPSTART
                  TIDREGITAB.BEREDSKAPSLUT TIDREGITAB.BERBEORD
                  TO tidallt.
                  ASSIGN
                  tidallt.TYP = "TID"
                  tidallt.RECTIDVIS = RECID(TIDREGITAB).
                  CREATE tidallt.
                  ASSIGN
                  tidallt.TIDLOG = FALSE
                  tidallt.PERSONALKOD = TIDREGITAB.PERSONALKOD
                  tidallt.DATUM      = TIDREGITAB.DATUM      
                  tidallt.GODKAND    = TIDREGITAB.GODKAND    
                  tidallt.VECKOKORD  = TIDREGITAB.VECKOKORD  
                  tidallt.DAG         = TIDREGITAB.DAG
                  tidallt.VECKONUMMER = TIDREGITAB.VECKONUMMER
                  tidallt.AONR        = TIDREGITAB.AONR  
                  tidallt.DELNR       = TIDREGITAB.DELNR                  
                  tidallt.BEREDSKAP   = TIDREGITAB.BEREDSKAP 
                  tidallt.BERANTAL = TIDREGITAB.BERANTAL 
                  tidallt.BEREDSKAPSTART = TIDREGITAB.BEREDSKAPSTART 
                  tidallt.BEREDSKAPSLUT   = TIDREGITAB.BEREDSKAPSLUT 
                  tidallt.BERBEORD   = TIDREGITAB.BERBEORD
                  tidallt.ANVANDARE   = TIDREGITAB.ANVANDARE
                  tidallt.TYP = "TID"
                  tidallt.RECTIDVIS = RECID(TIDREGITAB).
                  
               END.
               ELSE DO:
                  CREATE tidallt.
                  BUFFER-COPY TIDREGITAB TO tidallt.
                  ASSIGN
                  tidallt.TYP = "TID"
                  tidallt.RECTIDVIS = RECID(TIDREGITAB).
                  
               END.
            END.
            ELSE IF TIDREGITAB.TRAKTKOD NE "" THEN DO:
               IF TIDREGITAB.LONTILLAGG NE "" THEN DO:
                  CREATE tidallt.
                  BUFFER-COPY TIDREGITAB EXCEPT TIDREGITAB.TRAKTAUTO 
                  TIDREGITAB.TRAKTKOD TIDREGITAB.TRAKTANTAL 
                  TO tidallt.
                  ASSIGN
                  tidallt.TYP = "TID"
                  tidallt.RECTIDVIS = RECID(TIDREGITAB).
                  IF tidallt.TIDLOG = TRUE THEN DO:
                     FIND FIRST AONRTAB WHERE AONRTAB.AONR = tidallt.AONR AND AONRTAB.DELNR = tidallt.DELNR NO-LOCK NO-ERROR.
                     IF AVAILABLE AONRTAB THEN DO:
                        tidallt.ANVANDARE = AONRTAB.ORT.
                     END.
                  END.
                  
                  CREATE tidallt.
                  ASSIGN
                  tidallt.TIDLOG = FALSE
                  tidallt.PERSONALKOD = TIDREGITAB.PERSONALKOD
                  tidallt.DATUM      = TIDREGITAB.DATUM      
                  tidallt.GODKAND    = TIDREGITAB.GODKAND    
                  tidallt.VECKOKORD  = TIDREGITAB.VECKOKORD  
                  tidallt.DAG         = TIDREGITAB.DAG
                  tidallt.VECKONUMMER = TIDREGITAB.VECKONUMMER
                  tidallt.AONR        = TIDREGITAB.AONR  
                  tidallt.DELNR       = TIDREGITAB.DELNR
                  tidallt.TRAKTAUTO  = TIDREGITAB.TRAKTAUTO 
                  tidallt.TRAKTKOD   = TIDREGITAB.TRAKTKOD 
                  tidallt.TRAKTANTAL = TIDREGITAB.TRAKTANTAL 
                  tidallt.TRAKTAMENTE = TIDREGITAB.TRAKTAMENTE 
                  tidallt.TRAKTTOT   = TIDREGITAB.TRAKTTOT 
                  tidallt.TYP = "TID"
                  tidallt.RECTIDVIS = RECID(TIDREGITAB).
                  
               END.
               ELSE DO:
                  CREATE tidallt.
                  BUFFER-COPY TIDREGITAB TO tidallt.
                  ASSIGN
                  tidallt.TYP = "TID"
                  tidallt.RECTIDVIS = RECID(TIDREGITAB).
                  
                  IF tidallt.TIDLOG = TRUE THEN DO:
                     FIND FIRST AONRTAB WHERE AONRTAB.AONR = tidallt.AONR AND AONRTAB.DELNR = tidallt.DELNR NO-LOCK NO-ERROR.
                     IF AVAILABLE AONRTAB THEN DO:
                        tidallt.ANVANDARE = AONRTAB.ORT.
                     END.
                  END.
               END.
            END.
            ELSE DO:
               CREATE tidallt.
               BUFFER-COPY TIDREGITAB TO tidallt.
               ASSIGN
               tidallt.TYP = "TID"
               tidallt.RECTIDVIS = RECID(TIDREGITAB).
               
               IF tidallt.TIDLOG = TRUE THEN DO:
                  FIND FIRST AONRTAB WHERE AONRTAB.AONR = tidallt.AONR AND AONRTAB.DELNR = tidallt.DELNR NO-LOCK NO-ERROR.
                  IF AVAILABLE AONRTAB THEN DO:
                     tidallt.ANVANDARE = AONRTAB.ORT.
                  END.
               END.
            END.
            
         END.
         GET NEXT tq NO-LOCK.
      END.
      OPEN QUERY toq FOR EACH tidallt.
      GET FIRST toq NO-LOCK.
      DO WHILE AVAILABLE(tidallt):
         IF tidallt.OKOD1 NE "" THEN DO:
            RUN overt_UI.
            tidallt.TYP = "OVER".
         END.             
         IF tidallt.LONTILLAGG NE "" THEN DO:
            FIND FIRST LONTILL WHERE LONTILL.KOD = ANSTFORMTAB.KOD AND 
            LONTILL.LONTILLAGG = tidallt.LONTILLAGG NO-LOCK NO-ERROR.
            IF AVAILABLE LONTILL THEN DO:
               ASSIGN
               tidallt.VILART = LONTILL.VILART 
               tidallt.ENHET = LONTILL.ENHET.
            END.
            tidallt.TYP = "LON".
         END.
         IF tidallt.BEREDSKAP NE "" THEN DO:
            FIND FIRST BERKOD WHERE BERKOD.BEREDSKAPSAVTAL = PERSONALTAB.BEREDSKAPSAVTAL AND
            BERKOD.BEREDSKAP = tidallt.BEREDSKAP NO-LOCK NO-ERROR.
            IF AVAILABLE BERKOD THEN DO:
               tidallt.VILART = BERKOD.VILART. 
            END.
            tidallt.TYP = "BER".
         END.
         IF tidallt.TRAKTKOD NE "" THEN DO:
            FIND FIRST TRAKTATAB WHERE TRAKTATAB.TRAAVTAL = PERSONALTAB.TRAAVTAL AND 
            TRAKTATAB.TRAKTKOD = tidallt.TRAKTKOD NO-LOCK NO-ERROR.    
            IF AVAILABLE TRAKTATAB THEN DO:
               tidallt.VILART = TRAKTATAB.VILART. 
            END.
            tidallt.TYP = "TRA".        
         END.
         GET NEXT toq NO-LOCK.
      END.
      /* LENA 20120508 feldatum
      FOR EACH tidallt NO-LOCK:
         FIND FIRST TIDFEL WHERE TIDFEL.PERSONALKOD = tidallt.PERSONALKOD AND TIDFEL.DATUM = tidallt.DATUM AND TIDFEL.START = tidallt.START AND TIDFEL.SLUT = tidallt.SLUT AND
         TIDFEL.AONR = tidallt.AONR AND TIDFEL.DELNR = tidallt.DELNR NO-LOCK NO-ERROR.
         IF AVAILABLE TIDFEL THEN DO:
            tidallt.FELDATUM = TIDFEL.FELDATUM.
         END.    
      END.*/
   END.
  
   IF vadgora = 3 THEN DO:
      /*NYA POSTER*/
      FOR EACH tidallt WHERE tidallt.PERSONALKOD = "":
         FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidallt.RECTIDVIS NO-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:
            BUFFER-COPY TIDREGITAB TO tidallt.
            
         END.
         ELSE DELETE tidallt.
      END.
   END.
   
END PROCEDURE.
PROCEDURE over_UI.
   CREATE overtemp.
   ASSIGN 
   overtemp.AONR = TIDREGITAB.AONR 
   overtemp.DAG = TIDREGITAB.DAG 
   overtemp.DELNR = TIDREGITAB.DELNR 
   overtemp.OVERANTAL = TIDREGITAB.OANT1 
   overtemp.OVERAUTO = TIDREGITAB.OVERAUTO 
   overtemp.OVERTIDTILL = TIDREGITAB.OKOD1
   overtemp.DATUM = TIDREGITAB.DATUM
   overtemp.GODKAND = TIDREGITAB.GODKAND
   overtemp.PROGRAM = TIDREGITAB.PROGRAM
   overtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
   overtemp.VECKONUMMER = TIDREGITAB.VECKONUMMER
   overtemp.RECTIDVIS = RECID(TIDREGITAB).
   FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND
   OVERKOD.OVERTIDTILL = overtemp.OVERTIDTILL NO-LOCK NO-ERROR.                  
   IF AVAILABLE OVERKOD THEN overtemp.VILART = OVERKOD.VILART.  
   IF TIDREGITAB.OKOD2 NE "" THEN DO:
      CREATE overtemp.
      ASSIGN 
      overtemp.AONR = TIDREGITAB.AONR 
      overtemp.DAG = TIDREGITAB.DAG 
      overtemp.DELNR = TIDREGITAB.DELNR 
      overtemp.OVERANTAL = TIDREGITAB.OANT2 
      overtemp.OVERAUTO = TIDREGITAB.OVERAUTO 
      overtemp.OVERTIDTILL = TIDREGITAB.OKOD2
      overtemp.DATUM = TIDREGITAB.DATUM
      overtemp.GODKAND = TIDREGITAB.GODKAND
      overtemp.VECKONUMMER = TIDREGITAB.VECKONUMMER
      overtemp.PROGRAM = TIDREGITAB.PROGRAM
      overtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
      overtemp.VECKONUMMER = TIDREGITAB.VECKONUMMER
      overtemp.RECTIDVIS = RECID(TIDREGITAB).
      FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND
      OVERKOD.OVERTIDTILL = overtemp.OVERTIDTILL NO-LOCK NO-ERROR.                  
      IF AVAILABLE OVERKOD THEN overtemp.VILART = OVERKOD.VILART.      
   END.
   IF TIDREGITAB.OKOD3 NE "" THEN DO:
      CREATE overtemp.
      ASSIGN 
      overtemp.AONR = TIDREGITAB.AONR 
      overtemp.DAG = TIDREGITAB.DAG 
      overtemp.DELNR = TIDREGITAB.DELNR 
      overtemp.OVERANTAL = TIDREGITAB.OANT3 
      overtemp.OVERAUTO = TIDREGITAB.OVERAUTO 
      overtemp.OVERTIDTILL = TIDREGITAB.OKOD3
      overtemp.DATUM = TIDREGITAB.DATUM
      overtemp.GODKAND = TIDREGITAB.GODKAND
      overtemp.VECKONUMMER = TIDREGITAB.VECKONUMMER
      overtemp.PROGRAM = TIDREGITAB.PROGRAM
      overtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
      overtemp.VECKONUMMER = TIDREGITAB.VECKONUMMER
      overtemp.RECTIDVIS = RECID(TIDREGITAB).
      FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND
      OVERKOD.OVERTIDTILL = overtemp.OVERTIDTILL NO-LOCK NO-ERROR.                  
      IF AVAILABLE OVERKOD THEN overtemp.VILART = OVERKOD.VILART.      
   END.
END PROCEDURE.
PROCEDURE overt_UI.
   CREATE overtemp.
   ASSIGN 
   overtemp.AONR = tidallt.AONR 
   overtemp.DAG = tidallt.DAG 
   overtemp.DELNR = tidallt.DELNR 
   overtemp.OVERANTAL = tidallt.OANT1 
   overtemp.OVERAUTO = tidallt.OVERAUTO 
   overtemp.OVERTIDTILL = tidallt.OKOD1
   overtemp.DATUM = tidallt.DATUM
   overtemp.GODKAND = tidallt.GODKAND
   overtemp.PROGRAM = tidallt.PROGRAM
   overtemp.PERSONALKOD = tidallt.PERSONALKOD
   overtemp.VECKONUMMER = tidallt.VECKONUMMER
   overtemp.RECTIDVIS = tidallt.RECTIDVIS.
   FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND
   OVERKOD.OVERTIDTILL = overtemp.OVERTIDTILL NO-LOCK NO-ERROR.                  
   IF AVAILABLE OVERKOD THEN overtemp.VILART = OVERKOD.VILART.   
   IF tidallt.OKOD2 NE "" THEN DO:
      CREATE overtemp.
      ASSIGN 
      overtemp.AONR = tidallt.AONR 
      overtemp.DAG = tidallt.DAG 
      overtemp.DELNR = tidallt.DELNR 
      overtemp.OVERANTAL = tidallt.OANT2 
      overtemp.OVERAUTO = tidallt.OVERAUTO 
      overtemp.OVERTIDTILL = tidallt.OKOD2
      overtemp.DATUM = tidallt.DATUM
      overtemp.GODKAND = tidallt.GODKAND
      overtemp.PROGRAM = tidallt.PROGRAM
      overtemp.PERSONALKOD = tidallt.PERSONALKOD
      overtemp.VECKONUMMER = tidallt.VECKONUMMER
      overtemp.RECTIDVIS = tidallt.RECTIDVIS.
      FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND
      OVERKOD.OVERTIDTILL = overtemp.OVERTIDTILL NO-LOCK NO-ERROR.                  
      IF AVAILABLE OVERKOD THEN overtemp.VILART = OVERKOD.VILART.      
   END.
   IF tidallt.OKOD3 NE "" THEN DO:
      CREATE overtemp.
      ASSIGN 
      overtemp.AONR = tidallt.AONR 
      overtemp.DAG = tidallt.DAG 
      overtemp.DELNR = tidallt.DELNR 
      overtemp.OVERANTAL = tidallt.OANT3 
      overtemp.OVERAUTO = tidallt.OVERAUTO 
      overtemp.OVERTIDTILL = tidallt.OKOD3
      overtemp.DATUM = tidallt.DATUM
      overtemp.GODKAND = tidallt.GODKAND
      overtemp.PROGRAM = tidallt.PROGRAM
      overtemp.PERSONALKOD = tidallt.PERSONALKOD
      overtemp.VECKONUMMER = tidallt.VECKONUMMER
      overtemp.RECTIDVIS = tidallt.RECTIDVIS.
      FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND
      OVERKOD.OVERTIDTILL = overtemp.OVERTIDTILL NO-LOCK NO-ERROR.                  
      IF AVAILABLE OVERKOD THEN overtemp.VILART = OVERKOD.VILART.      
   END.
END PROCEDURE.
