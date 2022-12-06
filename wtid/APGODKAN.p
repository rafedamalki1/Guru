/*APGODKAN.P*/
&Scoped-define NEW NEW
{REGVAR.I}
{GODKANUT.I}
DEFINE VARIABLE arregvnr AS INTEGER NO-UNDO.
DEFINE VARIABLE istrans AS LOGICAL INITIAL YES.
FOR EACH appmarkpers USE-INDEX PERSONALKOD NO-LOCK.
   ASSIGN
   regar = YEAR(appmarkpers.DATUM)   
   regmnr = MONTH(appmarkpers.DATUM)
   regdatum = appmarkpers.DATUM.
   IF gvisatidpermanad = TRUE THEN DO:            
      OPEN QUERY tidq FOR EACH TIDREGITAB WHERE 
      TIDREGITAB.PERSONALKOD = appmarkpers.PERSONALKOD AND
      YEAR(TIDREGITAB.DATUM) = regar AND MONTH(TIDREGITAB.DATUM) = regmnr 
      AND TIDREGITAB.GODKAND = "" 
      NO-LOCK.
      GET FIRST tidq NO-LOCK.      
      IF NOT AVAILABLE TIDREGITAB THEN DO:
         RUN god_UI.
         NEXT.      
      END.
      ELSE RUN tidgodk_UI.      
   END.   
   ELSE DO:
      OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = appmarkpers.PERSONALKOD AND
      TIDREGITAB.VECKONUMMER = appmarkpers.VECKONUMMER  AND 
      TIDREGITAB.GODKAND = ""
      USE-INDEX PVNR NO-LOCK.                       
      GET FIRST tidq NO-LOCK.
      IF NOT AVAILABLE TIDREGITAB THEN NEXT.
      ELSE RUN tidgodk_UI.             
   END.
   CLOSE QUERY tidq.   
END.       
PROCEDURE god_UI:   
   DO TRANSACTION:         
      FIND FIRST GODKOLL WHERE 
      GODKOLL.PERSONALKOD = appmarkpers.PERSONALKOD AND  
      GODKOLL.DATAR = regar AND GODKOLL.DATMAN = regmnr 
      USE-INDEX PKODAR EXCLUSIVE-LOCK NO-ERROR.             
      IF NOT AVAILABLE GODKOLL THEN DO:
         CREATE GODKOLL.
      END.
      ASSIGN
      GODKOLL.ANVANDARE = globanv + STRING(TODAY) + STRING(TIME,"HH:MM")              
      GODKOLL.PERSONALKOD = appmarkpers.PERSONALKOD
      GODKOLL.DATAR = regar 
      GODKOLL.DATMAN = regmnr.                   
      IF GODKOLL.DATUM <= regdatum OR GODKOLL.DATUM = ? THEN DO:                  
         ASSIGN
         GODKOLL.DATUM = regdatum            
         GODKOLL.VECKONUMMER = regvnr.
      END.               
      IF MONTH(TODAY) = regmnr AND YEAR(TODAY) = regar THEN GODKOLL.KLAR = FALSE.
      ELSE DO:
         IF MONTH(GODKOLL.DATUM) = MONTH(GODKOLL.DATUM + 1) THEN GODKOLL.KLAR = FALSE. 
         ELSE GODKOLL.KLAR = TRUE.                             
      END.      
   END.   
END PROCEDURE.
PROCEDURE tidgodk_UI:   
   IF gvisatidpermanad = TRUE THEN DO:
      DO TRANSACTION:
         GET FIRST tidq EXCLUSIVE-LOCK.         
         IF AVAILABLE TIDREGITAB THEN DO:
            IF TIDREGITAB.DATUM <= regdatum THEN DO:               
               ASSIGN TIDREGITAB.GODKAND = "G" + STRING(regvnr, "999") + Guru.Konstanter:globanv.
            END.
         END.
      END.
      DO WHILE AVAILABLE(TIDREGITAB):         
         DO TRANSACTION:
            GET NEXT tidq EXCLUSIVE-LOCK.
            IF AVAILABLE TIDREGITAB THEN DO:
               IF TIDREGITAB.DATUM <= regdatum THEN DO:
                  ASSIGN TIDREGITAB.GODKAND = "G" + STRING(regvnr, "999") + Guru.Konstanter:globanv.
               END.
            END.
         END.  
      END.
      RUN god_UI.      
   END.
   ELSE DO: 
      DO TRANSACTION:
         GET FIRST tidq EXCLUSIVE-LOCK.
         IF AVAILABLE TIDREGITAB THEN DO:
            IF TIDREGITAB.DATUM <= regdatum THEN DO:
               ASSIGN TIDREGITAB.GODKAND = "G" + STRING(regvnr, "999") + Guru.Konstanter:globanv.
            END.
         END.
      END.
      DO WHILE AVAILABLE(TIDREGITAB):
         DO TRANSACTION:
            GET NEXT tidq EXCLUSIVE-LOCK.
            IF AVAILABLE TIDREGITAB THEN DO:
               IF TIDREGITAB.DATUM <= regdatum THEN DO:
                  ASSIGN TIDREGITAB.GODKAND = "G" + STRING(regvnr, "999") + Guru.Konstanter:globanv.
               END.
            END.
         END.  
      END.
   END.      
   /* Det måste vara vecka som hör till regdatum - alltså ej appmarkpers.VECKONUMMER*/
   RUN REGVEC.P.   
   IF YEAR(regdatum) < 2001 THEN arregvnr = regvnr.
   ELSE arregvnr = INTEGER(SUBSTRING(STRING(YEAR(regdatum),"9999"),1,3) + STRING(regvnr,"999")).
   
   FIND FIRST VECKOARBAV WHERE 
   VECKOARBAV.PERSONALKOD = appmarkpers.PERSONALKOD AND
   VECKOARBAV.VECKONUMMER = arregvnr 
   USE-INDEX PVNUMMER NO-LOCK NO-ERROR.
   IF NOT AVAILABLE VECKOARBAV THEN DO TRANSACTION:      
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = appmarkpers.PERSONALKOD
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      {RULLVECKO.I}
      CREATE VECKOARBAV.
      ASSIGN VECKOARBAV.PERSONALKOD = appmarkpers.PERSONALKOD
      VECKOARBAV.VECKONUMMER = arregvnr
      VECKOARBAV.VECKOSCHEMA = rull-veckovar.
   END.
END PROCEDURE.
