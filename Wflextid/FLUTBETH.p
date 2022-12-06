/*FLUTBETH.P*/
DEFINE TEMP-TABLE flbettemp
   FIELD PERSONALKOD AS CHARACTER    
   FIELD DATUM AS DATE  
   FIELD TIMMAR AS DECIMAL  
   FIELD ANVANDARE AS CHARACTER  
   FIELD ACCFORE AS DECIMAL  
   FIELD ACCEFTER AS DECIMAL
   FIELD FREC AS RECID
   INDEX PKOD IS PRIMARY PERSONALKOD DATUM
   INDEX DATUM DATUM PERSONALKOD
   INDEX TIMMAR TIMMAR 
   INDEX ACCFORE   ACCFORE   
   INDEX ACCEFTER  ACCEFTER  
   INDEX ANVANDARE ANVANDARE .
DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER frecvar AS RECID NO-UNDO.
DEFINE INPUT PARAMETER hjtim AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR flbettemp.
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE NEW SHARED VARIABLE hjvart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE seku AS INTEGER FORMAT "-9999999" NO-UNDO.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
IF vadgora = 1 THEN DO:
   EMPTY TEMP-TABLE flbettemp  NO-ERROR.    
   OPEN QUERY fq FOR EACH FLBET NO-LOCK.
   GET FIRST fq NO-LOCK.
   DO WHILE AVAILABLE(FLBET):
      CREATE flbettemp.
      BUFFER-COPY FLBET TO flbettemp.
      flbettemp.FREC = RECID(FLBET).
      GET NEXT fq NO-LOCK.
   END.
END.
IF vadgora = 2 THEN DO TRANSACTION:
   FIND FIRST flbettemp WHERE flbettemp.FREC = frecvar NO-ERROR.
   IF flbettemp.FREC = ? THEN DO:
      CREATE FLBET.
      flbettemp.FREC = RECID(FLBET).
   END.
   ELSE DO:
      FIND FLBET WHERE RECID(FLBET) = flbettemp.FREC EXCLUSIVE-LOCK NO-ERROR.
   END.
   BUFFER-COPY flbettemp TO FLBET.
   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      FIND FIRST FLEXSALDO WHERE FLEXSALDO.PERSONALKOD =  flbettemp.PERSONALKOD EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE FLEXSALDO THEN DO:
         CREATE FLEXSALDO.              
         ASSIGN FLEXSALDO.PERSONALKOD = flbettemp.PERSONALKOD.
      END.      
      IF frecvar = ? THEN DO:
         ASSIGN FLBET.ACCFORE = FLEXSALDO.PERIODFLEX.
         flbettemp.ACCFORE = FLEXSALDO.PERIODFLEX.
         nytid = FLEXSALDO.PERIODFLEX.
         RUN TIMSEK.P.
         seku = sekunder.
         nytid = FLBET.TIMMAR.
         RUN TIMSEK.P.
         sekunder = seku + sekunder.
         RUN FSEKTIM.P.
         ASSIGN FLEXSALDO.PERIODFLEX = fnytid  FLBET.ACCEFTER = fnytid. 
         flbettemp.ACCEFTER = FLBET.ACCEFTER.
      END.
      ELSE DO:
         nytid = FLEXSALDO.PERIODFLEX.
         RUN TIMSEK.P.
         seku = sekunder.
         nytid = hjtim.
         RUN TIMSEK.P.
         seku = seku - sekunder.
         sekunder = seku.
         RUN FSEKTIM.P.
         ASSIGN FLBET.ACCFORE = fnytid.
         flbettemp.ACCFORE = FLBET.ACCFORE.
         nytid = FLBET.TIMMAR.
         RUN TIMSEK.P.
         sekunder = seku + sekunder.
         RUN FSEKTIM.P.
         ASSIGN 
         FLEXSALDO.PERIODFLEX = fnytid  
         FLBET.ACCEFTER = fnytid. 
         flbettemp.ACCEFTER = FLBET.ACCEFTER.
      END.   
   END. 
END.

