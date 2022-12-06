/*TRZONUTF.P*/
/*DEFINE SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.

DEFINE SHARED VARIABLE avslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE SHARED VARIABLE maxzon LIKE TRAKTASTART.MAXVARDEZON NO-UNDO.

DEFINE VARIABLE tratot LIKE TIDREGITAB.TRAKTTOT NO-UNDO.
DEFINE VARIABLE stratot AS INTEGER NO-UNDO.
DEFINE VARIABLE ap AS INTEGER NO-UNDO.
DEFINE VARIABLE apmax AS INTEGER NO-UNDO.
DEFINE VARIABLE okzon AS LOGICAL NO-UNDO.


DEFINE SHARED TEMP-TABLE traktafil
   FIELD TSTART LIKE TIDREGITAB.START
   FIELD TSLUT LIKE TIDREGITAB.SLUT
   FIELD TTRAKTAMENTE LIKE TIDREGITAB.TRAKTAMENTE
   FIELD TZON LIKE TIDREGITAB.TRAKTAMENTE
   FIELD TTRAKTTOT LIKE TIDREGITAB.TRAKTTOT
   FIELD TSTRAKTTOT AS INTEGER
   FIELD TDATUM LIKE TIDREGITAB.DATUM.
DEFINE BUFFER tidbuff FOR TIDREGITAB.
FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR.
ap = 0.
FOR EACH traktafil BY traktafil.TDATUM BY traktafil.TSTART BY traktafil.TSLUT:
   nytid = traktafil.TTRAKTTOT.
   RUN TIMSEK.P.
   ASSIGN traktafil.TSTRAKTTOT = sekunder
   traktafil.TZON = 99.
   ap = ap + 1.
END.   
ASSIGN
apmax = ap
okzon = FALSE
stratot = 0.
REPEAT:
   REPEAT:
      FOR EACH traktafil WHERE traktafil.TZON > maxzon
      BY traktafil.TDATUM BY traktafil.TSTART BY traktafil.TSLUT:
	  ASSIGN traktafil.TZON = maxzon.
	  ap = ap - 1.
	  IF traktafil.TTRAKTAMENTE >= maxzon THEN DO:
	     ASSIGN
	     stratot = stratot + traktafil.TSTRAKTTOT
	     okzon = TRUE.
	  END.
	  ELSE DO:
	     IF okzon = TRUE THEN LEAVE.
	  END.
      END.
      LEAVE.
   END.
   sekunder = stratot.
   RUN SEKTIM.P.
   tratot = nytid.  
   FIND FIRST TRAKTREGLER WHERE TRAKTREGLER.ARBTIDMIN < tratot AND
   TRAKTREGLER.ARBTIDMAX >= tratot AND TRAKTREGLER.TRAAVTAL = PERSONALTAB.TRAAVTAL
   AND TRAKTREGLER.TRAKTAMENTE = maxzon USE-INDEX AVTAL NO-LOCK NO-ERROR.
   IF NOT AVAILABLE TRAKTREGLER THEN DO:
      IF Guru.Konstanter:globforetag = "gran"   OR Guru.Konstanter:globforetag = "celpa"   THEN DO:
         FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
         USE-INDEX ANSTF NO-LOCK NO-ERROR.
         IF ANSTFORMTAB.KOD BEGINS "K" AND tratot > 18 THEN DO:
            CREATE FELTEXT.
            ASSIGN 
            FELTEXT.ANVANDARE = Guru.Konstanter:globanv                      
            FELTEXT.FELTEXT = 'Det finns inget avtal som täcker förrättningar som är' + STRING(tratot) + ' tim. Kontakta löneadministratör'
            FELTEXT.PROGRAM = "TRAKTBER" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv.               
         END.
      END.
      IF ap = 0 THEN DO:                            
         ASSIGN
         ap = apmax
	     maxzon = maxzon - 1.
      END.         
      ASSIGN
      okzon = FALSE      
      tratot = 0
      stratot = 0.
      IF maxzon = 0 THEN LEAVE.
      IF Guru.Konstanter:globforetag = "GKAL" THEN DO :         
         /*75 kr avdrag om lart 701 37.5 kr avdrag om 700*/
         FOR EACH tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
         AND tidbuff.DATUM = avdatum AND tidbuff.LONTILLAGG BEGINS "72" 
         AND tidbuff.LONAUTO = TRUE EXCLUSIVE-LOCK:                                       
            ASSIGN tidbuff.LONTILLANTAL = 0.                              
         END.
         FOR EACH tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
         AND tidbuff.DATUM = avdatum AND tidbuff.LONTILLAGG BEGINS "78" 
         AND tidbuff.LONAUTO = TRUE EXCLUSIVE-LOCK:                                       
            ASSIGN tidbuff.LONTILLANTAL = 0.                              
         END.         
      END.         
   END.
   ELSE DO:     
      FIND FIRST ANSTFORMTAB WHERE ANSTFORM.ANSTALLNING = PERSONALTAB.ANSTALLNING
      USE-INDEX ANSTF NO-LOCK NO-ERROR.         
      DO:   
         FIND FIRST TIDREGITAB WHERE 
         TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = avdatum AND TIDREGITAB.SLUT = avslut AND
         TIDREGITAB.TIDLOG = TRUE USE-INDEX PSTART EXCLUSIVE-LOCK NO-ERROR.
         ASSIGN TIDREGITAB.TRAKTKOD = TRAKTREGLER.TRAKTKOD
         TIDREGITAB.TRAKTANTAL = TRAKTREGLER.TRAKTANTAL.        
         IF Guru.Konstanter:globforetag = "GKAL" THEN DO :         
            /*75 kr avdrag om lart 701 37.5 kr avdrag om 700*/
            FOR EACH tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
            AND tidbuff.DATUM = avdatum AND tidbuff.LONTILLAGG BEGINS "72" 
            AND tidbuff.LONAUTO = TRUE EXCLUSIVE-LOCK:
               /*lart 847 = halvt måltidsavdrag*/
               IF TIDREGITAB.TRAKTKOD = "700" THEN DO:
                  ASSIGN tidbuff.LONTILLAGG = "847" tidbuff.LONTILLANTAL = 1.                                           
               END.                
               ELSE IF TIDREGITAB.TRAKTKOD = "701" THEN ASSIGN tidbuff.LONTILLANTAL = 1.                                                                                        
            END.
            FOR EACH tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
            AND tidbuff.DATUM = avdatum AND tidbuff.LONTILLAGG = "847" 
            AND tidbuff.LONAUTO = TRUE EXCLUSIVE-LOCK:
               /*lart 847 = halvt måltidsavdrag*/
               IF TIDREGITAB.TRAKTKOD = "701" THEN DO:
                  ASSIGN tidbuff.LONTILLAGG = "727" tidbuff.LONTILLANTAL = 1.                                                             
               END.                                                                     
               ELSE IF TIDREGITAB.TRAKTKOD = "700" THEN ASSIGN tidbuff.LONTILLANTAL = 1.                                   
            END.
            FOR EACH tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
            AND tidbuff.DATUM = avdatum AND tidbuff.LONTILLAGG BEGINS "78" 
            AND tidbuff.LONAUTO = TRUE EXCLUSIVE-LOCK:                           
               IF TIDREGITAB.TRAKTKOD = "700" THEN ASSIGN tidbuff.LONTILLANTAL = 1.
               ELSE IF TIDREGITAB.TRAKTKOD = "701" THEN ASSIGN tidbuff.LONTILLANTAL = 1.
               ELSE ASSIGN tidbuff.LONTILLANTAL = 0.                              
            END.
         END.                  
      END.   
      tratot = 0.
      IF ap = 0 THEN LEAVE.
   END.
END.
