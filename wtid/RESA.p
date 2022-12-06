/*RESA.P kontrollerar inom arbetstid ger restidsersättning  */
/*DEFINE SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/
DEFINE SHARED VARIABLE nytid AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER NO-UNDO. 
DEFINE SHARED VARIABLE regdatum LIKE TIDREGITAB.DATUM NO-UNDO. 
DEFINE SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE SHARED VARIABLE regstartsek AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE regslutsek AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID  NO-UNDO.    
DEFINE SHARED VARIABLE tidtabrec AS RECID  NO-UNDO. 
DEFINE SHARED VARIABLE regvnr LIKE TIDREGITAB.VECKONUMMER NO-UNDO. 
DEFINE SHARED VARIABLE bustart3 LIKE TIDREGITAB.START NO-UNDO.
DEFINE SHARED VARIABLE bilforare AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.
DEFINE SHARED VARIABLE enflerdygns AS LOGICAL FORMAT "ENDAGS/FLERDYGNS" NO-UNDO. 

DEFINE VARIABLE outtag LIKE TIDREGITAB.OVERTIDUTTAG NO-UNDO.
DEFINE VARIABLE appfel AS LOGICAL NO-UNDO.
DEFINE VARIABLE prisvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE hjtid AS DECIMAL NO-UNDO.
DEFINE VARIABLE hjvar AS LOGICAL NO-UNDO.
DEFINE BUFFER tidbuff FOR TIDREGITAB. 

FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR.
DO TRANSACTION:
   FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.
   IF TIDREGITAB.ENFLERDAGS = "Endag" THEN enflerdygns = TRUE. 
   IF TIDREGITAB.ENFLERDAGS = "Flerdag" THEN enflerdygns = FALSE.
   ASSIGN   
   nytid = TIDREGITAB.START.
   RUN TIMSEK.P.
   ASSIGN
   regstartsek = sekunder
   nytid = TIDREGITAB.SLUT.
   RUN TIMSEK.P.
   ASSIGN
   regslutsek = sekunder
   regdatum = TIDREGITAB.DATUM   
   regvnr = TIDREGITAB.VECKONUMMER.
   RUN TOTTID.P.
   ASSIGN TIDREGITAB.TOTALT = nytid. 
   RUN SLUTARB.P.  
END.
ASSIGN
hjvar = FALSE.
IF TIDREGITAB.START GE regstart AND TIDREGITAB.START < regslut AND
TIDREGITAB.SLUT > regslut THEN DO TRANSACTION:
   FIND PREV TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE
   USE-INDEX PSTART EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE TIDREGITAB THEN persrec = persrec.
   ELSE DO:
      IF TIDREGITAB.DATUM NE regdatum THEN persrec = persrec.
      ELSE DO:
         FIND tidbuff WHERE RECID(tidbuff) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.
	      IF TIDREGITAB.SLUT > tidbuff.START AND
	      TIDREGITAB.SLUT < tidbuff.SLUT AND TIDREGITAB.START < tidbuff.START THEN DO:
	         ASSIGN TIDREGITAB.SLUT = tidbuff.START.  
	         nytid = TIDREGITAB.TOTALT.
            RUN TIMSEK.P.
            ASSIGN            
            nytid = TIDREGITAB.START.
            RUN TIMSEK.P.
            ASSIGN
            regstartsek = sekunder
            nytid = TIDREGITAB.SLUT.
            RUN TIMSEK.P.
            ASSIGN
            regslutsek = sekunder
            regdatum = TIDREGITAB.DATUM.            
            RUN TOTTID.P.
            ASSIGN TIDREGITAB.TOTALT = nytid.    /*NYTOT EJ I GAMMLA RESA*/
         END.
         ELSE DO:
            FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
            TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.START GE tidbuff.START AND TIDREGITAB.SLUT < tidbuff.SLUT
            AND RECID(TIDREGITAB) NE RECID(tidbuff) USE-INDEX PSTART EXCLUSIVE-LOCK.      
               TIDREGITAB.TRAKTAMENTE = tidbuff.TRAKTAMENTE.
            END.
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
            TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.START = tidbuff.START 
            AND RECID(TIDREGITAB) NE RECID(tidbuff) USE-INDEX PSTART NO-LOCK NO-ERROR.
            IF AVAILABLE TIDREGITAB THEN DO:              
               hjvar = TRUE.
            END.
            ELSE hjvar = FALSE.
         END.
      END.
   END.                 
   FIND tidbuff WHERE RECID(tidbuff) = tidtabrec
   EXCLUSIVE-LOCK NO-ERROR.
   IF tidbuff.SLUT > regslut AND hjvar = FALSE THEN DO:      
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.SLUT > regslut AND
      TIDREGITAB.START = regslut AND TIDREGITAB.PRISTYP = tidbuff.PRISTYP
      USE-INDEX PSTART EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE TIDREGITAB THEN DO:
         CREATE TIDREGITAB.
      END. 
      ASSIGN 
      TIDREGITAB.OVERTIDTILL = tidbuff.OVERTIDTILL
      TIDREGITAB.ENFLERDAGS = tidbuff.ENFLERDAGS
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD 
      TIDREGITAB.DAG = tidbuff.DAG 
      TIDREGITAB.DATUM = tidbuff.DATUM
      TIDREGITAB.VECKONUMMER = tidbuff.VECKONUMMER       
      TIDREGITAB.START = regslut TIDREGITAB.SLUT = tidbuff.SLUT
      SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "RESA" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv      
      TIDREGITAB.OVERTIDTILL = tidbuff.OVERTIDTILL
      TIDREGITAB.TRAKTAMENTE = tidbuff.TRAKTAMENTE
      TIDREGITAB.AONR = tidbuff.AONR 
      TIDREGITAB.DELNR = tidbuff.DELNR
      TIDREGITAB.BILFORARE = tidbuff.BILFORARE 
      TIDREGITAB.OVERTIDUTTAG = tidbuff.OVERTIDUTTAG
      TIDREGITAB.PRISTYP = tidbuff.PRISTYP
      TIDREGITAB.PRIS = tidbuff.PRIS.
      nytid = TIDREGITAB.TOTALT.
      RUN TIMSEK.P.
      ASSIGN      
      nytid = TIDREGITAB.START.
      RUN TIMSEK.P.
      ASSIGN
      regstartsek = sekunder
      nytid = TIDREGITAB.SLUT.
      RUN TIMSEK.P.
      ASSIGN
      regslutsek = sekunder
      regvnr = TIDREGITAB.VECKONUMMER
      regdatum = TIDREGITAB.DATUM.      
      RUN TOTTID.P.
      ASSIGN 
      TIDREGITAB.TOTALT = nytid        
      tidtabrec = RECID(TIDREGITAB). 
      ASSIGN 
      tidbuff.SLUT = regslut tidbuff.LONTILLAGG = ''
      tidbuff.LONTILLANTAL = 0.
      nytid = tidbuff.TOTALT.
      RUN TIMSEK.P.   
      ASSIGN      
      nytid = tidbuff.START.
      RUN TIMSEK.P.
      ASSIGN
      regstartsek = sekunder
      nytid = tidbuff.SLUT.
      RUN TIMSEK.P.
      ASSIGN
      regslutsek = sekunder
      regdatum = tidbuff.DATUM.
      RUN TOTTID.P.
      ASSIGN tidbuff.TOTALT = nytid.      
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = tidbuff.AONR AND AONRTAB.DELNR = tidbuff.DELNR
      NO-LOCK NO-ERROR.
      IF AVAILABLE AONRTAB THEN DO:   
         IF Guru.Konstanter:globforetag  = "ELPA" OR Guru.Konstanter:globforetag  = "GKAL" OR (Guru.Konstanter:globforetag = "GRAN" AND tidbuff.DATUM >= 10/01/2002) THEN DO:                           
            FIND LAST PERSONALPRIS WHERE 
            PERSONALPRIS.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
            PERSONALPRIS.BEFATTNING = tidbuff.OVERTIDTILL AND 
            PERSONALPRIS.STARTDATUM <= tidbuff.DATUM  AND 
            PERSONALPRIS.SLUTDATUM >= tidbuff.DATUM 
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE PERSONALPRIS THEN DO:
               FIND LAST PERSONALPRIS WHERE 
               PERSONALPRIS.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
               PERSONALPRIS.BEFATTNING = tidbuff.OVERTIDTILL 
               USE-INDEX PSTART NO-LOCK NO-ERROR.
            END.
            IF AVAILABLE PERSONALPRIS THEN DO:
               ASSIGN
               tidbuff.PRISTYP = AONRTAB.PRISTYP
               tidbuff.PRIS = PERSONALPRIS.PRIS.  
            END.
         END.   
         ELSE DO:
            FIND FIRST TIMKOSTNADSTAB WHERE 
            TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
            TIMKOSTNADSTAB.PRISTYP = AONRTAB.PRISTYP 
            USE-INDEX PRISPERS NO-LOCK NO-ERROR.         
            IF AVAILABLE TIMKOSTNADSTAB THEN DO:
               ASSIGN
               tidbuff.PRISTYP = AONRTAB.PRISTYP
               tidbuff.PRIS = TIMKOSTNADSTAB.PRISA.  
            END.
         END.       
      END.
   END.
   ELSE IF hjvar = TRUE THEN DO:
      FIND tidbuff WHERE RECID(tidbuff) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.
      ASSIGN tidbuff.START = regslut tidbuff.LONTILLAGG = ''   
      tidbuff.LONTILLANTAL = 0
      nytid = tidbuff.TOTALT.
      RUN TIMSEK.P.
      ASSIGN      
      nytid = tidbuff.START.
      RUN TIMSEK.P.
      ASSIGN
      regstartsek = sekunder
      nytid = tidbuff.SLUT.
      RUN TIMSEK.P.
      ASSIGN
      regslutsek = sekunder
      regdatum = tidbuff.DATUM.      
      RUN TOTTID.P.
      ASSIGN tidbuff.TOTALT = nytid.
   END.
   FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.  
END.
ASSIGN
hjvar = FALSE
hjtid = regstart.
IF TIDREGITAB.SLUT > regstart AND TIDREGITAB.SLUT LE regslut AND 
TIDREGITAB.START < regstart THEN DO TRANSACTION:
   FIND NEXT TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE
   USE-INDEX PSTART NO-LOCK NO-ERROR.
   IF NOT AVAILABLE TIDREGITAB THEN persrec = persrec.
   ELSE DO:       
      FIND tidbuff WHERE RECID(tidbuff) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.      
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.START < tidbuff.SLUT AND TIDREGITAB.SLUT > tidbuff.SLUT
      USE-INDEX PSTART EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE TIDREGITAB THEN DO:         
         hjtid = TIDREGITAB.START.
	      ASSIGN TIDREGITAB.START = tidbuff.SLUT. 
	      nytid = TIDREGITAB.TOTALT.
	      RUN TIMSEK.P.
         ASSIGN         
         nytid = TIDREGITAB.START.
         RUN TIMSEK.P.
         ASSIGN
         regstartsek = sekunder
         nytid = TIDREGITAB.SLUT.
         RUN TIMSEK.P.
         ASSIGN
         regslutsek = sekunder
         regdatum = TIDREGITAB.DATUM.         
         RUN TOTTID.P.
         ASSIGN TIDREGITAB.TOTALT = nytid.    /*NYTOT EJ I GAMMLA RESA*/
         FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.START < tidbuff.SLUT AND TIDREGITAB.START > tidbuff.START
         AND RECID(TIDREGITAB) NE RECID(tidbuff) USE-INDEX PSTART EXCLUSIVE-LOCK:
            ASSIGN
            TIDREGITAB.TRAKTAMENTE = tidbuff.TRAKTAMENTE.
         END.
      END.
      ELSE DO:
         FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.START < tidbuff.SLUT AND TIDREGITAB.START > tidbuff.START
         AND RECID(TIDREGITAB) NE RECID(tidbuff) USE-INDEX PSTART EXCLUSIVE-LOCK.      
            TIDREGITAB.TRAKTAMENTE = tidbuff.TRAKTAMENTE.
         END.
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.START LE tidbuff.SLUT AND TIDREGITAB.START > tidbuff.START
         AND RECID(TIDREGITAB) NE RECID(tidbuff) USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:
            /*skapa ej 7-9*/
            hjvar = TRUE.
         END.
         ELSE hjvar = FALSE.
      END.
   END.
   FIND tidbuff WHERE RECID(tidbuff) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.
   IF tidbuff.SLUT > regstart AND hjvar = FALSE THEN DO:      
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.SLUT > regslut AND
      TIDREGITAB.START = regslut AND TIDREGITAB.PRISTYP = tidbuff.PRISTYP
      USE-INDEX PSTART EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE TIDREGITAB THEN DO:
         CREATE TIDREGITAB.
      END.
      ASSIGN 
      TIDREGITAB.OVERTIDTILL = tidbuff.OVERTIDTILL
      TIDREGITAB.ENFLERDAGS = tidbuff.ENFLERDAGS
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD 
      TIDREGITAB.DAG = tidbuff.DAG 
      TIDREGITAB.DATUM = tidbuff.DATUM
      TIDREGITAB.VECKONUMMER = tidbuff.VECKONUMMER
      TIDREGITAB.START = hjtid TIDREGITAB.SLUT = tidbuff.SLUT
      SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "RESA" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
      TIDREGITAB.OVERTIDTILL = tidbuff.OVERTIDTILL
      TIDREGITAB.TRAKTAMENTE = tidbuff.TRAKTAMENTE
      TIDREGITAB.AONR = tidbuff.AONR 
      TIDREGITAB.DELNR = tidbuff.DELNR
      TIDREGITAB.BILFORARE = tidbuff.BILFORARE 
      TIDREGITAB.OVERTIDUTTAG = tidbuff.OVERTIDUTTAG
      TIDREGITAB.PRISTYP = tidbuff.PRISTYP
      TIDREGITAB.PRIS = tidbuff.PRIS.  
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = TIDREGITAB.AONR AND AONRTAB.DELNR = TIDREGITAB.DELNR
      NO-LOCK NO-ERROR.
      IF AVAILABLE AONRTAB THEN DO:   
         IF Guru.Konstanter:globforetag  = "ELPA" OR Guru.Konstanter:globforetag  = "GKAL" OR (Guru.Konstanter:globforetag = "GRAN" AND TIDREGITAB.DATUM >= 10/01/2002) THEN DO:                           
            FIND LAST PERSONALPRIS WHERE 
            PERSONALPRIS.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
            PERSONALPRIS.BEFATTNING = TIDREGITAB.OVERTIDTILL AND 
            PERSONALPRIS.STARTDATUM <= TIDREGITAB.DATUM  AND 
            PERSONALPRIS.SLUTDATUM >= TIDREGITAB.DATUM 
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE PERSONALPRIS THEN DO:
               FIND LAST PERSONALPRIS WHERE 
               PERSONALPRIS.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
               PERSONALPRIS.BEFATTNING = TIDREGITAB.OVERTIDTILL 
               USE-INDEX PSTART NO-LOCK NO-ERROR.
            END.
            IF AVAILABLE PERSONALPRIS THEN DO:
               ASSIGN
               TIDREGITAB.PRISTYP = AONRTAB.PRISTYP
               TIDREGITAB.PRIS = PERSONALPRIS.PRIS.  
            END.
         END.   
         ELSE DO:
            FIND FIRST TIMKOSTNADSTAB WHERE 
            TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
            TIMKOSTNADSTAB.PRISTYP = AONRTAB.PRISTYP 
            USE-INDEX PRISPERS NO-LOCK NO-ERROR.         
            IF AVAILABLE TIMKOSTNADSTAB THEN DO:
               ASSIGN
               TIDREGITAB.PRISTYP = AONRTAB.PRISTYP
               TIDREGITAB.PRIS = TIMKOSTNADSTAB.PRISA.  
            END.
         END.       
      END.         
      nytid = TIDREGITAB.TOTALT.
      RUN TIMSEK.P.
      ASSIGN      
      nytid = TIDREGITAB.START.
      RUN TIMSEK.P.
      ASSIGN
      regstartsek = sekunder
      nytid = TIDREGITAB.SLUT.
      RUN TIMSEK.P.
      ASSIGN
      regslutsek = sekunder
      regdatum = TIDREGITAB.DATUM.      
      RUN TOTTID.P.
      ASSIGN TIDREGITAB.TOTALT = nytid.            
      FIND tidbuff WHERE RECID(tidbuff) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.
      ASSIGN tidbuff.SLUT = regstart tidbuff.LONTILLAGG = ''   
      tidbuff.LONTILLANTAL = 0
      nytid = tidbuff.TOTALT.
      RUN TIMSEK.P.
      ASSIGN      
      nytid = tidbuff.START.
      RUN TIMSEK.P.
      ASSIGN
      regstartsek = sekunder
      nytid = tidbuff.SLUT.
      RUN TIMSEK.P.
      ASSIGN
      regslutsek = sekunder
      regdatum = tidbuff.DATUM.      
      RUN TOTTID.P.
      ASSIGN tidbuff.TOTALT = nytid.
   END.
   ELSE IF hjvar = TRUE THEN DO:
      FIND tidbuff WHERE RECID(tidbuff) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.
      ASSIGN tidbuff.SLUT = regstart tidbuff.LONTILLAGG = ''   
      tidbuff.LONTILLANTAL = 0
      nytid = tidbuff.TOTALT.
      RUN TIMSEK.P.
      ASSIGN      
      nytid = tidbuff.START.
      RUN TIMSEK.P.
      ASSIGN
      regstartsek = sekunder
      nytid = tidbuff.SLUT.
      RUN TIMSEK.P.
      ASSIGN
      regslutsek = sekunder
      regdatum = tidbuff.DATUM.      
      RUN TOTTID.P.
      ASSIGN tidbuff.TOTALT = nytid.
   END.
END.
IF TIDREGITAB.SLUT > regstart AND TIDREGITAB.SLUT LE regslut AND
TIDREGITAB.START GE regstart AND TIDREGITAB.START < regslut THEN DO TRANSACTION:
  
END. /*FINNS EJ MED I GAMMLA RESA*/
FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
USE-INDEX ANSTF NO-LOCK NO-ERROR.
DO TRANSACTION:
   outtag = PERSONALTAB.OVERTIDUTTAG.   
   /*restid men ej övertid*/
   IF Guru.Konstanter:globforetag = "SUND"  OR Guru.Konstanter:globforetag = "MISV" THEN DO:
      /*restid men ej övertid*/      
      ASSIGN outtag = "Ö". 
   END.   
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
      /*restid men ej övertid*/      
      ASSIGN outtag = "Ö". 
   END.
   
   IF outtag = 'I' THEN DO:
      IF enflerdygns = TRUE THEN DO:
         RUN TRAKTBER.P.
      END.
   END.   
   ELSE DO:
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.
      IF TIDREGITAB.ENFLERDAGS = "Endag" THEN enflerdygns = TRUE. 
      IF TIDREGITAB.ENFLERDAGS = "Flerdag" THEN enflerdygns = FALSE.
      IF enflerdygns = TRUE AND TIDREGITAB.BILFORARE = TRUE THEN DO:
         FIND FIRST UTRYCKNING WHERE UTRYCKNING.KOD = ANSTFORMTAB.KOD
         USE-INDEX UT NO-LOCK NO-ERROR.
         /* Sundsvall har ej endagsrestid*/        
         IF Guru.Konstanter:globforetag = "XSUND" OR Guru.Konstanter:globforetag = "CELPA" THEN regdatum = regdatum.
         ELSE IF UTRYCKNING.LUFT = TRUE THEN DO:
	         ASSIGN 
	         TIDREGITAB.OVERANTAL = 0
	         TIDREGITAB.LONTILLAGG = ' '
	         TIDREGITAB.LONTILLANTAL = 0
	         bilforare = TIDREGITAB.BILFORARE
	         bustart3 = TIDREGITAB.START
	         regdatum = TIDREGITAB.DATUM.
	         RUN OTOLKPR.P.
         END.
         ELSE DO:
            ASSIGN  TIDREGITAB.OVERANTAL = 0
	         TIDREGITAB.LONTILLAGG = ' ' TIDREGITAB.LONTILLANTAL = 0.      
            RUN RESTID1.P.   /*ENDAGS OCH BILF*/
         END.
         RUN TRAKTBER.P.
      END.
      IF enflerdygns = TRUE AND TIDREGITAB.BILFORARE = FALSE THEN DO:
         /* Sundsvall har ej endagsrestid*/        
         IF Guru.Konstanter:globforetag = "XSUND" OR Guru.Konstanter:globforetag = "CELPA" THEN regdatum = regdatum.
         ELSE DO:         
            ASSIGN  TIDREGITAB.OVERANTAL = 0
            TIDREGITAB.LONTILLAGG = ' ' TIDREGITAB.LONTILLANTAL = 0.
            RUN RESTID2.P.     /*ENDAGS EJ BILF*/            
         END.
         RUN TRAKTBER.P.
      END.
      IF enflerdygns = FALSE AND TIDREGITAB.BILFORARE = TRUE THEN DO:
         FIND FIRST UTRYCKNING WHERE UTRYCKNING.KOD = ANSTFORMTAB.KOD
         USE-INDEX UT NO-LOCK NO-ERROR.
         IF UTRYCKNING.LUFT = TRUE THEN DO:
            /* OM RESTID BILFÖRARE RÄKNAS SOM ÖVERTID SOM KAN KOMPAS */
	         ASSIGN  TIDREGITAB.OVERANTAL = 0
	         TIDREGITAB.LONTILLAGG = ' ' TIDREGITAB.LONTILLANTAL = 0
	         bilforare = TIDREGITAB.BILFORARE
	         bustart3 = TIDREGITAB.START
	         regdatum = TIDREGITAB.DATUM.
            RUN OTOLKPR.P.
         END.
         ELSE DO:
	         ASSIGN  TIDREGITAB.OVERANTAL = 0
	         TIDREGITAB.LONTILLAGG = ' ' TIDREGITAB.LONTILLANTAL = 0.
	         RUN RESTID3.P.        /*FLERDAGS OCH BILF*/
         END.
      END.
      IF enflerdygns = FALSE AND TIDREGITAB.BILFORARE = FALSE THEN DO:
         ASSIGN  TIDREGITAB.OVERANTAL = 0
         TIDREGITAB.LONTILLAGG = ' ' TIDREGITAB.LONTILLANTAL = 0.
         RUN RESTID4.P.        /*FLERDAGS EJ BILF*/
      END.
   END.
END.
