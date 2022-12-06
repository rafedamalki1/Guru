/*RESAW.P kontrollerar inom arbetstid ger restidsersättning  */
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}
{TIDALLT.I}
DEFINE INPUT PARAMETER varanv AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER troa AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR extratidallt.
DEFINE OUTPUT PARAMETER placerarec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID  NO-UNDO. 
DEFINE NEW SHARED VARIABLE tidtabrec AS RECID  NO-UNDO. 
DEFINE NEW SHARED VARIABLE bustart3 LIKE TIDREGITAB.START NO-UNDO.
DEFINE NEW SHARED VARIABLE bilforare AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.
DEFINE NEW SHARED VARIABLE enflerdygns AS LOGICAL FORMAT "ENDAGS/FLERDYGNS" NO-UNDO. 
DEFINE VARIABLE tidres AS INTEGER  NO-UNDO.
DEFINE VARIABLE hjtid AS INTEGER  NO-UNDO.
DEFINE VARIABLE hjtid1 AS INTEGER  NO-UNDO.
DEFINE VARIABLE hjtid2 AS INTEGER  NO-UNDO.
DEFINE VARIABLE hjtid3 AS DECIMAL  NO-UNDO.
DEFINE VARIABLE energiavt AS LOGICAL NO-UNDO.
DEFINE VARIABLE hjvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VARIABLE outtag LIKE TIDREGITAB.OVERTIDUTTAG NO-UNDO.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
ASSIGN
Guru.Konstanter:globforetag = FORETAG.FORETAG.

DEFINE BUFFER tidbuff FOR TIDREGITAB. 
FIND FIRST extratidallt NO-LOCK NO-ERROR.
FIND PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = extratidallt.PERSONALKOD NO-LOCK NO-ERROR.
persrec = RECID(PERSONALTAB). 
DO TRANSACTION:
   IF extratidallt.RECTIDVIS = ? THEN DO:
      CREATE TIDREGITAB.      
   END.
   ELSE FIND TIDREGITAB WHERE RECID(TIDREGITAB) = extratidallt.RECTIDVIS EXCLUSIVE-LOCK NO-ERROR.
   BUFFER-COPY extratidallt TO TIDREGITAB.
   tidtabrec = RECID(TIDREGITAB).
   placerarec = RECID(TIDREGITAB).
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
   IF troa = TRUE THEN DO:   
      OPEN QUERY trq FOR EACH tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      tidbuff.DATUM = regdatum AND tidbuff.START GE regstart AND tidbuff.SLUT LE
      regslut USE-INDEX PKOD NO-LOCK.      
      GET FIRST trq EXCLUSIVE-LOCK.
      DO WHILE AVAILABLE(tidbuff):  
         IF tidbuff.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
         ELSE IF tidbuff.TRAKTAMENTE = 0 THEN ASSIGN tidbuff.TRAKTAMENTE = 01.                 
         GET NEXT trq EXCLUSIVE-LOCK.
      END.         
   END.
END.
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
	      IF TIDREGITAB.SLUT > tidbuff.START AND TIDREGITAB.SLUT < tidbuff.SLUT AND 
         TIDREGITAB.START < tidbuff.START THEN DO:
	         ASSIGN 
            TIDREGITAB.SLUT = tidbuff.START.  
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
      END.
   END.                                                
   FIND tidbuff WHERE RECID(tidbuff) = tidtabrec
   EXCLUSIVE-LOCK NO-ERROR.
   FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.DATUM = regdatum AND TIDREGITAB.SLUT > regslut AND
   TIDREGITAB.START = regslut AND TIDREGITAB.PRISTYP = tidbuff.PRISTYP
   USE-INDEX PSTART EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE TIDREGITAB THEN DO:
      CREATE TIDREGITAB.
   END. 
   ASSIGN 
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
   placerarec = RECID(TIDREGITAB).
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
   FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.  
END.
IF TIDREGITAB.SLUT > regstart AND TIDREGITAB.SLUT LE regslut AND 
TIDREGITAB.START < regstart THEN DO TRANSACTION:
   FIND NEXT TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE
   USE-INDEX PSTART EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE TIDREGITAB THEN persrec = persrec.
   ELSE DO: 
      FIND tidbuff WHERE RECID(tidbuff) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.
      IF TIDREGITAB.START < tidbuff.SLUT THEN DO:
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
      END.
   END.
   FIND tidbuff WHERE RECID(tidbuff) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.
   FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.DATUM = regdatum AND TIDREGITAB.SLUT > regslut AND
   TIDREGITAB.START = regslut AND TIDREGITAB.PRISTYP = tidbuff.PRISTYP
   USE-INDEX PSTART EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE TIDREGITAB THEN DO:
      CREATE TIDREGITAB.
   END.
   ASSIGN TIDREGITAB.ENFLERDAGS = tidbuff.ENFLERDAGS
   TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD 
   TIDREGITAB.DAG = tidbuff.DAG 
   TIDREGITAB.DATUM = tidbuff.DATUM
   TIDREGITAB.VECKONUMMER = tidbuff.VECKONUMMER
   TIDREGITAB.START = regstart TIDREGITAB.SLUT = tidbuff.SLUT
   SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "RESA" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
   TIDREGITAB.OVERTIDTILL = tidbuff.OVERTIDTILL
   TIDREGITAB.TRAKTAMENTE = tidbuff.TRAKTAMENTE
   TIDREGITAB.AONR = tidbuff.AONR 
   TIDREGITAB.DELNR = tidbuff.DELNR
   TIDREGITAB.BILFORARE = tidbuff.BILFORARE 
   TIDREGITAB.OVERTIDUTTAG = tidbuff.OVERTIDUTTAG
   TIDREGITAB.PRISTYP = tidbuff.PRISTYP
   TIDREGITAB.PRIS = tidbuff.PRIS.
   placerarec = RECID(TIDREGITAB).
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
         IF Guru.Konstanter:globforetag = "CSUND" OR Guru.Konstanter:globforetag = "CELPA" THEN regdatum = regdatum.
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
         IF Guru.Konstanter:globforetag = "CSUND" OR Guru.Konstanter:globforetag = "CELPA" THEN regdatum = regdatum.
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
FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
USE-INDEX ANSTF NO-LOCK NO-ERROR.
FIND FIRST UTRYCKNING WHERE UTRYCKNING.KOD = ANSTFORMTAB.KOD
USE-INDEX UT NO-LOCK NO-ERROR.
      
IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN ASSIGN energiavt = TRUE.
IF Guru.Konstanter:globforetag = "GKAL" THEN ASSIGN energiavt = TRUE.
IF Guru.Konstanter:globforetag = "LULE" THEN ASSIGN energiavt = TRUE.
IF UTRYCKNING.RHALV = TRUE THEN DO:  
   IF energiavt = TRUE THEN DO:
      RUN tiokom_UI.          
   END.
END.

PROCEDURE tiokom_UI :
   regdatum = extratidallt.DATUM.
   RUN REGVEC.P.
   hjvnr = regvnr.
   FIND FIRST RESTIDTAB WHERE RESTIDTAB.KOD = ANSTFORMTAB.KOD 
   AND RESTIDTAB.ENFL = "ENEJB" NO-LOCK NO-ERROR.   
   ASSIGN tidres = 0.
   OPEN QUERY resq FOR EACH TIDREGITAB WHERE 
   TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND 
   TIDREGITAB.VECKONUMMER = hjvnr AND 
   TIDREGITAB.LONTILLAGG = RESTIDTAB.LONTILLAGG NO-LOCK 
   BY TIDREGITAB.DATUM BY TIDREGITAB.START.
   DO TRANSACTION:
      GET FIRST resq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):
         nytid = TIDREGITAB.LONTILLANTAL.
         RUN TIMSEK.P.
         tidres = tidres + sekunder.         
         IF tidres > 36000 THEN DO:                           
            IF tidres - sekunder < 36000 THEN DO:               
               hjtid = sekunder.
               hjtid1 = tidres - 36000.
               hjtid2 = hjtid - hjtid1.
               hjtid3 = TIDREGITAB.SLUT.               
               nytid = TIDREGITAB.START.
               RUN TIMSEK.P.
               sekunder = sekunder + hjtid2.
               RUN SEKTIM.P.
               ASSIGN 
               TIDREGITAB.SLUT = nytid.
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
               sekunder = hjtid2.
               RUN SEKTIM.P.               
               ASSIGN
               TIDREGITAB.LONTILLANTAL = nytid.
               sekunder = hjtid1.
               RUN SEKTIM.P.               
               CREATE tidbuff.
               ASSIGN
               tidbuff.TRAKTAMENTE = TIDREGITAB.TRAKTAMENTE                
               tidbuff.ENFLERDAGS = TIDREGITAB.ENFLERDAGS
               tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD 
               tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER 
               tidbuff.DATUM = TIDREGITAB.DATUM
               tidbuff.DAG = TIDREGITAB.DAG
               tidbuff.START = TIDREGITAB.SLUT
               tidbuff.SLUT = hjtid3 
               tidbuff.AONR = TIDREGITAB.AONR 
               tidbuff.DELNR = TIDREGITAB.DELNR
               tidbuff.PRISTYP = TIDREGITAB.PRISTYP
               tidbuff.PRIS = TIDREGITAB.PRIS
               tidbuff.TRAKTAUTO = TIDREGITAB.TRAKTAUTO
               tidbuff.BILFORARE = TIDREGITAB.BILFORARE 
               tidbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG
               tidbuff.OVERTIDTILL = TIDREGITAB.OVERTIDTILL
               tidbuff.PROGRAM = TIDREGITAB.PROGRAM
               tidbuff.TIDLOG = TRUE  
               tidbuff.RESMAL = TIDREGITAB.RESMAL
               tidbuff.LONTILLAGG = "081"
               tidbuff.LONTILLANTAL = nytid.
               nytid = tidbuff.START.                                   
               IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
                  ASSIGN tidbuff.LONTILLAGG = "4611".
               END.               
               IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
                  ASSIGN tidbuff.LONTILLAGG = "262".
               END.
               IF Guru.Konstanter:globforetag = "LULE" THEN DO:
                  ASSIGN tidbuff.LONTILLAGG = "771".
               END.
               RUN TIMSEK.P.
               ASSIGN
               regstartsek = sekunder
               nytid = tidbuff.SLUT.
               RUN TIMSEK.P.
               ASSIGN
               regslutsek = sekunder
               regdatum = tidbuff.DATUM   
               regvnr = tidbuff.VECKONUMMER.
               RUN TOTTID.P.
               ASSIGN tidbuff.TOTALT = nytid. 
                                                          
            END.
            ELSE IF tidres - sekunder GE 36000 THEN DO:                                             
               IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
                  ASSIGN TIDREGITAB.LONTILLAGG = "4611".
               END.               
               IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
                  ASSIGN TIDREGITAB.LONTILLAGG = "262".
               END.
               IF Guru.Konstanter:globforetag = "LULE" THEN DO:
                  ASSIGN TIDREGITAB.LONTILLAGG = "771".
               END.
            END.   
         END.   
         GET NEXT resq EXCLUSIVE-LOCK.
      END.
   END.   
END PROCEDURE.

