/*FLUTL.P UTLANDSTRAKTAMENTE */
/*DEFINE SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/

DEFINE SHARED VARIABLE regdatum LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE SHARED VARIABLE regvnr LIKE TIDREGITAB.VECKONUMMER NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID  NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE bdatum LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE SHARED VARIABLE avdatum LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE SHARED VARIABLE reddatum LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE SHARED VARIABLE enflerdygns AS LOGICAL FORMAT "ENDAGS/FLERDYGNS" NO-UNDO.
DEFINE SHARED VARIABLE bilforare AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.
DEFINE SHARED VARIABLE startresa LIKE TIDREGITAB.START NO-UNDO.
DEFINE SHARED VARIABLE slutresa LIKE TIDREGITAB.START NO-UNDO.
DEFINE SHARED VARIABLE uland LIKE LAND.LAND NO-UNDO.
DEFINE SHARED VARIABLE procent AS DECIMAL NO-UNDO.
DEFINE VARIABLE matlart AS CHARACTER NO-UNDO.
&Scoped-define NEW
{RESDEF.I}
/*DEFINE SHARED TEMP-TABLE kosters
   FIELD MPERSONALKOD LIKE PERSONALTAB.PERSONALKOD
   FIELD MDAG LIKE TIDREGITAB.DAG
   FIELD MVECKONUMMER LIKE TIDREGITAB.VECKONUMMER
   FIELD MDATUM LIKE TIDREGITAB.DATUM
   FIELD KR LIKE TIDREGITAB.LONTILLANTAL.
DEFINE SHARED TEMP-TABLE maltidfil
   FIELD MPERSONALKOD LIKE MALTIDTAB.PERSONALKOD
   FIELD MDAG LIKE MALTIDTAB.DAG
   FIELD MVECKONUMMER LIKE MALTIDTAB.VECKONUMMER
   FIELD MDATUM LIKE MALTIDTAB.DATUM
   FIELD MFRU LIKE MALTIDTAB.FRU
   FIELD MLUN LIKE MALTIDTAB.FRU
   FIELD MMID LIKE MALTIDTAB.FRU.*/
   
DEFINE VARIABLE maltid LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.
DEFINE VARIABLE skatt LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.
DEFINE VARIABLE skfri LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.


DEFINE BUFFER lontillbuff FOR LONTILL.
DEFINE BUFFER tidbuff FOR TIDREGITAB.
DEFINE SHARED VARIABLE FILL-IN-FRIMAT AS LOGICAL FORMAT "JA/NEJ":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
       NO-UNDO.
DEFINE  SHARED VARIABLE FILL-IN-REDTRAKT AS LOGICAL FORMAT "JA/NEJ":U INITIAL NO 
     LABEL "5. Reducerat traktamente                         ?" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1
       NO-UNDO.     
          
FIND tidbuff WHERE RECID(tidbuff) = tidtabrec NO-LOCK NO-ERROR.
FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR.
FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
USE-INDEX ANSTF NO-LOCK NO-ERROR.     
IF tidbuff.DATUM = bdatum THEN DO:            
   DO:             
      FIND FIRST LONFLER WHERE LONFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
      LONFLER.RESSTART LE startresa AND
      LONFLER.RESSTART2 GE startresa AND LONFLER.DAGTYP = "UTLANDU"
      USE-INDEX FLON NO-LOCK NO-ERROR.
   END.   
   IF NOT AVAILABLE LONFLER THEN persrec = persrec.
   ELSE DO:
      FIND FIRST LONTILL WHERE LONTILL.KOD = ANSTFORMTAB.KOD AND 
      LONTILL.LONTILLAGG = LONFLER.LONTILLAGG NO-LOCK NO-ERROR.
      IF LONFLER.LONTILLAGG NE '' THEN DO:
         FIND FIRST LAND WHERE LAND.LAND = uland NO-LOCK NO-ERROR.
         procent = 0.                                    
         FIND FIRST maltidfil WHERE maltidfil.MDATUM = tidbuff.DATUM NO-LOCK NO-ERROR.
         IF AVAILABLE maltidfil THEN DO:                           
            IF  maltidfil.MFRU = TRUE THEN procent = procent + 0.15.
            IF  maltidfil.MLUN = TRUE THEN procent = procent + 0.35.
            IF  maltidfil.MMID = TRUE THEN procent = procent + 0.35.                        
         END.             
         CREATE TIDREGITAB.
         ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
         SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "FLUTL" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv         
         TIDREGITAB.DAG = tidbuff.DAG
         TIDREGITAB.VECKONUMMER = tidbuff.VECKONUMMER
         TIDREGITAB.START = 7.00
         TIDREGITAB.SLUT = 7.00
         TIDREGITAB.TIDLOG = FALSE
         TIDREGITAB.LONTILLAGG = LONFLER.LONTILLAGG
         TIDREGITAB.LONTILLANTAL = LONFLER.LONTILLANTAL *  LAND.BELOPP
         TIDREGITAB.AONR = tidbuff.AONR
         TIDREGITAB.DELNR = tidbuff.DELNR
         TIDREGITAB.BILFORARE = tidbuff.BILFORARE
         TIDREGITAB.ENFLERDAGS = tidbuff.ENFLERDAGS
         TIDREGITAB.DATUM = tidbuff.DATUM.   
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:            
            ASSIGN matlart = "844".               
         END.   
         IF Guru.Konstanter:globforetag = "LULE" THEN ASSIGN matlart = "844".
         IF Guru.Konstanter:globforetag = "JSBF" THEN ASSIGN matlart = "844".                           
         IF Guru.Konstanter:globforetag = "GKAL" THEN ASSIGN matlart = "MUTL".
         IF Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "elpa" THEN ASSIGN matlart = "5150".              
         IF matlart NE ""  AND procent > 0 THEN DO:            
            CREATE TIDREGITAB.
            ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
            SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "FLUTL" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
            TIDREGITAB.DAG = tidbuff.DAG
            TIDREGITAB.VECKONUMMER = tidbuff.VECKONUMMER
            TIDREGITAB.START = 7.00
            TIDREGITAB.SLUT = 7.00
            TIDREGITAB.TIDLOG = FALSE
            TIDREGITAB.LONTILLAGG = matlart
            TIDREGITAB.LONTILLANTAL = LONFLER.LONTILLANTAL *  LAND.BELOPP * procent
            TIDREGITAB.AONR = tidbuff.AONR
            TIDREGITAB.DELNR = tidbuff.DELNR
            TIDREGITAB.BILFORARE = tidbuff.BILFORARE
            TIDREGITAB.ENFLERDAGS = tidbuff.ENFLERDAGS
            TIDREGITAB.DATUM = tidbuff.DATUM.   
         END.      
      END.
   END.
END.
IF tidbuff.DATUM > bdatum AND tidbuff.DATUM < avdatum THEN DO:
   /* MELLANDAGAR */  
   FIND FIRST LONFLER WHERE LONFLER.DAGTYP = "UTLAND" AND
   LONFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL USE-INDEX FLON NO-LOCK NO-ERROR.
   IF NOT AVAILABLE LONFLER THEN persrec = persrec.
   ELSE DO:
      FIND FIRST LONTILL WHERE LONTILL.KOD = ANSTFORMTAB.KOD AND 
      LONTILL.LONTILLAGG = LONFLER.LONTILLAGG NO-LOCK NO-ERROR.
      FIND FIRST LAND WHERE LAND.LAND = uland NO-LOCK NO-ERROR.               
      procent = 0.               
      FIND FIRST maltidfil WHERE maltidfil.MDATUM = tidbuff.DATUM NO-LOCK NO-ERROR.
      IF AVAILABLE maltidfil THEN DO:                     
         IF  maltidfil.MFRU = TRUE THEN procent = procent + 0.15.
         IF  maltidfil.MLUN = TRUE THEN procent = procent + 0.35.
         IF  maltidfil.MMID = TRUE THEN procent = procent + 0.35.               
      END.               
      CREATE TIDREGITAB.
      ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
      SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "FLUTL" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
      TIDREGITAB.DAG = tidbuff.DAG
      TIDREGITAB.VECKONUMMER = tidbuff.VECKONUMMER
      TIDREGITAB.START = 7.00
      TIDREGITAB.SLUT = 7.00
      TIDREGITAB.TIDLOG = FALSE
      TIDREGITAB.LONTILLAGG = LONFLER.LONTILLAGG
      TIDREGITAB.LONTILLANTAL = LONFLER.LONTILLANTAL *  LAND.BELOPP
      TIDREGITAB.AONR = tidbuff.AONR
      TIDREGITAB.DELNR = tidbuff.DELNR
      TIDREGITAB.BILFORARE = tidbuff.BILFORARE
      TIDREGITAB.ENFLERDAGS = tidbuff.ENFLERDAGS
      TIDREGITAB.DATUM = tidbuff.DATUM.   
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN ASSIGN matlart = "844".               
      IF Guru.Konstanter:globforetag = "LULE" THEN ASSIGN matlart = "844".
      IF Guru.Konstanter:globforetag = "JSBF" THEN ASSIGN matlart = "844".               
      IF Guru.Konstanter:globforetag = "GKAL" THEN ASSIGN matlart = "MUTL".
      IF Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "elpa" THEN ASSIGN matlart = "5150".               
      IF matlart NE "" AND procent > 0 THEN DO:            
         CREATE TIDREGITAB.
         ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
         SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "FLUTL" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
         TIDREGITAB.DAG = tidbuff.DAG
         TIDREGITAB.VECKONUMMER = tidbuff.VECKONUMMER
         TIDREGITAB.START = 7.00
         TIDREGITAB.SLUT = 7.00
         TIDREGITAB.TIDLOG = FALSE
         TIDREGITAB.LONTILLAGG = matlart
         TIDREGITAB.LONTILLANTAL = LONFLER.LONTILLANTAL *  LAND.BELOPP * procent
         TIDREGITAB.AONR = tidbuff.AONR
         TIDREGITAB.DELNR = tidbuff.DELNR
         TIDREGITAB.BILFORARE = tidbuff.BILFORARE
         TIDREGITAB.ENFLERDAGS = tidbuff.ENFLERDAGS
         TIDREGITAB.DATUM = tidbuff.DATUM.   
      END.      
       
   END.
END.        
IF tidbuff.DATUM = avdatum THEN DO:      
   FIND FIRST LONFLER WHERE LONFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
   LONFLER.RESSTART LE slutresa AND
   LONFLER.RESSTART2 GE slutresa AND LONFLER.DAGTYP = "UTLANDH"
   USE-INDEX FLON NO-LOCK NO-ERROR.
   IF NOT AVAILABLE LONFLER THEN persrec = persrec.
   ELSE DO:
      FIND FIRST LONTILL WHERE LONTILL.KOD = ANSTFORMTAB.KOD AND 
      LONTILL.LONTILLAGG = LONFLER.LONTILLAGG NO-LOCK NO-ERROR.      
      IF LONFLER.LONTILLAGG NE '' THEN DO:         
         FIND FIRST LAND WHERE LAND.LAND = uland NO-LOCK NO-ERROR.                  
         procent = 0.                 
         FIND FIRST maltidfil WHERE maltidfil.MDATUM = tidbuff.DATUM NO-LOCK NO-ERROR.
         IF AVAILABLE maltidfil THEN DO:                        
            IF  maltidfil.MFRU = TRUE THEN procent = procent + 0.15.
            IF  maltidfil.MLUN = TRUE THEN procent = procent + 0.35.
            IF  maltidfil.MMID = TRUE THEN procent = procent + 0.35.                       
         END.       
         CREATE TIDREGITAB.
         ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
         SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "FLUTL" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
         TIDREGITAB.DAG = tidbuff.DAG
         TIDREGITAB.VECKONUMMER = tidbuff.VECKONUMMER
         TIDREGITAB.START = 7.00
         TIDREGITAB.SLUT = 7.00
         TIDREGITAB.TIDLOG = FALSE
         TIDREGITAB.LONTILLAGG = LONFLER.LONTILLAGG
         TIDREGITAB.LONTILLANTAL = LONFLER.LONTILLANTAL *  LAND.BELOPP
         TIDREGITAB.AONR = tidbuff.AONR
         TIDREGITAB.DELNR = tidbuff.DELNR
         TIDREGITAB.BILFORARE = tidbuff.BILFORARE
         TIDREGITAB.ENFLERDAGS = tidbuff.ENFLERDAGS
         TIDREGITAB.DATUM = tidbuff.DATUM.   
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN ASSIGN matlart = "844".               
         IF Guru.Konstanter:globforetag = "LULE" THEN ASSIGN matlart = "844".
         IF Guru.Konstanter:globforetag = "JSBF" THEN ASSIGN matlart = "844".               
         IF Guru.Konstanter:globforetag = "GKAL" THEN ASSIGN matlart = "MUTL".
         IF Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "elpa" THEN ASSIGN matlart = "5150".              
         IF matlart NE "" AND procent > 0 THEN DO:            
            CREATE TIDREGITAB.
            ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
            SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "FLUTL" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
            TIDREGITAB.DAG = tidbuff.DAG
            TIDREGITAB.VECKONUMMER = tidbuff.VECKONUMMER
            TIDREGITAB.START = 7.00
            TIDREGITAB.SLUT = 7.00
            TIDREGITAB.TIDLOG = FALSE
            TIDREGITAB.LONTILLAGG = matlart
            TIDREGITAB.LONTILLANTAL = LONFLER.LONTILLANTAL *  LAND.BELOPP * procent
            TIDREGITAB.AONR = tidbuff.AONR
            TIDREGITAB.DELNR = tidbuff.DELNR
            TIDREGITAB.BILFORARE = tidbuff.BILFORARE
            TIDREGITAB.ENFLERDAGS = tidbuff.ENFLERDAGS
            TIDREGITAB.DATUM = tidbuff.DATUM.   
         END.         
      END.
   END.
END.
