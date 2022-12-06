/*FLUTL2.P UTLANDSTRAKTAMENTE */
/*DEFINE SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/

DEFINE SHARED VARIABLE regdatum LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE SHARED VARIABLE regvnr LIKE TIDREGITAB.VECKONUMMER NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID  NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.  
DEFINE VARIABLE hjrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE bdatum LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE SHARED VARIABLE avdatum LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE SHARED VARIABLE reddatum LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE SHARED VARIABLE uland LIKE LAND.LAND NO-UNDO.
DEFINE VARIABLE matlart AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE enflerdygns AS LOGICAL FORMAT "ENDAGS/FLERDYGNS" NO-UNDO.
DEFINE SHARED VARIABLE bilforare AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.
DEFINE SHARED VARIABLE resrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE procent AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE startresa LIKE TIDREGITAB.START NO-UNDO.
DEFINE SHARED VARIABLE slutresa LIKE TIDREGITAB.START NO-UNDO.
DEFINE BUFFER tidbuff FOR TIDREGITAB.
DEFINE SHARED VARIABLE FILL-IN-FRIMAT AS LOGICAL FORMAT "JA/NEJ":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
       NO-UNDO.
DEFINE SHARED VARIABLE FILL-IN-REDTRAKT AS LOGICAL FORMAT "JA/NEJ":U INITIAL NO 
     LABEL "5. Reducerat traktamente                         ?" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1
       NO-UNDO.     
&Scoped-define NEW
{RESDEF.I}        
/*DEFINE SHARED TEMP-TABLE respers    
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD VECKONUMMER LIKE TIDREGITAB.VECKONUMMER
   FIELD DATUM LIKE TIDREGITAB.DATUM
   FIELD DAG LIKE TIDREGITAB.DAG
   FIELD START LIKE TIDREGITAB.START
   FIELD SLUT LIKE TIDREGITAB.SLUT 
   FIELD PRIS AS DECIMAL
   FIELD PRISTYP AS CHARACTER
   FIELD NATTRAKT AS LOGICAL FORMAT "JA/NEJ" LABEL "NATT TRAKT"
   FIELD OVERTIDUTTAG LIKE PERSONALTAB.OVERTIDUTTAG 
   FIELD BILFORARE LIKE TIDREGITAB.BILFORARE
   FIELD ENFLERDAGS LIKE TIDREGITAB.ENFLERDAGS  
   FIELD TIDREC AS RECID
   INDEX RESPERS IS PRIMARY DATUM START SLUT ASCENDING.
DEFINE SHARED TEMP-TABLE kosters
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

     
FIND respers WHERE RECID(respers) = resrec NO-LOCK NO-ERROR.
FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR.
FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
USE-INDEX ANSTF NO-LOCK NO-ERROR.
IF respers.DATUM = bdatum THEN DO:
   hjrec = RECID(respers).
   FIND NEXT respers NO-LOCK NO-ERROR.
   IF NOT AVAILABLE respers THEN DO:
      FIND respers WHERE RECID(respers) = resrec NO-LOCK NO-ERROR.
   END.   
   ELSE IF respers.DATUM = bdatum AND respers.START NE respers.SLUT THEN DO:
      hjrec = RECID(respers).
   END.  
   ELSE DO:
      FIND respers WHERE RECID(respers) = resrec NO-LOCK NO-ERROR.
   END.                                                       
   FIND FIRST LONFLER WHERE LONFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
   LONFLER.RESSTART LE startresa AND
   LONFLER.RESSTART2 GE startresa AND LONFLER.DAGTYP = "UTLANDU"
   USE-INDEX FLON NO-LOCK NO-ERROR.

   IF NOT AVAILABLE LONFLER THEN persrec = persrec.
   ELSE DO:
      FIND respers WHERE RECID(respers) = hjrec NO-LOCK NO-ERROR. 
      FIND FIRST LONTILL WHERE LONTILL.KOD = ANSTFORMTAB.KOD AND 
      LONTILL.LONTILLAGG = LONFLER.LONTILLAGG NO-LOCK NO-ERROR.
      IF LONFLER.LONTILLAGG NE '' THEN DO:
         FIND FIRST LAND WHERE LAND.LAND = uland NO-LOCK NO-ERROR.                  
         procent = 0.                 
         FIND FIRST maltidfil WHERE maltidfil.MDATUM = respers.DATUM NO-LOCK NO-ERROR.
         IF AVAILABLE maltidfil THEN DO:                           
            IF  maltidfil.MFRU = TRUE THEN procent = procent + 0.15.
            IF  maltidfil.MLUN = TRUE THEN procent = procent + 0.35.
            IF  maltidfil.MMID = TRUE THEN procent = procent + 0.35.                        
         END.          
         CREATE TIDREGITAB.
         ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
         SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "FLUTL2" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
         TIDREGITAB.DAG = respers.DAG
         TIDREGITAB.VECKONUMMER = respers.VECKONUMMER
         TIDREGITAB.START = 7.00
         TIDREGITAB.SLUT = 7.00
         TIDREGITAB.TIDLOG = FALSE
         TIDREGITAB.LONTILLAGG = LONFLER.LONTILLAGG
         TIDREGITAB.LONTILLANTAL = LONFLER.LONTILLANTAL *  LAND.BELOPP
         TIDREGITAB.AONR = respers.AONR
         TIDREGITAB.DELNR = respers.DELNR
         TIDREGITAB.BILFORARE = respers.BILFORARE         
         TIDREGITAB.ENFLERDAGS =  "Flerdagu"
         TIDREGITAB.DATUM = respers.DATUM.   
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:            
            ASSIGN matlart = "844".               
         END.
         IF Guru.Konstanter:globforetag = "LULE" THEN  ASSIGN matlart = "844".
         IF Guru.Konstanter:globforetag = "JSBF" THEN  ASSIGN matlart = "844".                           
         IF Guru.Konstanter:globforetag = "GKAL" THEN ASSIGN matlart = "MUTL".                           
         IF Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "elpa" THEN ASSIGN matlart = "5150".                           
         IF matlart NE "" AND procent > 0 THEN DO:            
            CREATE TIDREGITAB.
            ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
            SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "FLUTL2" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
            TIDREGITAB.DAG = respers.DAG
            TIDREGITAB.VECKONUMMER = respers.VECKONUMMER
            TIDREGITAB.START = 7.00
            TIDREGITAB.SLUT = 7.00
            TIDREGITAB.TIDLOG = FALSE
            TIDREGITAB.LONTILLAGG = matlart
            TIDREGITAB.LONTILLANTAL = LONFLER.LONTILLANTAL *  LAND.BELOPP * procent
            TIDREGITAB.AONR = respers.AONR
            TIDREGITAB.DELNR = respers.DELNR
            TIDREGITAB.BILFORARE = respers.BILFORARE            
            TIDREGITAB.ENFLERDAGS =  "Flerdagu"
            TIDREGITAB.DATUM = respers.DATUM.   
         END.         
         FIND respers WHERE RECID(respers) = resrec NO-LOCK NO-ERROR.
      END.
   END.
END.
IF respers.DATUM > bdatum AND respers.DATUM < avdatum THEN DO:
   /* MELLANDAGAR */ 
   FIND FIRST LONFLER WHERE LONFLER.DAGTYP = "UTLAND" AND
   LONFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL USE-INDEX FLON NO-LOCK NO-ERROR.
   IF NOT AVAILABLE LONFLER THEN persrec = persrec.
   ELSE DO:
      FIND FIRST LONTILL WHERE LONTILL.KOD = ANSTFORMTAB.KOD AND 
      LONTILL.LONTILLAGG = LONFLER.LONTILLAGG NO-LOCK NO-ERROR.            
      DO:
         FIND FIRST LAND WHERE LAND.LAND = uland NO-LOCK NO-ERROR.                  
         procent = 0.                  
         FIND FIRST maltidfil WHERE maltidfil.MDATUM = respers.DATUM NO-LOCK NO-ERROR.
         IF AVAILABLE maltidfil THEN DO:                        
            IF  maltidfil.MFRU = TRUE THEN procent = procent + 0.15.
            IF  maltidfil.MLUN = TRUE THEN procent = procent + 0.35.
            IF  maltidfil.MMID = TRUE THEN procent = procent + 0.35.                        
         END.         
         CREATE TIDREGITAB.
         ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
         SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "FLUTL2" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
         TIDREGITAB.DAG = respers.DAG
         TIDREGITAB.VECKONUMMER = respers.VECKONUMMER
         TIDREGITAB.START = 7.00
         TIDREGITAB.SLUT = 7.00
         TIDREGITAB.TIDLOG = FALSE
         TIDREGITAB.LONTILLAGG = LONFLER.LONTILLAGG
         TIDREGITAB.LONTILLANTAL = LONFLER.LONTILLANTAL *  LAND.BELOPP
         TIDREGITAB.AONR = respers.AONR
         TIDREGITAB.DELNR = respers.DELNR
         TIDREGITAB.BILFORARE = respers.BILFORARE         
         TIDREGITAB.ENFLERDAGS =  "Flerdagu"
         TIDREGITAB.DATUM = respers.DATUM.   
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:            
            ASSIGN matlart = "844".               
         END.
         IF Guru.Konstanter:globforetag = "LULE" THEN ASSIGN matlart = "844".
         IF Guru.Konstanter:globforetag = "JSBF" THEN  ASSIGN matlart = "844".                          
         IF Guru.Konstanter:globforetag = "GKAL" THEN ASSIGN matlart = "MUTL".      
         IF Guru.Konstanter:globforetag = "elpa" OR Guru.Konstanter:globforetag = "SKOK"  THEN ASSIGN matlart = "5150".                           
         IF matlart NE "" AND procent > 0 THEN DO:            
            CREATE TIDREGITAB.
            ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
            SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "FLUTL2" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
            TIDREGITAB.DAG = respers.DAG
            TIDREGITAB.VECKONUMMER = respers.VECKONUMMER
            TIDREGITAB.START = 7.00
            TIDREGITAB.SLUT = 7.00
            TIDREGITAB.TIDLOG = FALSE
            TIDREGITAB.LONTILLAGG = matlart
            TIDREGITAB.LONTILLANTAL = LONFLER.LONTILLANTAL *  LAND.BELOPP * procent
            TIDREGITAB.AONR = respers.AONR
            TIDREGITAB.DELNR = respers.DELNR
            TIDREGITAB.BILFORARE = respers.BILFORARE            
            TIDREGITAB.ENFLERDAGS =  "Flerdagu"
            TIDREGITAB.DATUM = respers.DATUM.   
         END.         
      END.   
   END.
END.        

IF respers.DATUM = avdatum THEN DO:         
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
         FIND FIRST maltidfil WHERE maltidfil.MDATUM = respers.DATUM NO-LOCK NO-ERROR.
         IF AVAILABLE maltidfil THEN DO:                           
            IF  maltidfil.MFRU = TRUE THEN procent = procent + 0.15.
            IF  maltidfil.MLUN = TRUE THEN procent = procent + 0.35.
            IF  maltidfil.MMID = TRUE THEN procent = procent + 0.35.                        
         END.          
         CREATE TIDREGITAB.
         ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
         SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "FLUTL2" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
         TIDREGITAB.DAG = respers.DAG
         TIDREGITAB.VECKONUMMER = respers.VECKONUMMER
         TIDREGITAB.START = 7.00
         TIDREGITAB.SLUT = 7.00
         TIDREGITAB.TIDLOG = FALSE
         TIDREGITAB.LONTILLAGG = LONFLER.LONTILLAGG
         TIDREGITAB.LONTILLANTAL = LONFLER.LONTILLANTAL *  LAND.BELOPP
         TIDREGITAB.AONR = respers.AONR
         TIDREGITAB.DELNR = respers.DELNR
         TIDREGITAB.BILFORARE = respers.BILFORARE         
         TIDREGITAB.ENFLERDAGS =  "Flerdagu"
         TIDREGITAB.DATUM = respers.DATUM.   
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:            
            ASSIGN matlart = "844".               
         END.
         IF Guru.Konstanter:globforetag = "LULE" THEN ASSIGN matlart = "844". 
         IF Guru.Konstanter:globforetag = "JSBF" THEN  ASSIGN matlart = "844".              
         IF Guru.Konstanter:globforetag = "GKAL" THEN ASSIGN matlart = "MUTL".
         IF Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "elpa" THEN ASSIGN matlart = "5150".  
         IF matlart NE "" AND procent > 0 THEN DO:            
            CREATE TIDREGITAB.
            ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
            SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "FLUTL2" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
            TIDREGITAB.DAG = respers.DAG
            TIDREGITAB.VECKONUMMER = respers.VECKONUMMER
            TIDREGITAB.START = 7.00
            TIDREGITAB.SLUT = 7.00
            TIDREGITAB.TIDLOG = FALSE
            TIDREGITAB.LONTILLAGG = matlart
            TIDREGITAB.LONTILLANTAL = LONFLER.LONTILLANTAL *  LAND.BELOPP * procent
            TIDREGITAB.AONR = respers.AONR
            TIDREGITAB.DELNR = respers.DELNR
            TIDREGITAB.BILFORARE = respers.BILFORARE            
            TIDREGITAB.ENFLERDAGS =  "Flerdagu"
            TIDREGITAB.DATUM = respers.DATUM.   
         END.
      END.
   END.
END.
