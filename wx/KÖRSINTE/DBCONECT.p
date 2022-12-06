/*DBCONECT.P*/
&Scoped-define NEW NEW 
{GLOBVAR2DEL1.I}
{REGVAR.I} 
{EXTRATAB.I}   
DEFINE INPUT  PARAMETER globanv AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE bilforare AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.
DEFINE NEW SHARED VARIABLE enflerdygns AS LOGICAL FORMAT "ENDAGS/FLERDYGNS" NO-UNDO.
DEFINE NEW SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE NEW SHARED VARIABLE klocka LIKE TIDREGITAB.START NO-UNDO.
DEFINE NEW SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE tidtabrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE muszval AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE vartgamla AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE NEW SHARED VARIABLE bustart3 LIKE TIDREGITAB.START NO-UNDO.

DEFINE SHARED VARIABLE datssling  AS INTEGER LABEL "SLINGA" NO-UNDO. 
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.
DEFINE VARIABLE regdagspar AS CHARACTER FORMAT "X(3)" NO-UNDO.  
DEFINE VARIABLE justtid AS DECIMAL FORMAT "99.99" NO-UNDO.   
DEFINE BUFFER tidbuff FOR TIDREGITAB.
DEFINE VARIABLE kolladatumvar AS DATE NO-UNDO.
DEFINE VARIABLE tidtabrecvar AS RECID NO-UNDO.
DEFINE VARIABLE pkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE aonruvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE delnruvar AS INTEGER NO-UNDO.
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{TIDAPPDEF.I}
{SOKDEF.I}
{AOKOLLNW2.I}
{TIDKOLW2.I}
DEFINE SHARED TEMP-TABLE tidtemp 
    FIELD FORETAG AS CHARACTER
    FIELD PERSONALKOD AS CHARACTER FORMAT "X(5)"
    FIELD AONR AS CHARACTER FORMAT "X(6)"
    FIELD DELNR AS INTEGER FORMAT "999"
    FIELD PRISTYP AS CHARACTER FORMAT "X(9)"     
    FIELD ANSTALLNING AS CHARACTER FORMAT "X(15)"     
    FIELD DATUM AS DATE  
    FIELD TRAKTAMENTE AS INTEGER FORMAT "9"
    FIELD SEKTID AS INTEGER
    FIELD START AS DECIMAL
    FIELD SLUT AS DECIMAL
    FIELD OVERTIDUTTAG AS CHARACTER FORMAT "X(1)"
    FIELD UTRYCKNING AS LOGICAL 
    FIELD PREC AS RECID
    FIELD TELNR AS CHARACTER FORMAT "X(15)"
    INDEX PKOD IS PRIMARY PERSONALKOD DATUM START SLUT AONR DELNR.   

DEFINE SHARED TEMP-TABLE tidtemp2 
    FIELD FORETAG AS CHARACTER
    FIELD PERSONALKOD AS CHARACTER FORMAT "X(5)"
    FIELD AONR AS CHARACTER FORMAT "X(6)"
    FIELD DELNR AS INTEGER FORMAT "999"
    FIELD PRISTYP AS CHARACTER FORMAT "X(9)"     
    FIELD ANSTALLNING AS CHARACTER FORMAT "X(15)"     
    FIELD DATUM AS DATE  
    FIELD TRAKTAMENTE AS INTEGER FORMAT "9"
    FIELD SEKTID AS INTEGER
    FIELD START AS DECIMAL
    FIELD SLUT AS DECIMAL
    FIELD OVERTIDUTTAG AS CHARACTER FORMAT "X(1)"
    FIELD UTRYCKNING AS LOGICAL 
    FIELD PREC AS RECID
    FIELD TELNR AS CHARACTER FORMAT "X(15)"
    INDEX PKOD IS PRIMARY PERSONALKOD DATUM START SLUT AONR DELNR.   

/*
datssling = 0   NORMALTID SAMMA DAG 
datssling = 1   ÖVRTID VILKEN DAG SOM HELST
datssling = 2   NORMAL ARBETSTID SAMMA DAG ANNAN TID
datssling = 3   NORMAL ARBETSTID FRAMÅT I TIDEN 
datssling = 9   VISA DAGENS REGISTRERINGAR
*/

FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.     
IF Guru.Konstanter:globforetag = "ELPA"  OR Guru.Konstanter:globforetag = "GRAN" OR 
Guru.Konstanter:globforetag = "GRIT" OR Guru.Konstanter:globforetag = "GKAL" OR  
Guru.Konstanter:globforetag = "GSYD" OR Guru.Konstanter:globforetag = "NORD" OR Guru.Konstanter:globforetag = "SOLE"
OR Guru.Konstanter:globforetag = "ETA" OR Guru.Konstanter:globforetag = "ESAN" OR Guru.Konstanter:globforetag = "ESMA"
OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"   OR Guru.Konstanter:globforetag = "LULE" THEN gvisatidpermanad = TRUE.
{FORESTYR.I}
FOR EACH tidtemp WHERE tidtemp.PERSONALKOD = "":
   DELETE tidtemp.
END.   
FOR EACH tidtemp WHERE tidtemp.AONR = "":
   IF datssling = 9 THEN musz = musz.   
   ELSE DELETE tidtemp.
END.
SKAPA:  
FOR EACH tidtemp USE-INDEX PKOD NO-LOCK:
   ASSIGN
   regdatum = tidtemp.DATUM 
   persrec = tidtemp.PREC.       
   RUN REGVEC.P.
   RUN REGDAG.P. 
   RUN godkoll_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      OUTPUT TO talsvarfel APPEND.
      EXPORT datssling TODAY STRING(TIME,"HH:MM:SS") "GODKÄND" SKIP          
      tidtemp.PERSONALKOD tidtemp.AONR 
      tidtemp.DATUM tidtemp.START tidtemp.SLUT
      tidtemp.TRAKTAMENTE Guru.Konstanter:globforetag.         
      OUTPUT CLOSE.
      NEXT SKAPA.
   END. 
   FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR.  
   ASSIGN
   kolladatumvar = regdatum
   tidtabrecvar = ?
   pkod = PERSONALTAB.PERSONALKOD
   aonruvar = tidtemp.AONR
   delnruvar = tidtemp.DELNR.
   RUN tidkoll_UI.
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      OUTPUT TO talsvarfel APPEND.
      EXPORT datssling TODAY STRING(TIME,"HH:MM:SS") felmeddtemp.FELMEDD SKIP          
      tidtemp.PERSONALKOD tidtemp.AONR 
      tidtemp.DATUM tidtemp.START tidtemp.SLUT
      tidtemp.TRAKTAMENTE Guru.Konstanter:globforetag.         
      OUTPUT CLOSE.
      DELETE felmeddtemp.
      NEXT SKAPA.     
   END.
   FIND FIRST TIMKOSTNADSTAB WHERE TIMKOSTNADSTAB.PERSONALKOD = tidtemp.PERSONALKOD AND
   TIMKOSTNADSTAB.PRISTYP = tidtemp.PRISTYP USE-INDEX PRISPERS NO-LOCK NO-ERROR.             

   FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = tidtemp.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.                                               
   IF datssling = 0 OR datssling = 2 OR datssling = 3 THEN DO:
      /*NORMAL ARBETSTID*/ 
      ASSIGN
      regdatum = tidtemp.DATUM 
      sekunder = tidtemp.SEKTID.
      RUN SEKTIM.P.
      justtid = nytid.      
      RUN SLUTARB.P.      
      /*JUSTTID = TELEFONSAMTALETS TIDPUNKT*/
      IF datssling = 0 AND justtid >= regslut THEN DO: 
      /*TIDPUNKT FÖR TELESAMTAL EFTER PERSONENS ARBETSTIDSLUT*/
         OUTPUT TO talsvarfel APPEND.
         EXPORT datssling TODAY STRING(TIME,"HH:MM:SS") "EFTER DIN ARBETSTID" SKIP          
         justtid
         tidtemp.PERSONALKOD tidtemp.AONR tidtemp.DATUM tidtemp.START tidtemp.SLUT
         tidtemp.TRAKTAMENTE regvnr regdatum regstart regslut regdagnamn Guru.Konstanter:globforetag.                   
         OUTPUT CLOSE.
         NEXT SKAPA.         
      END.
      IF datssling = 2 AND tidtemp.START >= regslut THEN DO:   
      /*ANGIVEN TIDPUNKT EFTER PERSONENS ARBETSTIDSLUT*/
         OUTPUT TO talsvarfel APPEND.
         EXPORT datssling TODAY STRING(TIME,"HH:MM:SS") "EFTER DIN ARBETSTID" SKIP          
         justtid
         tidtemp.PERSONALKOD tidtemp.AONR tidtemp.DATUM  tidtemp.START tidtemp.SLUT
         tidtemp.TRAKTAMENTE regvnr regdatum regstart regslut regdagnamn Guru.Konstanter:globforetag.                   
         OUTPUT CLOSE.
         NEXT SKAPA.         
      END.
      IF tidtemp.SLUT = 0 AND tidtemp.START = 0 THEN DO:
         /*OM MAN RINGER FÖRE ARBETSTIDENSSTART BLIR TIDEN = ARBETSTIDENSSTART*/
         IF justtid < regstart THEN justtid = regstart. 
      END. 
      ELSE IF tidtemp.SLUT = 0 THEN DO:      
         /*JUSTTID = ANGIVEN TIDPUNKT*/
         justtid = tidtemp.START.         
      END.                  
      /*FIXA DEN GAMLA*/       
      FIND LAST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidtemp.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE AND
      TIDREGITAB.START GE regstart AND TIDREGITAB.SLUT LE regslut
      USE-INDEX PSTART NO-LOCK NO-ERROR.
      IF AVAILABLE TIDREGITAB THEN DO TRANSACTION:
         IF datssling = 3 THEN DO:
            OUTPUT TO talsvarfel APPEND.
            EXPORT datssling TODAY STRING(TIME,"HH:MM:SS") 
            "FINNS EN REGISTRERING DENNA DAG" SKIP             
            justtid 
            tidtemp.PERSONALKOD 
            tidtemp.AONR tidtemp.DATUM  
            tidtemp.START tidtemp.SLUT tidtemp.TRAKTAMENTE
            TIDREGITAB.PROGRAM TIDREGITAB.START TIDREGITAB.SLUT TIDREGITAB.AONR              
            Guru.Konstanter:globforetag.            
            OUTPUT CLOSE.
            NEXT SKAPA.               
         END.
         IF datssling = 2 THEN DO:
         /*KOLLA OM > MÖJLIG NEJ DET BLIR 7-7 REG. OK OM DELETE TIDREGITAB NEDAN.*/
            IF TIDREGITAB.START > justtid THEN DO:
               OUTPUT TO talsvarfel APPEND.               
               EXPORT datssling TODAY STRING(TIME,"HH:MM:SS")   
               "FINNS EN REGISTRERING I DETTA INTERVALL" SKIP 
               justtid  
               tidtemp.PERSONALKOD 
               tidtemp.AONR tidtemp.DATUM 
               tidtemp.START tidtemp.SLUT tidtemp.TRAKTAMENTE 
               TIDREGITAB.PROGRAM 
               TIDREGITAB.START TIDREGITAB.SLUT TIDREGITAB.AONR                
               Guru.Konstanter:globforetag.                         
               OUTPUT CLOSE.
               NEXT SKAPA.         
            END. 
            IF TIDREGITAB.START = justtid THEN DO:
               IF justtid < regstart THEN DO: 
                  OUTPUT TO talsvarfel APPEND.               
                  EXPORT datssling TODAY STRING(TIME,"HH:MM:SS")   
                  "FINNS EN REGISTRERING I DETTA INTERVALL2" SKIP 
                  justtid  
                  tidtemp.PERSONALKOD 
                  tidtemp.AONR tidtemp.DATUM 
                  tidtemp.START tidtemp.SLUT tidtemp.TRAKTAMENTE 
                  TIDREGITAB.PROGRAM 
                  TIDREGITAB.START TIDREGITAB.SLUT TIDREGITAB.AONR                
                  Guru.Konstanter:globforetag.                         
                  OUTPUT CLOSE.
                  NEXT SKAPA.         
               END.
            END.
         END. 
         IF datssling = 0 THEN DO:
            IF TIDREGITAB.START >= justtid THEN DO:
               OUTPUT TO talsvarfel APPEND.
               EXPORT datssling TODAY STRING(TIME,"HH:MM:SS") 
               "FÖRE FÖRSTA REGISTRERINGEN" SKIP                
               justtid   
               tidtemp.PERSONALKOD 
               tidtemp.AONR tidtemp.DATUM               
               tidtemp.START tidtemp.SLUT tidtemp.TRAKTAMENTE 
               TIDREGITAB.PROGRAM TIDREGITAB.START TIDREGITAB.SLUT TIDREGITAB.AONR
               Guru.Konstanter:globforetag.
               OUTPUT CLOSE.              
               NEXT SKAPA.         
            END.                       
         END.
         tidtabrec = RECID(TIDREGITAB).
         FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.         
         IF TIDREGITAB.START <= justtid AND TIDREGITAB.SLUT >= justtid THEN DO:
            ASSIGN TIDREGITAB.SLUT = justtid.
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
            regslutsek = sekunder.
            regdatum = TIDREGITAB.DATUM.
            RUN TOTTID.P.
            ASSIGN TIDREGITAB.TOTALT = nytid.                                
            IF TIDREGITAB.SLUT = TIDREGITAB.START THEN DELETE TIDREGITAB.
         END.         
      END.
      musz = FALSE.
   END.         
   IF datssling = 1 THEN DO:     
      RUN tidstart_UI.         
      IF musz = FALSE THEN DO:  
         IF tidtemp.START > tidtemp.SLUT THEN musz = TRUE.  /*DYGNSBRYT*/       
         RUN tidslut_UI.                       
      END.
      IF musz = FALSE THEN DO:                   
         ASSIGN            
         regstart = tidtemp.START
         regslut = tidtemp.SLUT .                               
      END. 
      IF musz = TRUE AND (Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "NORD" OR Guru.Konstanter:globforetag = "ELPA") THEN DO: 
         musz = FALSE.
         RUN andra_UI. 
         IF musz = FALSE THEN DO:                   
            ASSIGN            
            regstart = tidtemp.START
            regslut = tidtemp.SLUT .                               
         END. 
      END.       
      IF musz = TRUE THEN RUN overfel_UI.                                   
   END.   
   IF musz = FALSE THEN DO TRANSACTION:  
      CREATE TIDREGITAB.
      tidtabrec = RECID(TIDREGITAB).
      ASSIGN 
      TIDREGITAB.PROGRAM = "DATASVAR" + STRING(TODAY) + globanv + tidtemp.TELNR
      TIDREGITAB.OVERTIDTILL = PERSONALTAB.BEFATTNING
      TIDREGITAB.PERSONALKOD = tidtemp.PERSONALKOD 
      TIDREGITAB.AONR = tidtemp.AONR 
      TIDREGITAB.DELNR = tidtemp.DELNR 
      TIDREGITAB.DATUM = regdatum
      TIDREGITAB.DAG = regdagnamn
      TIDREGITAB.VECKONUMMER = regvnr
      TIDREGITAB.UTRYCKNING = tidtemp.UTRYCKNING.
      IF datssling = 3 THEN DO:
         ASSIGN TIDREGITAB.SLUT = regslut
         TIDREGITAB.START = regstart. 
      END.
      ELSE DO:
         IF tidtemp.SLUT = 0 AND tidtemp.START = 0 THEN DO:
            ASSIGN TIDREGITAB.SLUT = regslut
            TIDREGITAB.START = justtid. 
         END. 
         ELSE IF tidtemp.SLUT = 0 THEN DO:
            ASSIGN TIDREGITAB.SLUT = regslut
            TIDREGITAB.START = tidtemp.START. 
         END. 
         ELSE DO:
            ASSIGN TIDREGITAB.SLUT = regslut
            TIDREGITAB.START = regstart. 
         END. 
      END.       
      IF datssling = 1 THEN DO:
         IF Guru.Konstanter:globforetag = "GRAN" 
           OR Guru.Konstanter:globforetag = "ELPA"  THEN DO:
            IF ANSTFORMTAB.KOD BEGINS "K" THEN DO:
               OPEN QUERY ttq FOR EACH tidbuff WHERE 
               tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
               tidbuff.DATUM = TIDREGITAB.DATUM AND tidbuff.BEREDSKAP NE "" USE-INDEX PSTART NO-LOCK.
               GET FIRST ttq NO-LOCK.
               DO WHILE AVAILABLE(tidbuff): 
                  IF TIDREGITAB.START GE tidbuff.BEREDSKAPSTART AND
                  TIDREGITAB.SLUT < tidbuff.BEREDSKAPSLUT THEN DO: 
                     ASSIGN TIDREGITAB.UTRYCKNING = TRUE.               
                  END.    
                 GET NEXT ttq NO-LOCK.
               END.
            END.   
         END.   
      END.

      ASSIGN
      TIDREGITAB.PRISTYP = tidtemp.PRISTYP
      TIDREGITAB.TRAKTAMENTE = tidtemp.TRAKTAMENTE
      TIDREGITAB.OVERTIDUTTAG = tidtemp.OVERTIDUTTAG.
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 1
      soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
      soktemp.SOKCHAR[2] = TIDREGITAB.PERSONALKOD
      soktemp.SOKCHAR[3] = tidtemp.PRISTYP
      soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
      soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
      {SOKANROP.I}         
      TIDREGITAB.PRIS = soktemp.SOKDECI[1]. 
      IF ANSTFORMTAB.KOD BEGINS "T" AND TIDREGITAB.OVERTIDUTTAG = "L" THEN DO:
         ASSIGN TIDREGITAB.OVERTIDUTTAG = PERSONALTAB.OVERTIDUTTAG.
      END.                      

      IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN ASSIGN TIDREGITAB.TRAKTAMENTE = 00.   
      IF TIDREGITAB.OVERTIDUTTAG = "" THEN ASSIGN TIDREGITAB.OVERTIDUTTAG = PERSONALTAB.OVERTIDUTTAG. 
      IF TIDREGITAB.OVERTIDUTTAG = ? THEN ASSIGN TIDREGITAB.OVERTIDUTTAG = PERSONALTAB.OVERTIDUTTAG.
      IF TIDREGITAB.START < TIDREGITAB.SLUT AND 
      TIDREGITAB.START >= regstart AND TIDREGITAB.SLUT <= regslut THEN DO:
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
      END.        
   END.
   RELEASE TIDREGITAB NO-ERROR.
   FOR EACH tidapptemp:
      DELETE tidapptemp.
   END.   
   DO TRANSACTION:
      CREATE tidapptemp.
      ASSIGN
      tidapptemp.FORETAG = Guru.Konstanter:globforetag
      tidapptemp.ANVANDARE = globanv + tidtemp.TELNR
      tidapptemp.RECPERS = persrec
      tidapptemp.RECTID = tidtabrec
      tidapptemp.DATUM = regdatum.      
   END.
      
   {TIDUPPIN.I}                         
   
   musz = FALSE.  
END.  
IF datssling = 9 THEN DO:
   SKAPA2:  
   FOR EACH tidtemp USE-INDEX PKOD NO-LOCK:
      ASSIGN
      regdatum = tidtemp.DATUM 
      persrec = tidtemp.PREC.       
      FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR.        
      RUN tidkoll2_UI.
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         OUTPUT TO talsvarfel APPEND.
         EXPORT datssling TODAY STRING(TIME,"HH:MM:SS") felmeddtemp.FELMEDD SKIP          
         Guru.Konstanter:globforetag.         
         OUTPUT CLOSE.
         DELETE felmeddtemp.      
      END.
   END.

END.
musz = FALSE. 
FIND FIRST tidtemp NO-LOCK NO-ERROR.  
IF AVAILABLE tidtemp THEN DO:
   IF tidtemp.FORETAG = "GRAN" THEN DO:
      IF CONNECTED("grannord") THEN DO:
         DISCONNECT grannord NO-ERROR. 
      END.
   END. 
   IF tidtemp.FORETAG = "GSYD" THEN DO:
      IF CONNECTED("gransyd") THEN DO:
         DISCONNECT gransyd NO-ERROR. 
      END.
   END.  
   ELSE IF tidtemp.FORETAG = "MALA" THEN DO:
      IF CONNECTED("mala") THEN DO:
         DISCONNECT mala NO-ERROR. 
      END.
   END.                                                         
   ELSE IF tidtemp.FORETAG = "ROSL" THEN DO:
      IF CONNECTED("rosl") THEN DO:
         DISCONNECT rosl NO-ERROR. 
      END.
   END.
   ELSE IF tidtemp.FORETAG = "GKAL" THEN DO:
      IF CONNECTED("gkal") THEN DO:
         DISCONNECT gkal NO-ERROR. 
      END.
   END.
END.

FOR EACH tidtemp:
   DELETE tidtemp.
END.    
PROCEDURE overfel_UI.   
   OUTPUT TO talsvarfel APPEND.
   IF AVAILABLE TIDREGITAB THEN DO:
      EXPORT datssling TODAY STRING(TIME,"HH:MM:SS") 
      "ÖVERTID FINNS I DETTA INTERVALL" SKIP    
      justtid 
      tidtemp.PERSONALKOD                   
      tidtemp.AONR tidtemp.DATUM 
      tidtemp.START tidtemp.SLUT tidtemp.TRAKTAMENTE tidtemp.OVERTIDUTTAG 
      TIDREGITAB.PROGRAM TIDREGITAB.START TIDREGITAB.SLUT TIDREGITAB.AONR
      Guru.Konstanter:globforetag.           
   END.
   ELSE DO: 
      EXPORT datssling TODAY STRING(TIME,"HH:MM:SS") 
      "ÖVERTID FINNS I DETTA INTERVALL" SKIP    
      justtid 
      tidtemp.PERSONALKOD "DET FINNS INGEN DATA"                  
      tidtemp.AONR tidtemp.DATUM 
      tidtemp.START tidtemp.SLUT tidtemp.TRAKTAMENTE tidtemp.OVERTIDUTTAG 
      Guru.Konstanter:globforetag.           
   END.
   OUTPUT CLOSE.
   musz = TRUE. 
END PROCEDURE.  
PROCEDURE andra_UI.   
   RUN SLUTARB.P.     
   IF regstart = regslut THEN DO:       /*HELG*/
      musz = TRUE.
      RETURN.
   END. 
   IF regstart > tidtemp.START THEN DO:       /*UTANFÖR ORDINARIE ARBSTART*/
      OPEN QUERY tidq FOR EACH TIDREGITAB WHERE 
      TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.SLUT <= regstart AND
      TIDREGITAB.TIDLOG = TRUE USE-INDEX PSTART NO-LOCK.          
      GET FIRST tidq NO-LOCK.     
      DO WHILE AVAILABLE(TIDREGITAB): /*INGEN ÖVERTID FÖRE ORDINARIE ARBETS TID*/
         musz = TRUE.
         CLOSE QUERY tidq.
         RETURN.    
      END. 
      CLOSE QUERY tidq.    
   END.
   IF regslut < tidtemp.SLUT THEN DO:         /*UTANFÖR ORDINARIE ARBSLUT*/
      OPEN QUERY tidq FOR EACH TIDREGITAB WHERE 
      TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.START >= regslut AND
      TIDREGITAB.TIDLOG = TRUE USE-INDEX PSTART NO-LOCK.          
      GET FIRST tidq NO-LOCK.     
      DO WHILE AVAILABLE(TIDREGITAB):       /*INGEN ÖVERTID EFTER ORDINARIE ARBETS TID*/
         musz = TRUE.         
         CLOSE QUERY tidq. 
         RETURN.   
      END. 
      CLOSE QUERY tidq.    
   END.   
   RUN tidstart_UI.   
   IF musz = TRUE THEN DO TRANSACTION:
      musz = FALSE.
      tidtabrec = RECID(TIDREGITAB).
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.         
      IF tidtemp.SLUT < TIDREGITAB.SLUT AND TIDREGITAB.SLUT LE regslut THEN DO:
          /*Om 7-16 finns redan och 11-13 skickas in skapas här 13-16*/
          CREATE tidbuff.
          BUFFER-COPY TIDREGITAB TO tidbuff.
          ASSIGN 
          tidbuff.START = tidtemp.SLUT.                    
          nytid = tidbuff.START.
          RUN TIMSEK.P.
          ASSIGN 
          regstartsek = sekunder
          nytid = tidbuff.SLUT.
          RUN TIMSEK.P.
          ASSIGN
          regslutsek = sekunder.
          regdatum = tidbuff.DATUM.
          RUN TOTTID.P.
          ASSIGN tidbuff.TOTALT = nytid.                                                            
      END.
      ASSIGN TIDREGITAB.SLUT = tidtemp.START.
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
      regslutsek = sekunder.
      regdatum = TIDREGITAB.DATUM.
      RUN TOTTID.P.
      ASSIGN TIDREGITAB.TOTALT = nytid.                                                            
   END. 
   RUN tidslut_UI.
   IF musz = TRUE THEN DO TRANSACTION:
      musz = FALSE.
      tidtabrec = RECID(TIDREGITAB).
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.         
      ASSIGN TIDREGITAB.START = tidtemp.SLUT.
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
      regslutsek = sekunder.
      regdatum = TIDREGITAB.DATUM.
      RUN TOTTID.P.
      ASSIGN TIDREGITAB.TOTALT = nytid.                                                      
   END.                                              
   OPEN QUERY tidq FOR EACH TIDREGITAB WHERE 
   TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.DATUM = regdatum AND TIDREGITAB.START >= tidtemp.START AND
   TIDREGITAB.TIDLOG = TRUE USE-INDEX PSTART NO-LOCK.          
   DO TRANSACTION:
      GET FIRST tidq EXCLUSIVE-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):
         IF TIDREGITAB.SLUT <= tidtemp.SLUT THEN DO:                      
            DELETE TIDREGITAB.
         END.
         GET NEXT tidq EXCLUSIVE-LOCK.
      END. 
   END.                                    
END PROCEDURE.                        
PROCEDURE godkoll_UI.
   FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR.
   IF gvisatidpermanad = TRUE THEN DO:
      FIND FIRST GODKOLL WHERE GODKOLL.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      GODKOLL.DATAR = YEAR(regdatum) AND GODKOLL.DATMAN = MONTH(regdatum) 
      USE-INDEX PKODAR NO-LOCK NO-ERROR.      
      IF NOT AVAILABLE GODKOLL THEN DO:
         musz = FALSE.
         RETURN.
      END.
      ELSE DO:        
         IF GODKOLL.DATUM < regdatum THEN DO:
            musz = FALSE.
            RETURN.
         END.
         ELSE DO:
            musz = TRUE.
            RETURN.
         END.
      END.
   END.
   ELSE DO:
      FIND FIRST TIDREGITAB WHERE 
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.VECKONUMMER = regvnr AND 
      TIDREGITAB.GODKAND NE "" USE-INDEX PVNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE TIDREGITAB THEN DO:
         musz = FALSE.
         RETURN.
      END.
      ELSE DO:        
         IF TIDREGITAB.GODKAND = "" THEN DO:
            musz = FALSE.
            RETURN.
         END.
         ELSE DO:            
            musz = TRUE.
            RETURN.
         END.
      END.
   END.
END PROCEDURE.   

PROCEDURE tidstart_UI.
   FIND FIRST TIDREGITAB WHERE 
   TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.DATUM = regdatum AND TIDREGITAB.START LE tidtemp.START AND
   TIDREGITAB.SLUT > tidtemp.START AND TIDREGITAB.TIDLOG = TRUE 
   USE-INDEX PSTART NO-LOCK NO-ERROR.
   IF NOT AVAILABLE TIDREGITAB THEN DO:
      musz = FALSE.   
   END.
   ELSE DO:    
      musz = TRUE.      
   END.
END PROCEDURE.       
PROCEDURE tidslut_UI.
   IF musz = FALSE THEN DO:                /*EJ DYGNSBRYT*/
      musz = FALSE.
      FIND FIRST TIDREGITAB WHERE 
      TIDREGITAB.PERSONAL = tidtemp.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.START < tidtemp.SLUT AND
      TIDREGITAB.SLUT >= tidtemp.SLUT AND TIDREGITAB.TIDLOG = TRUE 
      USE-INDEX PSTART NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE TIDREGITAB THEN DO: 
         FIND FIRST TIDREGITAB WHERE 
         TIDREGITAB.PERSONAL = tidtemp.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum AND TIDREGITAB.START > tidtemp.START AND
         TIDREGITAB.SLUT < tidtemp.SLUT AND TIDREGITAB.TIDLOG = TRUE 
         USE-INDEX PSTART NO-LOCK NO-ERROR.                 
         IF NOT AVAILABLE TIDREGITAB THEN DO: 
            FIND FIRST TIDREGITAB WHERE 
            TIDREGITAB.PERSONAL = tidtemp.PERSONALKOD AND
            TIDREGITAB.DATUM = regdatum AND TIDREGITAB.START < tidtemp.START AND
            TIDREGITAB.SLUT > tidtemp.SLUT AND TIDREGITAB.TIDLOG = TRUE 
            USE-INDEX PSTART NO-LOCK NO-ERROR.        
            IF NOT AVAILABLE TIDREGITAB THEN DO:    
               FIND FIRST TIDREGITAB WHERE 
               TIDREGITAB.PERSONAL = tidtemp.PERSONALKOD AND
               TIDREGITAB.DATUM = regdatum AND TIDREGITAB.START LE tidtemp.START AND
               TIDREGITAB.SLUT > tidtemp.START AND TIDREGITAB.TIDLOG = TRUE 
               USE-INDEX PSTART NO-LOCK NO-ERROR.        
               IF NOT AVAILABLE TIDREGITAB THEN DO:    
                  musz = FALSE.               
               END.  
               ELSE DO:
                  musz = TRUE.                 
               END.
            END.
            ELSE DO:
               musz = TRUE.                 
            END.
         END.
         ELSE DO:
            musz = TRUE.                 
         END.
      END.  
      ELSE DO:
         musz = TRUE.         
      END.   
   END.   
   ELSE DO:
      musz = FALSE.
      FIND FIRST TIDREGITAB WHERE 
      TIDREGITAB.PERSONAL = tidtemp.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.START GE tidtemp.START AND
      TIDREGITAB.SLUT LE 24.00 AND TIDREGITAB.TIDLOG = TRUE 
      USE-INDEX PSTART NO-LOCK NO-ERROR.
      IF NOT AVAILABLE TIDREGITAB THEN DO:
         musz = FALSE.   
         FIND FIRST TIDREGITAB WHERE 
         TIDREGITAB.PERSONAL = tidtemp.PERSONALKOD AND
         TIDREGITAB.DATUM = (regdatum + 1) AND TIDREGITAB.START GE 00.00 AND
         TIDREGITAB.SLUT LE tidtemp.SLUT AND TIDREGITAB.TIDLOG = TRUE 
         USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF NOT AVAILABLE TIDREGITAB THEN DO:
            musz = FALSE.                 
         END.
         ELSE DO:
            musz = TRUE.            
         END.    
      END.  
      ELSE DO:
         musz = TRUE.               
      END.     
   END.                
END PROCEDURE.
