/* AUREG.P AUTOMATREGISTRERING   */
/* OM startaprog = FALSE DÅ KÖRS AUREG.P PERSISTENT SE EJAUREG*/

DEFINE NEW SHARED VARIABLE varforetypval AS INTEGER EXTENT 100 NO-UNDO.     
DEFINE NEW SHARED VARIABLE varforetypchar AS CHARACTER EXTENT 100 NO-UNDO.     
DEFINE NEW SHARED VARIABLE globanv AS CHARACTER NO-UNDO. 
/*{SOKDEF.I}*/
DEFINE NEW SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.

DEFINE NEW SHARED VARIABLE appcon AS LOGICAL NO-UNDO.

DEFINE NEW SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE printer LIKE SKRIVARDEF.UTSKRIFTSTYP NO-UNDO.
DEFINE NEW SHARED VARIABLE printer1 LIKE SKRIVARDEF.SKRIVARID NO-UNDO.
DEFINE NEW SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
/*DEFINE NEW SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/
DEFINE NEW SHARED VARIABLE globlos LIKE ANVANDARE.AV-LOSEN NO-UNDO.
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE NEW SHARED VARIABLE plusaonr LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE NEW SHARED VARIABLE plusdnr LIKE TIDREGITAB.DELNR NO-UNDO.
DEFINE NEW SHARED VARIABLE plusrec AS RECID  NO-UNDO.
DEFINE NEW SHARED VARIABLE plustidrec AS RECID  NO-UNDO.
DEFINE NEW SHARED VARIABLE plustid AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE plusdval LIKE PROGVAL.JANEJ NO-UNDO.
DEFINE NEW SHARED VARIABLE succelval LIKE PROGVAL.JANEJ NO-UNDO.     
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE NEW SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE NEW SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE bilforare AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.
DEFINE NEW SHARED VARIABLE enflerdygns AS LOGICAL FORMAT "ENDAGS/FLERDYGNS" NO-UNDO.
DEFINE NEW SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE NEW SHARED VARIABLE klocka LIKE TIDREGITAB.START NO-UNDO.
DEFINE NEW SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE tidtabrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE vartgamla AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE NEW SHARED VARIABLE frustarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE fruslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffestart AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffeslut AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchstarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE regtotalt LIKE TIDREGITAB.TOTALT NO-UNDO.   
DEFINE NEW SHARED VARIABLE regstartsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE regslutsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE bustart3 LIKE TIDREGITAB.START NO-UNDO. 
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE VARIABLE utvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE kolldatum AS DATE NO-UNDO.
DEFINE VARIABLE lunorm AS INTEGER NO-UNDO.
DEFINE VARIABLE lufl AS INTEGER NO-UNDO.
DEFINE VARIABLE fdatum AS DATE NO-UNDO.
DEFINE TEMP-TABLE ftemp   
   FIELD DATUM AS DATE
   FIELD DAG AS CHARACTER
   FIELD OSTART AS DECIMAL 
   FIELD OSLUT AS DECIMAL
   FIELD FSTART AS DECIMAL 
   FIELD FSLUT AS DECIMAL 
   FIELD MKFLEX AS INTEGER
   FIELD MKTFLEX AS DECIMAL
   FIELD TFLEX AS INTEGER
   FIELD TOTFLEX AS DECIMAL
   FIELD LFLEX AS DECIMAL
   FIELD MFLEX AS DECIMAL
   FIELD KFLEX AS DECIMAL.

DEFINE TEMP-TABLE ekoforst
   FIELD PERSONALKOD LIKE EKRAPPRESULT.EPERSONALKOD 
   FIELD AONR LIKE EKRAPPRESULT.EPROJEKT 
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD ORG LIKE EKRAPPRESULT.EORG  
   FIELD PRIS LIKE TIDREGITAB.PRIS   
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP    
   FIELD BELOPP AS DECIMAL
   FIELD OBELOPP AS DECIMAL
   FIELD ANTAL AS DECIMAL
   FIELD OTIMMAR AS DECIMAL             
 /*  FIELD LONTILLAGG LIKE EKRAPPRESULT.ELONTILLAGG 
   FIELD LONTILLANTAL LIKE EKRAPPRESULT.ELONTILLANTAL    */
   FIELD LONBELOPP LIKE EKRAPPRESULT.ELONBELOPP  
   FIELD OANT1 LIKE TIDREGITAB.OANT1 
   FIELD OANT2 LIKE TIDREGITAB.OANT2 
   FIELD OANT3 LIKE TIDREGITAB.OANT3 
   FIELD OKOD1 LIKE TIDREGITAB.OKOD1 
   FIELD OKOD2 LIKE TIDREGITAB.OKOD2 
   FIELD OKOD3 LIKE TIDREGITAB.OKOD3   
   FIELD TRAKTKOD LIKE TIDREGITAB.TRAKTKOD 
   FIELD TRAKTANTAL LIKE TIDREGITAB.TRAKTANTAL      
   FIELD TBELOPP LIKE EKRAPPRESULT.EBELOPP
   INDEX PERSAO IS PRIMARY PERSONALKOD AONR DELNR ASCENDING
   INDEX ORGAONR ORG AONR DELNR ASCENDING.
DEFINE TEMP-TABLE trakt
   FIELD TRAKTKOD LIKE TRAKTATAB.TRAKTKOD
   FIELD ERSATTNING LIKE TRAKTATAB.ERSATTNING
   INDEX TRAKT IS PRIMARY TRAKTKOD ASCENDING .
DEFINE TEMP-TABLE lon
   FIELD LONTILLAGG LIKE LONTILL.LONTILLAGG
   FIELD ERSATTNING LIKE LONTILL.ERSATTNING
   INDEX LONT IS PRIMARY LONTILLAGG ASCENDING.   
DEFINE TEMP-TABLE slutsum           
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD PRIS LIKE TIDREGITAB.PRIS     
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP 
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR 
   FIELD ANTAL AS DECIMAL LABEL "TIMMAR"          
   FIELD OTIMMAR AS DECIMAL LABEL "OTIMMAR"
   FIELD BELOPP AS DECIMAL LABEL "ARBKOSTNAD"           
   FIELD OBELOPP AS DECIMAL LABEL "Ö-KOSTNAD"  
   FIELD OANTAL AS DECIMAL  LABEL "Ö-ANTAL"         
   FIELD TBELOPP AS DECIMAL LABEL "T-KOSTNAD"
   FIELD TANTAL AS DECIMAL  LABEL "T-ANTAL"         
   INDEX AONR IS PRIMARY AONR DELNR ASCENDING.  
DEFINE TEMP-TABLE oversum   
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD PRIS LIKE TIDREGITAB.PRIS     
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP 
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR 
   FIELD ANTAL LIKE EKRAPPRESULT.EANTAL                 
   FIELD BELOPP LIKE EKRAPPRESULT.EBELOPP              
   FIELD OVERTIDTILL LIKE TIDREGITAB.OVERTIDTILL
   INDEX AONR IS PRIMARY AONR DELNR ASCENDING.     
DEFINE TEMP-TABLE traktsum   
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD PRIS LIKE TIDREGITAB.PRIS     
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP 
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR 
   FIELD ANTAL LIKE EKRAPPRESULT.EANTAL                 
   FIELD BELOPP LIKE EKRAPPRESULT.EBELOPP              
   FIELD TRAKTKOD LIKE TIDREGITAB.TRAKTKOD
   INDEX AONR IS PRIMARY AONR DELNR ASCENDING.      
DEFINE TEMP-TABLE tilltab                                                  
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD  
   FIELD AONR LIKE TIDREGITAB.AONR  
   FIELD DELNR LIKE TIDREGITAB.DELNR             
   FIELD TBELOPP AS DECIMAL
   FIELD OBELOPP AS DECIMAL
   FIELD LBELOPP AS DECIMAL 
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD AONR DELNR.    
DEFINE TEMP-TABLE overtidhj 
   FIELD PERSONALKOD LIKE ekoforst.PERSONALKOD 
   FIELD PRIS LIKE TIDREGITAB.PRIS    
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP   
   FIELD AONR LIKE EKRAPPRESULT.EPROJEKT 
   FIELD DELNR LIKE TIDREGITAB.DELNR     
   FIELD BELOPP LIKE TIDREGITAB.PRIS
   FIELD OVERTIDTILL LIKE TIDREGITAB.OVERTIDTILL   
   FIELD OVERANTAL LIKE TIDREGITAB.OVERANTAL.      
/*{TIDAPPDEF.I}*/
DEFINE TEMP-TABLE flexen
   FIELD PLONTILLANTAL AS DECIMAL FORMAT "-999.99"
   FIELD PDATUM LIKE TIDREGITAB.DATUM
   FIELD PKOD LIKE PERSONALTAB.PERSONALKOD
   INDEX PKOD IS PRIMARY PKOD ASCENDING.   
DEFINE TEMP-TABLE flexsum
   FIELD PLONTILLANTAL AS DECIMAL FORMAT "-999.99"
   FIELD PLONTILLSEK AS INTEGER
   FIELD PKOD LIKE PERSONALTAB.PERSONALKOD
   INDEX PKOD IS PRIMARY PKOD ASCENDING.   

DEFINE VARIABLE tidtim AS DECIMAL NO-UNDO. 
DEFINE VARIABLE otidtim AS DECIMAL NO-UNDO.
DEFINE VARIABLE tid100 AS DECIMAL NO-UNDO.
DEFINE VARIABLE otidtim1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE otidtim2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE otidtim3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE arrhjsum AS DECIMAL NO-UNDO.    
DEFINE VARIABLE arrhjsum2 AS DECIMAL NO-UNDO. 
DEFINE VARIABLE arrhjsum3 AS DECIMAL NO-UNDO.  
DEFINE VARIABLE idag AS DATE NO-UNDO. 
DEFINE VARIABLE regdatum2 AS DATE NO-UNDO.  
DEFINE VARIABLE regdatum3 AS DATE NO-UNDO.
DEFINE VARIABLE helg AS INTEGER NO-UNDO.  
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.
DEFINE VARIABLE regdagspar AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE nattpers LIKE TIDREGITAB.PERSONALKOD NO-UNDO.    
DEFINE VARIABLE nattpris LIKE TIDREGITAB.PRIS NO-UNDO.   
DEFINE VARIABLE nattaonr LIKE AONRTAB.AONR NO-UNDO.  
DEFINE VARIABLE nattdelnr LIKE AONRTAB.DELNR NO-UNDO.  
DEFINE VARIABLE nattaoomr LIKE AONRTAB.OMRADE NO-UNDO.  
DEFINE VARIABLE ovkod LIKE TIDREGITAB.OVERTIDTILL NO-UNDO. 
DEFINE VARIABLE ovantal LIKE TIDREGITAB.OVERANTAL NO-UNDO.
DEFINE VARIABLE ovbelopp LIKE EKRAPPRESULT.EOVERBELOPP NO-UNDO. 
DEFINE VARIABLE multi AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE justtid AS DECIMAL FORMAT "99.99" NO-UNDO.     
DEFINE VARIABLE kodanst LIKE ANSTFORMTAB.KOD NO-UNDO.
DEFINE VARIABLE traav LIKE PERSONALTAB.TRAAVTAL NO-UNDO. 
DEFINE VARIABLE pkod LIKE PERSONALTAB.PERSONALKOD NO-UNDO. 
DEFINE VARIABLE reskod LIKE TIDREGITAB.LONTILLAGG NO-UNDO.  
DEFINE VARIABLE resantal AS DECIMAL NO-UNDO. 
DEFINE VARIABLE resbelopp LIKE EKRAPPRESULT.ELONBELOPP NO-UNDO. 
DEFINE VARIABLE totpristim AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE trakod LIKE TIDREGITAB.TRAKTKOD NO-UNDO. 
DEFINE VARIABLE traantal LIKE TIDREGITAB.TRAKTANTAL NO-UNDO.
DEFINE VARIABLE trabelopp LIKE EKRAPPRESULT.ETRAKTBELOPP NO-UNDO. 
DEFINE VARIABLE lonkod LIKE TIDREGITAB.LONTILLAGG NO-UNDO. 
DEFINE VARIABLE lonantal LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.
DEFINE VARIABLE lonbelopp LIKE EKRAPPRESULT.ELONBELOPP NO-UNDO. 
DEFINE VARIABLE typover LIKE EKRAPPRESULT.ERESULTENH NO-UNDO.   
DEFINE VARIABLE arrflex AS INTEGER NO-UNDO.
DEFINE VARIABLE plflex AS DECIMAL NO-UNDO.
DEFINE VARIABLE seku AS INTEGER NO-UNDO.

DEFINE BUFFER tidbuff FOR TIDREGITAB.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
/*{FORESTYR.I}*/
globanv = "AUTOMAT".   
/*IF  globforetag = "LULE" OR globforetag = "ELPA" THEN DO: 
   RUN ejkordflexlul_UI.   
END.

PROCEDURE ejkordflexlul_UI :*/
   OPEN QUERY pq FOR EACH PERSONALTAB WHERE  PERSONALTAB.PERSONALKOD = "120" AND PERSONALTAB.PERSMASK = TRUE
   USE-INDEX PERSONALKOD NO-LOCK.   
   GET FIRST pq NO-LOCK.
   DO WHILE AVAILABLE(PERSONALTAB):
      persrec = RECID(PERSONALTAB).
      FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD 
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.            
      IF AVAILABLE FLEXAVT AND FLEXAVT.FLEXTID = TRUE THEN DO TRANSACTION:
         FIND FIRST FLEXSALDO WHERE FLEXSALDO.PERSONALKOD = PERSONALTAB.PERSONALKOD 
         EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE FLEXSALDO THEN DO:
            ASSIGN 
            FLEXSALDO.EJKORDFLEX = 0.   
         END.      
         IF globforetag = "CLULE" OR  globforetag = "ELPA" THEN DO:
            EMPTY TEMP-TABLE ftemp NO-ERROR.             
            fdatum = ?.
            OPEN QUERY tidq FOR EACH TIDREGITAB WHERE
            TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
            AND TIDLOG = TRUE AND TIDREGITAB.DATUM > 10/31/2005 
            AND SUBSTRING(TIDREGITAB.VECKOKORD,10,9) = "" USE-INDEX PSTART NO-LOCK.    
            GET FIRST tidq NO-LOCK.
            DO WHILE AVAILABLE (TIDREGITAB):               
               IF TIDREGITAB.TIDLOG = FALSE THEN musz = musz.
               IF TIDREGITAB.OVERTIDUTTAG NE "F" THEN musz = musz.
               ELSE IF TIDREGITAB.DATUM NE fdatum THEN DO:                  
                  regdatum = TIDREGITAB.DATUM.
                  regvnr = TIDREGITAB.VECKONUMMER.
                  RUN SLUTARB.P.            
                  CREATE ftemp.
                  ASSIGN
                  ftemp.DAG = TIDREGITAB.DAG
                  ftemp.DATUM = TIDREGITAB.DATUM
                  ftemp.OSTART = regstart
                  ftemp.OSLUT = regslut
                  ftemp.FSTART = TIDREGITAB.START
                  ftemp.FSLUT = TIDREGITAB.SLUT
                  fdatum = TIDREGITAB.DATUM.
               END.
               ELSE DO:
                  ASSIGN
                  ftemp.FSLUT = TIDREGITAB.SLUT.
               END.
               GET NEXT tidq NO-LOCK.
            END.            
         END.


         ASSIGN
         kolldatum = ?
         lufl = 0.
         fdatum = ?.
         OPEN QUERY tq FOR EACH TIDREGITAB WHERE
         TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
         AND TIDLOG = TRUE AND TIDREGITAB.DATUM > 10/31/2005 
         AND SUBSTRING(TIDREGITAB.VECKOKORD,10,9) = "" USE-INDEX PSTART NO-LOCK.    
         GET FIRST tq NO-LOCK.
         DO WHILE AVAILABLE(TIDREGITAB):            
            regdatum = TIDREGITAB.DATUM.
            regvnr = TIDREGITAB.VECKONUMMER.
            RUN SLUTARB.P.
            musz = FALSE.
            IF TIDREGITAB.AONR = "155" THEN DO:
               nytid = TIDREGITAB.TOTALT.
               RUN TIMSEK.P.           
               CREATE flexen.
               ASSIGN flexen.PKOD = PERSONALTAB.PERSONALKOD                 
               flexen.PLONTILLANTAL = (-1) * sekunder
               flexen.PDATUM = TIDREGITAB.DATUM.             
            END.
            ELSE IF TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.LONTILLAGG = "" THEN DO:
               IF TIDREGITAB.AONR = "155" THEN musz = musz.
               ELSE IF regstart = regslut THEN musz = TRUE.
               ELSE IF TIDREGITAB.START < regstart THEN musz = TRUE.                                                                                     
               ELSE IF TIDREGITAB.SLUT > regslut THEN musz = TRUE.                                                             
            END.  
            IF musz = TRUE THEN DO:              
               musz = FALSE.
               IF TIDREGITAB.START < regstart THEN DO:
                  IF TIDREGITAB.SLUT > regstart THEN DO:                          
                     nytid = TIDREGITAB.START.
                     RUN TIMSEK.P.
                     seku = sekunder.
                     nytid = regstart.
                     RUN TIMSEK.P.
                     sekunder = sekunder - seku.
                     RUN SEKTIM.P.                   
                     plflex = nytid.
                  END.
                  ELSE DO:                     
                     plflex = TIDREGITAB.TOTALT.
                  END.
                  nytid = plflex.
                  RUN TIMSEK.P.           
                  CREATE flexen.
                  ASSIGN flexen.PKOD = PERSONALTAB.PERSONALKOD
                  flexen.PLONTILLANTAL = sekunder
                  flexen.PDATUM = TIDREGITAB.DATUM.             
               END.
               IF TIDREGITAB.SLUT > regslut THEN DO:
                  IF TIDREGITAB.START < regslut THEN DO:                  
                     nytid = TIDREGITAB.SLUT.
                     RUN TIMSEK.P.
                     seku = sekunder.
                     nytid = regslut.
                     RUN TIMSEK.P.
                     sekunder = seku - sekunder.
                     RUN SEKTIM.P.                     
                     plflex = nytid.
                  END.
                  ELSE DO:                      
                      plflex = TIDREGITAB.TOTALT.
                  END.                        
                  nytid = plflex.
                  RUN TIMSEK.P.           
                  CREATE flexen.
                  ASSIGN flexen.PKOD = PERSONALTAB.PERSONALKOD
                  flexen.PLONTILLANTAL = sekunder
                  flexen.PDATUM = TIDREGITAB.DATUM.             
               END.               
            END.               
            
            IF globforetag = "cLULE" OR globforetag = "elpa" THEN DO:                                                                     
               IF kolldatum NE TIDREGITAB.DATUM THEN DO:
                  FIND FIRST ftemp WHERE ftemp.DATUM = TIDREGITAB.DATUM NO-LOCK NO-ERROR.
                  IF AVAILABLE ftemp THEN DO:
                     /*räkna minusflex om det saknas registrering på morgonen*/
                     IF ftemp.FSTART > ftemp.OSTART THEN DO:
                        nytid = ftemp.FSTART.
                        RUN TIMSEK.P.
                        seku = sekunder.
                        nytid = ftemp.OSTART.
                        RUN TIMSEK.P.
                        sekunder = sekunder - seku.                        
                        CREATE flexen.
                        ASSIGN flexen.PKOD = PERSONALTAB.PERSONALKOD
                        flexen.PLONTILLANTAL = sekunder
                        flexen.PDATUM = TIDREGITAB.DATUM.                                     
                     END.
                  END.
               END.
            END.
            IF globforetag = "cLULE" OR globforetag = "elpa" THEN DO:                                                                     
               IF kolldatum NE TIDREGITAB.DATUM THEN DO:
                  FIND FIRST ftemp WHERE ftemp.DATUM = TIDREGITAB.DATUM NO-LOCK NO-ERROR.
                  IF AVAILABLE ftemp THEN DO:                                 
                     /*räkna minusflex om det saknas registrering på kvällen*/
                     IF ftemp.FSLUT < ftemp.OSLUT THEN DO:
                        nytid = ftemp.FSLUT.
                        RUN TIMSEK.P.
                        seku = sekunder.
                        nytid = ftemp.OSLUT.
                        RUN TIMSEK.P.
                        sekunder = seku - sekunder.
                        CREATE flexen.
                        ASSIGN flexen.PKOD = PERSONALTAB.PERSONALKOD
                        flexen.PLONTILLANTAL = sekunder
                        flexen.PDATUM = TIDREGITAB.DATUM.                                     
                     END.
                  END.
               END.
            END. 

            musz = FALSE.
            IF TIDREGITAB.TIDLOG = TRUE THEN DO:                                                                                             
               IF kolldatum NE TIDREGITAB.DATUM THEN DO:
                   ASSIGN
                   lufl = 0
                   musz = TRUE.
               END.
               ELSE IF kolldatum = TIDREGITAB.DATUM AND lufl = 0 THEN musz = TRUE.                        
               IF musz = TRUE THEN DO:
                  ASSIGN
                  musz = FALSE
                  regdatum = TIDREGITAB.DATUM
                  regvnr = TIDREGITAB.VECKONUMMER.
                  RUN SLFLARB.P.
                  IF TIDREGITAB.START < regslut AND TIDREGITAB.SLUT > regstart THEN DO:                           
                      nytid = lunchslutet.
                      RUN TIMSEK.P.
                      ASSIGN
                      seku = sekunder
                      nytid = lunchstarten.
                      RUN TIMSEK.P.                                                     
                      lunorm = (seku - sekunder) / 60.
                      IF TIDREGITAB.LAGANTAL > 0 AND TIDREGITAB.LAGANTAL < lunorm THEN DO:                              
                         ASSIGN
                         sekunder = (lunorm - TIDREGITAB.LAGANTAL) * 60
                         /*fltid = fltid + sekunder*/
                         lufl = 1.
                         CREATE flexen.
                         ASSIGN flexen.PKOD = PERSONALTAB.PERSONALKOD
                         flexen.PLONTILLANTAL = sekunder
                         flexen.PDATUM = TIDREGITAB.DATUM.             
                      END.
                      IF TIDREGITAB.LAGANTAL > lunorm THEN DO:                              
                         sekunder = (TIDREGITAB.LAGANTAL - lunorm ) * 60.                      
                         ASSIGN
                         /*fltid = fltid - sekunder*/
                         lufl = 1.
                         CREATE flexen.
                         ASSIGN flexen.PKOD = PERSONALTAB.PERSONALKOD
                         flexen.PLONTILLANTAL = (-1) * sekunder
                         flexen.PDATUM = TIDREGITAB.DATUM.             
                      END.                                           
                  END.
               END.
               kolldatum = TIDREGITAB.DATUM.
            END.
            
            
            GET NEXT tq NO-LOCK.
         END. 
      END.
      GET NEXT pq NO-LOCK.
   END.   
   
   arrflex = 0.
   FOR EACH flexen BREAK BY flexen.PKOD:    
      ACCUMULATE flexen.PLONTILLANTAL(TOTAL BY flexen.PKOD).       
      IF LAST-OF(flexen.PKOD) THEN DO:
         CREATE flexsum.                    
         ASSIGN  
         flexsum.PKOD =  flexen.PKOD
         sekunder = (ACCUM TOTAL flexen.PLONTILLANTAL) - arrflex.
         RUN FSEKTIM.P.
         ASSIGN
         flexsum.PLONTILLSEK = sekunder
         flexsum.PLONTILLANTAL = fnytid.
         arrflex = ACCUM TOTAL flexen.PLONTILLANTAL.             
      END.
   END.
   FOR EACH flexsum:
      DO TRANSACTION:
         FIND FIRST FLEXSALDO WHERE FLEXSALDO.PERSONALKOD = flexsum.PKOD 
         EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE FLEXSALDO THEN DO:
            CREATE FLEXSALDO.
            ASSIGN FLEXSALDO.PERSONALKOD = flexsum.PKOD.
         END.   
         ASSIGN 
         FLEXSALDO.EJKORDFLEX = flexsum.PLONTILLANTAL.
         MESSAGE FLEXSALDO.EJKORDFLEX VIEW-AS ALERT-BOX.
      END.   
   END.   
/*END PROCEDURE.*/
