 /* AUREG.P AUTOMATREGISTRERING   */
/* OM startaprog = FALSE DÅ KÖRS AUREG.P PERSISTENT SE EJAUREG*/
DEFINE INPUT PARAMETER startaprog AS LOGICAL NO-UNDO. 

&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}

{SOKDEF.I}
DEFINE VARIABLE orgglobanv AS CHARACTER NO-UNDO.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
   Guru.Konstanter:globforetag = FORETAG.FORETAG.
DEFINE NEW SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
/*
DEFINE NEW SHARED VARIABLE printer LIKE SKRIVARDEF.UTSKRIFTSTYP NO-UNDO.
DEFINE NEW SHARED VARIABLE printer1 LIKE SKRIVARDEF.SKRIVARID NO-UNDO.
*/
DEFINE NEW SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
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
{TIDAPPDEF.I}
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
IF (Guru.Konstanter:globforetag = "GRAN"   OR Guru.Konstanter:globforetag = 'GSOL'   ) THEN DO:        
   utvar = "\\GRANGURU\guru_ser\server\PRO9S\".
END.
IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
   utvar = "d:\DELAD\server\PRO9S\".
END.
IF Guru.Konstanter:globforetag = "LULE" THEN DO:        
   utvar = "D:\elpool\delad\PRO9S\".
END.
IF Guru.Konstanter:globforetag = "SUND" THEN DO:
   utvar = "D:\DELAD\server\PRO10S\".
END.
IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
   utvar = "D:\DELAD\server\PRO10S\".
   /*SNATBERGET*/
   utvar = REPLACE(utvar,"D:\DELAD\SERVER\PRO10S\","D:\DELAD\PRO10S\").
      
END.  
IF Guru.Konstanter:globforetag = "MISV" THEN DO:
   utvar = "C:\elpool\delad\pro10s\".   
    utvar = "D:\elpool\delad\pro10s\".
END.
utvar = utvar + "autotid.txt".
RUN textut_UI ("AUREG START").

IF startaprog = TRUE  THEN DO:     
   RUN textut_UI ("mmkoll start").
   /*KOLL AV BERDARE MM*/
   RUN mmkoll_UI.
   RUN textut_UI ("mmkoll slut").
   RUN medbort_UI.
   RUN textut_UI ("medbort").
   RUN tidrens_UI.
   RUN textut_UI ("tidrens").
   RUN startkor_UI.
   Guru.Konstanter:globanv = orgglobanv.
END.
/* **********************  Internal Procedures  *********************** */
PROCEDURE startkor_UI:
   
   {FORESTYR.I}
   orgglobanv = Guru.Konstanter:globanv.
   Guru.Konstanter:globanv = "AUTOMAT".
   RUN textut_UI ("startakor start").
   RUN autoreg_UI (INPUT TODAY).
   RUN textut_UI ("startakor slut").
   DO TRANSACTION:
      FIND FIRST FAKTBOKP EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE FAKTBOKP THEN DO:
         CREATE FAKTBOKP.
         ASSIGN
         FAKTBOKP.FDATUM = TODAY
         FAKTBOKP.TDATUM = TODAY.
      END.
      IF YEAR(FAKTBOKP.FDATUM) < YEAR(TODAY) THEN DO:
         IF TODAY > DATE(01,01,YEAR(TODAY)) + 10 THEN FAKTBOKP.FDATUM = DATE(01,01,YEAR(TODAY)).            
      END.
   END.   
   /*FLEXKOLL*/
   
   IF  Guru.Konstanter:globforetag = "GKAL" THEN RUN ejkordflexkal_UI.
   IF  Guru.Konstanter:globforetag = "LULE" THEN RUN ejkordflexlul_UI.      
   /*UPPFÖLJNING*/
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:        
      RUN textut_UI ("TIDER KLARA AUREG").
      FIND FIRST VKORN NO-LOCK NO-ERROR.
      IF NOT AVAILABLE VKORN THEN DO:
         RUN textut_UI ("NU ÄR DET FEL AUREG").
         RETURN.
      END.
      IF VKORN.VECKOK = FALSE THEN DO:         
         RUN SUMDAG.P.
         RUN textut_UI ("DAG UPP KLARA AUREG").
      END.  
   END.
   IF Guru.Konstanter:globforetag = "LULE" THEN DO:        
      RUN textut_UI ("TIDER KLARA AUREG").
      FIND FIRST VKORN NO-LOCK NO-ERROR.
      IF NOT AVAILABLE VKORN THEN DO:
         RUN textut_UI ("NU ÄR DET FEL AUREG").
         RETURN.
      END.
      IF VKORN.VECKOK = FALSE THEN DO:         
         RUN SUMDAG.P.
         RUN textut_UI ("DAG UPP KLARA AUREG").
      END.  
   END.
   IF Guru.Konstanter:globforetag = "SUND" THEN DO:
      RUN textut_UI ("TIDER KLARA AUREG").
      FIND FIRST VKORN NO-LOCK NO-ERROR.
      IF NOT AVAILABLE VKORN THEN DO:
         RETURN.
      END.
      IF VKORN.VECKOK = FALSE THEN DO:   
         RUN SUMDAG.P.  
      END.
      RUN textut_UI ("DAG UPP KLARA AUREG").
      RUN AUTOAOSU.P.
      /*RUN SJINTYG.P.*/
   END.
   IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
      RUN textut_UI ("TIDER KLARA AUREG").
      FIND FIRST VKORN NO-LOCK NO-ERROR.
      IF NOT AVAILABLE VKORN THEN DO:
         RETURN.
      END.
      IF VKORN.VECKOK = FALSE THEN DO:   
         RUN SUMDAG.P.  
      END.
      RUN textut_UI ("DAG UPP KLARA AUREG").
      RUN AUTOAOSU.P.
      RUN SUNDIN.P.      
   END.
   IF Guru.Konstanter:globforetag = "MISV" THEN DO:
      RUN textut_UI ("TIDER KLARA AUREG").
      FIND FIRST VKORN NO-LOCK NO-ERROR.
      IF NOT AVAILABLE VKORN THEN DO:
         RETURN.
      END.
      IF VKORN.VECKOK = FALSE THEN DO:   
         RUN SUMDAG.P.  
      END.
      RUN textut_UI ("DAG UPP KLARA AUREG").           
   END.
   
END PROCEDURE.

PROCEDURE autoreg_UI:
   DEFINE INPUT PARAMETER kordatum AS DATE NO-UNDO. 
   FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
   Guru.Konstanter:globforetag = FORETAG.FORETAG.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "MISV" THEN RETURN.
   {FORESTYR.I}
   
   ASSIGN
   idag = kordatum
   regdatum = kordatum.
   RUN REGDAG.P.
   helg = 0.
   IF WEEKDAY(regdatum) = 2 THEN helg = 2.
   START:
   FOR EACH PERSONALTAB WHERE PERSONALTAB.AKTIV = TRUE
   USE-INDEX  PERSONALKOD NO-LOCK:
      persrec = RECID(PERSONALTAB).
      FIND FIRST ANSTFORMTAB WHERE ANSTFORM.ANSTALLNING = PERSONALTAB.ANSTALLNING
      USE-INDEX ANSTF NO-LOCK NO-ERROR.
      /* KOLLEKTIV SKA EJ HA AUTOMATREGISTRERINGAR  000229 */
      
      FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdatum AND
      OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
      IF NOT AVAILABLE OVERAVTAB THEN DO:
         persrec = persrec.
      END.
      ELSE DO:
         IF OVERAVTAB.EQDAG = 7 OR OVERAVTAB.EQDAG = 1 THEN NEXT START.
         IF OVERAVTAB.DAGEQ = "KLA" THEN NEXT START.
      END.  
      RUN REGVEC.P.
      RUN SLUTARB.P.
      IF regstart = regslut THEN NEXT START.
      regdatum3 = regdatum - helg.
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE AND
      TIDREGITAB.START GE regstart AND TIDREGITAB.SLUT LE regslut
      USE-INDEX PSTART NO-LOCK NO-ERROR.
      IF NOT AVAILABLE TIDREGITAB THEN DO:
         FIND LAST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM < regdatum3 AND TIDREGITAB.TIDLOG = TRUE AND
         TIDREGITAB.OKOD1 = "" AND TIDREGITAB.PRISTYP NE "RESTID..."      
         USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF NOT AVAILABLE TIDREGITAB THEN DO:
            persrec = persrec.
         END.
         ELSE DO:           
            LOOP:
            REPEAT:
               FIND FIRST AONRTAB WHERE AONRTAB.AONR = TIDREGITAB.AONR AND 
               AONRTAB.DELNR = TIDREGITAB.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
               IF NOT AVAILABLE AONRTAB THEN DO:
                  RUN nastatid_UI.
                  IF AVAILABLE TIDREGITAB THEN NEXT LOOP.
                  ELSE LEAVE LOOP.         
               END.
               ELSE DO:                                   
                  IF AONRTAB.AONRAVDATUM = 01/01/1991 OR
                  AONRTAB.AONRAVDATUM >= regdatum THEN regdatum = regdatum. 
                  ELSE DO:                                                                        
                     RUN nastatid_UI.
                     IF AVAILABLE TIDREGITAB THEN NEXT LOOP.
                     ELSE LEAVE LOOP.  
                  END.
               END.
	            regdatum2 = TIDREGITAB.DATUM.
	            FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdatum2 AND
	            OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
	            IF NOT AVAILABLE OVERAVTAB THEN DO:
	               LEAVE LOOP.
	            END.
	            ELSE DO:
	               IF OVERAVTAB.EQDAG = 7 OR OVERAVTAB.EQDAG = 1 OR 
	               OVERAVTAB.DAGEQ = "KLA" THEN DO:
	                  RUN nastatid_UI.
                     IF AVAILABLE TIDREGITAB THEN NEXT LOOP.
                     ELSE LEAVE LOOP.  
	               END.   
	               ELSE LEAVE LOOP.
	            END.
	         END. 
	         IF NOT AVAILABLE TIDREGITAB THEN NEXT START.
	         IF TIDREGITAB.DATUM < TODAY - 30 THEN NEXT START.
	         DO TRANSACTION:
	            RUN godkoll_UI (INPUT TIDREGITAB.PERSONALKOD, INPUT TODAY, OUTPUT musz).
	            IF musz = TRUE THEN musz = FALSE.
   	         ELSE DO:
                  {SOKSTART.I}
                  ASSIGN
                  soktemp.SOKVAL = 1
                  soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4] 
                  soktemp.SOKCHAR[2] = TIDREGITAB.PERSONALKOD
                  soktemp.SOKCHAR[3] = TIDREGITAB.PRISTYP
                  soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
                  soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
                  {SOKANROP.I}
                  regdatum = idag. /*ONÖDIGT*/
   	            CREATE tidbuff.
   	            ASSIGN 
   	            tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD 
   	            tidbuff.DATUM = regdatum
   	            tidbuff.PROGRAM = 'AUREG' + STRING(TODAY) + Guru.Konstanter:globanv
   	            tidbuff.OVERTIDTILL = TIDREGITAB.OVERTIDTILL
   	            tidbuff.VECKONUMMER = regvnr 
   	            tidbuff.DAG = regdagnamn
   	            tidbuff.START = regstart 
   	            tidbuff.SLUT = regslut
   	            tidbuff.AONR = TIDREGITAB.AONR  
   	            tidbuff.DELNR = TIDREGITAB.DELNR 
   	            tidbuff.TRAKTAMENTE = TIDREGITAB.TRAKTAMENTE 
   	            tidbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG 
   	            tidbuff.UTRYCKNING = TIDREGITAB.UTRYCKNING
   	            tidbuff.PRISTYP = TIDREGITAB.PRISTYP 
   	            tidbuff.PRIS = TIDREGITAB.PRIS 
   	            tidbuff.TIDLOG = TRUE. 
                  IF TIDREGITAB.OVERTIDTILL = "" THEN DO:
                     ASSIGN tidbuff.OVERTIDTILL = PERSONALTAB.BEFATTNING.
                  END.
                  /*PRISFOR*/                 
                  IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:         
   	               tidbuff.PRIS = soktemp.SOKDECI[1].
                     IF AVAILABLE AONRTAB THEN tidbuff.PRISTYP = AONRTAB.PRISTYP.
                     ELSE DO:
                        FIND FIRST AONRTAB WHERE AONRTAB.AONR = tidbuff.AONR AND 
                        AONRTAB.DELNR = tidbuff.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
                        IF AVAILABLE AONRTAB THEN tidbuff.PRISTYP = AONRTAB.PRISTYP.
                     END.
                  END.
                  
                  ASSIGN
   	            tidtabrec = RECID(tidbuff)
                  nytid = regstart.
   	            RUN TIMSEK.P.          
   	            ASSIGN
   	            regstartsek = sekunder
   	            nytid = regslut.
   	            RUN TIMSEK.P.         
   	            ASSIGN
   	            regslutsek = sekunder.   	            
                  RUN TOTTID.P.
   	            ASSIGN tidbuff.TOTALT = nytid.	   
   	            RELEASE TIDREGITAB NO-ERROR.
   	            RELEASE tidbuff NO-ERROR.
                  EMPTY TEMP-TABLE tidapptemp NO-ERROR.    	            
                  CREATE tidapptemp.
                  ASSIGN
                  tidapptemp.FORETAG = Guru.Konstanter:globforetag
                  tidapptemp.ANVANDARE = Guru.Konstanter:globanv
                  tidapptemp.RECPERS = persrec
                  tidapptemp.RECTID = tidtabrec
                  tidapptemp.DATUM = regdatum.      
                  RUN tidupp_UI. 
               END.
            END. 
         END.
      END. 
      
   END.           
   FOR EACH TIDREGITAB WHERE TIDREGITAB.TOTALT = ?:
      IF TIDREGITAB.GODKAND = "" THEN DELETE TIDREGITAB.
      ELSE DO:
         OUTPUT TO autofel APPEND.
         PUT UNFORMATTED TODAY
         TIDREGITAB.PERSONALKOD TIDREGITAB.DATUM TIDREGITAB.VECKONUMMER 
         TIDREGITAB.START TIDREGITAB.SLUT TIDREGITAB.TOTALT 
         TIDREGITAB.AONR TIDREGITAB.DELNR TIDREGITAB.PROGRAM.
      END.  
      OUTPUT CLOSE.
   END.   
END PROCEDURE.

PROCEDURE ejkordflex_UI :
   OPEN QUERY pq FOR EACH PERSONALTAB WHERE PERSONALTAB.PERSMASK = TRUE
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
      END.   
      OPEN QUERY tq FOR EACH TIDREGITAB WHERE
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
      AND TIDLOG = TRUE
      AND TIDREGITAB.VECKOKORD = "" USE-INDEX PSTART NO-LOCK.    
      GET FIRST tq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):
         IF AVAILABLE FLEXAVT AND FLEXAVT.FLEXTID = TRUE THEN DO:
	     regdatum = TIDREGITAB.DATUM.
	     regvnr = TIDREGITAB.VECKONUMMER.
	     RUN SLUTARB.P.
	     musz = FALSE.
	     IF TIDREGITAB.AONR = "910927" OR TIDREGITAB.AONR = "27" 
            OR TIDREGITAB.AONR BEGINS "1097" THEN DO:
               nytid = TIDREGITAB.TOTALT.
               RUN TIMSEK.P.           
               CREATE flexen.
               ASSIGN flexen.PKOD = PERSONALTAB.PERSONALKOD                 
               flexen.PLONTILLANTAL = (-1) * sekunder
               flexen.PDATUM = TIDREGITAB.DATUM.             
            END.
            ELSE IF TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.LONTILLAGG = "" THEN DO:          
               IF TIDREGITAB.AONR = "910927" OR TIDREGITAB.AONR = "27" 
               OR TIDREGITAB.AONR BEGINS "1097" THEN musz = musz.
               ELSE IF regstart = regslut THEN musz = TRUE.
               ELSE IF TIDREGITAB.START < regstart THEN musz = TRUE.                                                                                     
               ELSE IF TIDREGITAB.START GE regslut THEN musz = TRUE.                                                             
            END.  
            IF musz = TRUE THEN DO:              
               musz = FALSE.
               nytid = TIDREGITAB.TOTALT.
               RUN TIMSEK.P.           
               CREATE flexen.
               ASSIGN flexen.PKOD = PERSONALTAB.PERSONALKOD
               flexen.PLONTILLANTAL = sekunder
               flexen.PDATUM = TIDREGITAB.DATUM.             
            END.               
         END.   	           
         GET NEXT tq NO-LOCK.
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
      END.   
   END.   
END PROCEDURE.

PROCEDURE ejkordflexkal_UI :
   OPEN QUERY pq FOR EACH PERSONALTAB WHERE PERSONALTAB.PERSMASK = TRUE
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
      END.   
      OPEN QUERY tq FOR EACH TIDREGITAB WHERE
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
      AND TIDLOG = TRUE
      AND TIDREGITAB.VECKOKORD = "" USE-INDEX PSTART NO-LOCK.    
      GET FIRST tq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):
         IF AVAILABLE FLEXAVT AND FLEXAVT.FLEXTID = TRUE THEN DO:
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
            
         END.   	           
         GET NEXT tq NO-LOCK.
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
      END.   
   END.   
END PROCEDURE.

PROCEDURE ejkordflexlul_UI :
   OPEN QUERY pq FOR EACH PERSONALTAB WHERE PERSONALTAB.PERSMASK = TRUE
   USE-INDEX PERSONALKOD NO-LOCK.   
   GET FIRST pq NO-LOCK.
   DO WHILE AVAILABLE(PERSONALTAB):
      persrec = RECID(PERSONALTAB).
      EMPTY TEMP-TABLE ftemp NO-ERROR.             
      FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD 
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.            
      IF AVAILABLE FLEXAVT AND FLEXAVT.FLEXTID = TRUE THEN DO TRANSACTION:
         FIND FIRST FLEXSALDO WHERE FLEXSALDO.PERSONALKOD = PERSONALTAB.PERSONALKOD 
         EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE FLEXSALDO THEN DO:
            ASSIGN 
            FLEXSALDO.EJKORDFLEX = 0.   
         END.      
         IF Guru.Konstanter:globforetag = "LULE" OR  Guru.Konstanter:globforetag = "ELPA" THEN DO:
            
            fdatum = ?.
            OPEN QUERY tidq FOR EACH TIDREGITAB WHERE
            TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
            AND TIDLOG = TRUE AND TIDREGITAB.DATUM > 03/31/2005 
            AND SUBSTRING(TIDREGITAB.VECKOKORD,10,9) = "" USE-INDEX PSTART NO-LOCK.    
            GET FIRST tidq NO-LOCK.
            DO WHILE AVAILABLE (TIDREGITAB):               
               IF TIDREGITAB.TIDLOG = FALSE THEN musz = musz.
               ELSE IF TIDREGITAB.OVERTIDUTTAG NE "F" THEN musz = musz.
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
                  FIND FIRST ftemp WHERE FTEMP.DATUM = TIDREGITAB.DATUM NO-ERROR.                  
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
         AND TIDLOG = TRUE AND TIDREGITAB.DATUM > 03/31/2005 
         AND SUBSTRING(TIDREGITAB.VECKOKORD,10,9) = "" USE-INDEX PSTART NO-LOCK.    
         GET FIRST tq NO-LOCK.
         DO WHILE AVAILABLE(TIDREGITAB):            
            regdatum = TIDREGITAB.DATUM.
            regvnr = TIDREGITAB.VECKONUMMER.
            RUN SLUTARB.P.
            musz = FALSE.
            IF TIDREGITAB.AONR = "155" THEN DO:
               IF TIDREGITAB.START GE regslut THEN musz = musz.
               ELSE IF TIDREGITAB.SLUT LE regstart THEN musz = musz.
               ELSE DO:               
                  nytid = TIDREGITAB.TOTALT.
                  RUN TIMSEK.P.           
                  CREATE flexen.
                  ASSIGN flexen.PKOD = PERSONALTAB.PERSONALKOD                 
                  flexen.PLONTILLANTAL = (-1) * sekunder
                  flexen.PDATUM = TIDREGITAB.DATUM.           
               END.
            END.
            ELSE IF TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.LONTILLAGG = "" THEN DO:            
               regdatum = TIDREGITAB.DATUM.
               regvnr = TIDREGITAB.VECKONUMMER.
               RUN SLUTARB.P.                                       
               FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = TIDREGITAB.DATUM AND 
               OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM  NO-LOCK NO-ERROR.                  
               IF WEEKDAY(TIDREGITAB.DATUM) = 1 OR WEEKDAY(TIDREGITAB.DATUM) = 7
               THEN DO:                         
                  plflex = TIDREGITAB.TOTALT.
                  nytid = plflex.
                  RUN TIMSEK.P.           
                  CREATE flexen.
                  ASSIGN flexen.PKOD = PERSONALTAB.PERSONALKOD
                  flexen.PLONTILLANTAL = sekunder
                  flexen.PDATUM = TIDREGITAB.DATUM.             
                  
               END.      
               ELSE IF AVAILABLE OVERAVTAB AND ( OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7) THEN DO:                           
                  plflex = TIDREGITAB.TOTALT.
                  nytid = plflex.
                  RUN TIMSEK.P.           
                  CREATE flexen.
                  ASSIGN flexen.PKOD = PERSONALTAB.PERSONALKOD
                  flexen.PLONTILLANTAL = sekunder
                  flexen.PDATUM = TIDREGITAB.DATUM.             
                  
               END.     
               ELSE DO: 
                  IF regstart = regslut THEN DO:
                     plflex = TIDREGITAB.TOTALT.
                     nytid = plflex.
                     RUN TIMSEK.P.           
                     CREATE flexen.
                     ASSIGN flexen.PKOD = PERSONALTAB.PERSONALKOD
                     flexen.PLONTILLANTAL = sekunder
                     flexen.PDATUM = TIDREGITAB.DATUM.                                  
                  END.                     
                  ELSE DO:        
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
               END.
            END.
            IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "elpa" THEN DO:                                                                     
               IF regstart = regslut THEN.
               ELSE DO:               
                  IF kolldatum NE TIDREGITAB.DATUM THEN DO:
                     FIND FIRST ftemp WHERE ftemp.DATUM = TIDREGITAB.DATUM NO-LOCK NO-ERROR.
                     IF AVAILABLE ftemp THEN DO:
                        /*räkna minusflex om det saknas registrering på morgonen*/
                        IF ftemp.OSTART = ftemp.OSLUT THEN.
                        ELSE DO:
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
               END.
            END.
            IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "elpa" THEN DO:                                                                     
               IF regstart = regslut THEN.
               ELSE DO:               
                  IF kolldatum NE TIDREGITAB.DATUM THEN DO:
                     FIND FIRST ftemp WHERE ftemp.DATUM = TIDREGITAB.DATUM NO-LOCK NO-ERROR.
                     IF AVAILABLE ftemp THEN DO:                                 
                        /*räkna minusflex om det saknas registrering på kvällen*/
                        IF ftemp.OSTART = ftemp.OSLUT THEN.
                        ELSE DO:
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
                         lufl = 1.
                         CREATE flexen.
                         ASSIGN flexen.PKOD = PERSONALTAB.PERSONALKOD
                         flexen.PLONTILLANTAL = sekunder
                         flexen.PDATUM = TIDREGITAB.DATUM.             
                      END.
                      IF TIDREGITAB.LAGANTAL > lunorm THEN DO:                              
                         sekunder = (TIDREGITAB.LAGANTAL - lunorm ) * 60.                      
                         ASSIGN                
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
      END.   
   END.   
END PROCEDURE.

PROCEDURE godkoll_UI:
   DEFINE INPUT PARAMETER gpkod LIKE PERSONALTAB.PERSONALKOD NO-UNDO.
   DEFINE INPUT PARAMETER gdatum AS DATE NO-UNDO.
   DEFINE OUTPUT PARAMETER gfel AS LOGICAL NO-UNDO.
   gfel = FALSE.
   FIND FIRST GODKOLL WHERE GODKOLL.PERSONALKOD = gpkod AND
   GODKOLL.DATAR = YEAR(gdatum) AND 
   GODKOLL.DATMAN = MONTH(gdatum) 
   USE-INDEX PKODAR NO-LOCK NO-ERROR.
   IF AVAILABLE GODKOLL THEN DO:
      IF GODKOLL.DATUM >= gdatum THEN DO:
         gfel = TRUE. 
      END.            
   END.
END PROCEDURE.

PROCEDURE nastatid_UI :
   FIND PREV TIDREGITAB WHERE
   TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.DATUM < regdatum3 AND TIDREGITAB.TIDLOG = TRUE AND
   TIDREGITAB.OKOD1 = "" AND TIDREGITAB.PRISTYP NE "RESTID..." 
   USE-INDEX PSTART NO-LOCK NO-ERROR.
END PROCEDURE.     
PROCEDURE utfyllback_UI:
   FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.
   ASSIGN TIDREGITAB.START = regstart
   TIDREGITAB.PROGRAM = "BACK" + STRING(TODAY) + Guru.Konstanter:globanv.
   nytid = TIDREGITAB.TOTALT.
   RUN TIMSEK.P.
   ASSIGN
   nytid = TIDREGITAB.START.
   RUN TIMSEK.P.
   regstartsek = sekunder.
   nytid = TIDREGITAB.SLUT.
   RUN TIMSEK.P.
   regslutsek = sekunder.
   regdatum = TIDREGITAB.DATUM.
   RUN TOTTID.P.
   ASSIGN TIDREGITAB.TOTALT = nytid.                          
   IF (Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "cELPA") THEN DO:     
      IF PERSONALTAB.LAGBAS = TRUE THEN RUN LAGBAS.P.                                 
   END. 
END PROCEDURE.
PROCEDURE tidupp_UI:
   {TIDUPPIN.I} 
END PROCEDURE.
PROCEDURE mmkoll_UI:
   OPEN QUERY bag FOR EACH BEREDAONR NO-LOCK.
   GET FIRST bag NO-LOCK.
   DO WHILE AVAILABLE(BEREDAONR):
      FIND FIRST AONRTAB WHERE AONRTAB.BEREDARE = BEREDAONR.PERSONALKOD NO-LOCK NO-ERROR.    
      IF NOT AVAILABLE AONRTAB THEN DO:        
         FIND FIRST PLANNRTAB WHERE PLANNRTAB.BEREDARE = BEREDAONR.PERSONALKOD NO-LOCK NO-ERROR.        
         IF NOT AVAILABLE PLANNRTAB THEN DO TRANSACTION:
            GET CURRENT bag EXCLUSIVE-LOCK.
            DELETE BEREDAONR.
         END.
      END.
      GET NEXT bag NO-LOCK.
   END.                                      
   OPEN QUERY ag FOR EACH ANSVAONR NO-LOCK.
   GET FIRST ag NO-LOCK.
   DO WHILE AVAILABLE(ANSVAONR):
      FIND FIRST AONRTAB WHERE AONRTAB.ARBANSVARIG = ANSVAONR.PERSONALKOD NO-LOCK NO-ERROR.    
      IF NOT AVAILABLE AONRTAB THEN DO:        
         FIND FIRST PLANNRTAB WHERE PLANNRTAB.ARBANSVARIG = ANSVAONR.PERSONALKOD NO-LOCK NO-ERROR.        
         IF NOT AVAILABLE PLANNRTAB THEN DO TRANSACTION:
            GET CURRENT ag EXCLUSIVE-LOCK.
            DELETE ANSVAONR.
         END.
      END.
      GET NEXT ag NO-LOCK.
   END.
END PROCEDURE.
/*TA BORTGAMLA MEDDELANDE*/
PROCEDURE medbort_UI:
   OPEN QUERY meddq FOR EACH MEDDELANDE WHERE MEDDELANDE.EMOTAGET = FALSE NO-LOCK.      
   DO TRANSACTION:
      GET FIRST meddq EXCLUSIVE-LOCK.
      IF AVAILABLE MEDDELANDE THEN DO:
         IF MEDDELANDE.SANDARE = "FAKT.ADM." THEN musz = musz.
         ELSE IF MEDDELANDE.SDATUM <= TODAY - 30 THEN DELETE MEDDELANDE.
      END.      
   END.   
   REPEAT:
      DO TRANSACTION:
         GET NEXT meddq EXCLUSIVE-LOCK.
         IF AVAILABLE MEDDELANDE THEN DO:
            IF MEDDELANDE.SANDARE = "FAKT.ADM." THEN musz = musz.
            ELSE IF MEDDELANDE.SDATUM <= TODAY - 30 THEN DELETE MEDDELANDE.
         END.      
         ELSE LEAVE.
      END.
   END.    
END PROCEDURE.
PROCEDURE tidrens_UI:
   /*AUTOMATKÖRNING*/
   OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM = ? NO-LOCK.
   GET FIRST tq NO-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB):
      DO TRANSACTION:
         GET CURRENT tq EXCLUSIVE-LOCK.
         DELETE TIDREGITAB.
      END.
      GET NEXT tq NO-LOCK.
   END.
END PROCEDURE.
PROCEDURE textut_UI:
   DEFINE INPUT PARAMETER meddvar AS CHARACTER NO-UNDO.
   OUTPUT TO VALUE(utvar) APPEND.
   PUT UNFORMATTED meddvar " " Guru.Konstanter:globforetag  " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE.
