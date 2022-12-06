 /* SUMDAG.P SUMMER TIDREGTABAR/DAG   */
DEF VAR CC AS INTEGER.
DEFINE NEW SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE printer LIKE SKRIVARDEF.UTSKRIFTSTYP NO-UNDO.
DEFINE NEW SHARED VARIABLE printer1 LIKE SKRIVARDEF.SKRIVARID NO-UNDO.
DEFINE NEW SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
/*DEFINE NEW SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/
DEFINE NEW SHARED VARIABLE globlos LIKE ANVANDARE.AV-LOSEN NO-UNDO.
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE NEW SHARED VARIABLE Guru.GlobalaVariabler:plusaonr LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE NEW SHARED VARIABLE Guru.GlobalaVariabler:plusdnr LIKE TIDREGITAB.DELNR NO-UNDO.
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
DEFINE NEW SHARED VARIABLE muszval AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE vartgamla AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE NEW SHARED VARIABLE frustarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE fruslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffestart AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffeslut AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchstarten LIKE ARBETSTIDTAB.LUNCHSTART NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchslutet LIKE ARBETSTIDTAB.LUNCHSLUT NO-UNDO.
DEFINE NEW SHARED VARIABLE regtotalt LIKE TIDREGITAB.TOTALT NO-UNDO.   
DEFINE NEW SHARED VARIABLE regstartsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE regslutsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE bustart3 LIKE TIDREGITAB.START NO-UNDO. 

DEFINE TEMP-TABLE ekoforst
   FIELD OMRADE LIKE PERSONALTAB.OMRADE 
   FIELD PERSONALKOD LIKE EKRAPPRESULT.EPERSONALKOD 
   FIELD AONR LIKE EKRAPPRESULT.EPROJEKT 
   FIELD DELNR LIKE TIDREGITAB.DELNR 
   FIELD DATUM LIKE TIDREGITAB.DATUM
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
   FIELD TRAAVTAL LIKE PERSONALTAB.TRAAVTAL      
   FIELD TBELOPP LIKE EKRAPPRESULT.EBELOPP
   INDEX PERSAO IS PRIMARY PERSONALKOD AONR DELNR ASCENDING
   INDEX ORGAONR ORG AONR DELNR ASCENDING.
DEFINE TEMP-TABLE slutsum               
   FIELD OMRADE LIKE PERSONALTAB.OMRADE 
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD PRIS LIKE TIDREGITAB.PRIS     
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP
   FIELD DATUM LIKE TIDREGITAB.DATUM 
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
   FIELD DATUM LIKE TIDREGITAB.DATUM
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
   FIELD DATUM LIKE TIDREGITAB.DATUM
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR 
   FIELD ANTAL LIKE EKRAPPRESULT.EANTAL                 
   FIELD BELOPP LIKE EKRAPPRESULT.EBELOPP              
   FIELD TRAKTKOD LIKE TIDREGITAB.TRAKTKOD
   FIELD TRAAVTAL LIKE PERSONALTAB.TRAAVTAL
   INDEX AONR IS PRIMARY AONR DELNR ASCENDING.      
DEFINE TEMP-TABLE tilltab               
   FIELD OMRADE LIKE PERSONALTAB.OMRADE                                    
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD  
   FIELD AONR LIKE TIDREGITAB.AONR  
   FIELD DELNR LIKE TIDREGITAB.DELNR       
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP   
   FIELD DATUM LIKE TIDREGITAB.DATUM            
   FIELD TBELOPP AS DECIMAL
   FIELD OBELOPP AS DECIMAL
   FIELD LBELOPP AS DECIMAL    
   FIELD IKOST LIKE SUMTID.IKOSTNAD 
   FIELD TYPKOD LIKE LONTILL.TYPKOD   
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD AONR DELNR PRISTYP.    
DEFINE TEMP-TABLE overtidhj 
   FIELD PERSONALKOD LIKE ekoforst.PERSONALKOD 
   FIELD PRIS LIKE TIDREGITAB.PRIS    
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP
   FIELD DATUM LIKE TIDREGITAB.DATUM   
   FIELD AONR LIKE EKRAPPRESULT.EPROJEKT 
   FIELD DELNR LIKE TIDREGITAB.DELNR     
   FIELD BELOPP LIKE TIDREGITAB.PRIS
   FIELD OVERTIDTILL LIKE TIDREGITAB.OVERTIDTILL   
   FIELD OVERANTAL LIKE TIDREGITAB.OVERANTAL.           
DEFINE TEMP-TABLE restid               
   FIELD OMRADE LIKE PERSONALTAB.OMRADE 
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD DATUM LIKE TIDREGITAB.DATUM         
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP 
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR 
   FIELD ANTAL AS DECIMAL LABEL "RTIMMAR"                
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD AONR DELNR ASCENDING.     
DEFINE TEMP-TABLE ejlon
   FIELD AONR LIKE SUMEJLON.AONR 
   FIELD DATUM LIKE SUMEJLON.DATUM 
   FIELD DELNR LIKE SUMEJLON.DELNR 
   FIELD LONKOST LIKE SUMEJLON.LONKOST 
   FIELD PERSONALKOD LIKE SUMEJLON.PERSONALKOD 
   FIELD PRISTYP LIKE SUMEJLON.PRISTYP 
   FIELD TYPKOD LIKE SUMEJLON.TYPKOD 
   FIELD VECKOKORD LIKE SUMEJLON.VECKOKORD
   INDEX AONR IS PRIMARY AONR DELNR DATUM PERSONALKOD PRISTYP TYPKOD.
DEFINE VARIABLE enkpris LIKE TIMKOSTNADSTAB.PRISA NO-UNDO.
DEFINE VARIABLE kvalpris LIKE TIMKOSTNADSTAB.PRISA NO-UNDO.   
DEFINE VARIABLE vartypkod LIKE LONTILL.TYPKOD NO-UNDO.         
DEFINE VARIABLE omrpers LIKE PERSONALTAB.OMRADE NO-UNDO.
DEFINE VARIABLE tidtim AS DECIMAL NO-UNDO. 
DEFINE VARIABLE otidtim AS DECIMAL NO-UNDO.
DEFINE VARIABLE tid100 AS DECIMAL NO-UNDO.
DEFINE VARIABLE otidtim1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE otidtim2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE otidtim3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE arrhjsum AS DECIMAL NO-UNDO.    
DEFINE VARIABLE arrhjsum2 AS DECIMAL NO-UNDO. 
DEFINE VARIABLE arrhjsum3 AS DECIMAL NO-UNDO.  

DEFINE VARIABLE nattpers LIKE TIDREGITAB.PERSONALKOD NO-UNDO.    
DEFINE VARIABLE nattpris LIKE TIDREGITAB.PRIS NO-UNDO.   
DEFINE VARIABLE nattaonr LIKE AONRTAB.AONR NO-UNDO.  
DEFINE VARIABLE nattdelnr LIKE AONRTAB.DELNR NO-UNDO.  
DEFINE VARIABLE nattaoomr LIKE AONRTAB.OMRADE NO-UNDO.  
DEFINE VARIABLE ovkod LIKE TIDREGITAB.OVERTIDTILL NO-UNDO. 
DEFINE VARIABLE ovantal LIKE TIDREGITAB.OVERANTAL NO-UNDO.
DEFINE VARIABLE ovbelopp LIKE EKRAPPRESULT.EOVERBELOPP NO-UNDO. 
DEFINE VARIABLE multi AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE kodanst LIKE ANSTFORMTAB.KOD NO-UNDO.
DEFINE VARIABLE traav LIKE PERSONALTAB.TRAAVTAL NO-UNDO. 
DEFINE VARIABLE pkod LIKE PERSONALTAB.PERSONALKOD NO-UNDO. 
DEFINE VARIABLE reskod LIKE TIDREGITAB.LONTILLAGG NO-UNDO.  
DEFINE VARIABLE resantal AS DECIMAL NO-UNDO. 
DEFINE VARIABLE resbelopp LIKE EKRAPPRESULT.ELONBELOPP NO-UNDO. 
DEFINE VARIABLE totpristim AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE interpristim AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE trakod LIKE TIDREGITAB.TRAKTKOD NO-UNDO. 
DEFINE VARIABLE traantal LIKE TIDREGITAB.TRAKTANTAL NO-UNDO.
DEFINE VARIABLE trabelopp LIKE EKRAPPRESULT.ETRAKTBELOPP NO-UNDO. 
DEFINE VARIABLE lonkod LIKE TIDREGITAB.LONTILLAGG NO-UNDO. 
DEFINE VARIABLE lonantal LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.
DEFINE VARIABLE lonbeloppv LIKE EKRAPPRESULT.ELONBELOPP NO-UNDO. 
DEFINE VARIABLE typover LIKE EKRAPPRESULT.ERESULTENH NO-UNDO.   
DEFINE VARIABLE kollvecka LIKE VECKONATT.VECKOKORD NO-UNDO.
DEFINE VARIABLE inder AS LOGICAL NO-UNDO.
DEFINE VARIABLE sumtidrec AS RECID NO-UNDO.
DEFINE BUFFER tidbuff FOR TIDREGITAB.
DEFINE QUERY sumq FOR SUMTIDDAG.
DEFINE QUERY tidq FOR TIDREGITAB.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG. 
IF FORETAG.PROGRAM = "PLUSD" THEN plusdval = TRUE.
			     ELSE plusdval = FALSE.
/*SUMERING/DAG*/

ASSIGN 
kollvecka = ""
pkod = " ".
/*RUN sumtid_UI.   /*TID FÖR EJ VECKOKÖRDA*/ */
ASSIGN   
kollvecka = ""
pkod = " ".
RUN sumtill_UI.  /*TILL FÖR EJ VECKOKÖRDA*/      
   
   
/* **********************  Internal Procedures  *********************** */


PROCEDURE noll_UI:
   ASSIGN    
   lonbeloppv = 0
   resbelopp = 0
   trabelopp = 0    
   ovbelopp = 0                    
   ovkod = " "         
   tidtim = 0
   otidtim = 0
   tid100 = 0
   otidtim1 = 0
   otidtim2 = 0
   otidtim3 = 0
   inder = TRUE.       
END PROCEDURE.      
PROCEDURE over_UI:
   typover = "OVE".
   multi = 0.
   FIND FIRST OVERKOD WHERE OVERKOD.KOD = kodanst AND
   OVERKOD.OVERTIDTILL = ovkod 
   USE-INDEX OVER NO-LOCK NO-ERROR.
   IF AVAILABLE OVERKOD THEN DO:
      multi = OVERKOD.MULTIP.      
   END.       
   ovbelopp = (nattpris + (nattpris * multi)) * ovantal.    
   IF AVAILABLE TIDREGITAB THEN DO:
      IF nattpris = 0 AND TIDREGITAB.TIDLOG = FALSE THEN DO:       
         ovbelopp = (totpristim + (totpristim * multi)) * ovantal.
      END.
   END.
END.
PROCEDURE res_UI:
   multi = 0.          
   FIND FIRST LONTILL WHERE LONTILL.KOD = kodanst AND
   LONTILL.LONTILLAGG = reskod
   USE-INDEX LON NO-LOCK NO-ERROR.
   IF AVAILABLE LONTILL THEN DO:
      multi = LONTILL.MULTIP.      
   END.
   ELSE RETURN.
   typover = SUBSTRING(LONTILL.TYPKOD,1,3).
   IF SUBSTRING(LONTILL.TYPKOD,1,3) = "EJE" THEN RETURN.     
   IF SUBSTRING(LONTILL.TYPKOD,1,3) = "REL" THEN DO:
      nattpris = TIDREGITAB.PRIS.
      resbelopp = (nattpris * multi) * resantal.    
      IF SUBSTRING(LONTILL.TYPKOD,9,5) = "EJIND" THEN inder = FALSE.                                 
      ELSE inder = TRUE.       
   END.
   ELSE IF SUBSTRING(LONTILL.TYPKOD,1,3) = "RES" THEN DO:      
      nattpris = TIDREGITAB.PRIS.
      ovbelopp = (nattpris + (nattpris * multi)) * resantal.
      inder = FALSE.                                            
   END.
   ELSE DO:      /*BORDE ALLDRIG INTRÄFFA*/     
      resbelopp = (nattpris + (nattpris * multi)) * resantal.       
      IF SUBSTRING(LONTILL.TYPKOD,9,5) = "EJIND" THEN inder = FALSE.                                 
      ELSE inder = TRUE. 
   END.   
   IF kollvecka NE "" THEN DO:
      IF SUBSTRING(LONTILL.TYPKOD,1,3) = "EJE" THEN musz = musz.
      ELSE DO:     
         CREATE ejlon.
         ASSIGN
         ejlon.AONR = TIDREGITAB.AONR 
         ejlon.DATUM = TIDREGITAB.DATUM 
         ejlon.DELNR = TIDREGITAB.DELNR 
         ejlon.LONKOST = resbelopp + ovbelopp
         ejlon.PERSONALKOD = TIDREGITAB.PERSONALKOD 
         ejlon.PRISTYP = TIDREGITAB.PRISTYP 
         ejlon.TYPKOD = LONTILL.TYPKOD 
         ejlon.VECKOKORD = kollvecka.
      END.
   END.                                 
END PROCEDURE.
PROCEDURE restid_UI:     
   DO TRANSACTION:
      FIND FIRST restid WHERE restid.DATUM = TIDREGITAB.DATUM AND
      restid.PERSONALKOD = TIDREGITAB.PERSONALKOD AND 
      restid.AONR = TIDREGITAB.AONR AND restid.DELNR = TIDREGITAB.DELNR 
      EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE restid THEN DO: 
         CREATE restid.
      END.
      ASSIGN           
      restid.OMRADE = PERSONALTAB.OMRADE 
      restid.PERSONALKOD = TIDREGITAB.PERSONALKOD 
      restid.DATUM = TIDREGITAB.DATUM      
      restid.PRISTYP = TIDREGITAB.PRISTYP 
      restid.AONR = TIDREGITAB.AONR
      restid.DELNR = TIDREGITAB.DELNR 
      restid.ANTAL = restid.ANTAL +
      DECIMAL(SUBSTRING(STRING(TIDREGITAB.TOTALT,"99.99"),1,2)) +
      (DECIMAL(SUBSTRING(STRING(TIDREGITAB.TOTALT,"99.99"),4,2)) / 60).                           
   END.
END PROCEDURE.  	
PROCEDURE lon_UI:
   ASSIGN
   lonbeloppv = 0
   multi = 0     
   typover = "LON".
   FIND FIRST AONRTAB WHERE AONRTAB.AONR = TIDREGITAB.AONR AND 
   AONRTAB.DELNR = TIDREGITAB.DELNR
   USE-INDEX AONR NO-LOCK NO-ERROR.        
   FIND FIRST LONTILL WHERE LONTILL.KOD = kodanst AND
   LONTILL.LONTILLAGG = TIDREGITAB.LONTILLAGG 
   USE-INDEX LON NO-LOCK NO-ERROR.
   IF AVAILABLE LONTILL THEN DO:
      ASSIGN
      multi = LONTILL.MULTIP
      typover = SUBSTRING(LONTILL.TYPKOD,1,3).
      IF SUBSTRING(LONTILL.TYPKOD,9,5) = "EJIND" THEN inder = FALSE. 
      ELSE inder = TRUE.                  
   END.                      
   ELSE RETURN.
   IF typover = "EJE" THEN RETURN.  
   IF typover = "RES" THEN DO:       
      FIND FIRST TIMKOSTNADSTAB 
      WHERE TIMKOSTNADSTAB.PERSONALKOD = pkod AND
      TIMKOSTNADSTAB.PRISTYP = 'RESTID...' USE-INDEX PRISPERS NO-LOCK NO-ERROR.
      nytid = lonantal.
      RUN TIMSEK.P.
      lonantal = (sekunder / 3600).		 
      ovbelopp = (TIMKOSTNADSTAB.PRISA + (TIMKOSTNADSTAB.PRISA * multi)) * lonantal.        
      inder = FALSE.
   END. 
   ELSE IF typover = "REL" THEN DO:      
      FIND FIRST TIMKOSTNADSTAB 
      WHERE TIMKOSTNADSTAB.PERSONALKOD = pkod AND
      TIMKOSTNADSTAB.PRISTYP = 'RESTID...' USE-INDEX PRISPERS NO-LOCK NO-ERROR.      
      nytid = lonantal.
      RUN TIMSEK.P.
      lonantal = (sekunder / 3600).		 
      lonbeloppv = (TIMKOSTNADSTAB.PRISA * multi) * lonantal.
      IF SUBSTRING(LONTILL.TYPKOD,9,5) = "EJIND" THEN inder = FALSE. 
      ELSE inder = TRUE.             
   END. 
   ELSE IF typover = "RE2" THEN DO:
      FIND FIRST TIMKOSTNADSTAB 
      WHERE TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIMKOSTNADSTAB.PRISTYP = 'TOT.PRIS.' USE-INDEX PRISPERS NO-LOCK NO-ERROR. 
      IF PERSONALTAB.OMRADE NE AONRTAB.OMRADE THEN DO:  
         IF globforetag = "NORD" THEN DO:                                      
            FIND FIRST TIMKOSTNADSTAB WHERE 
            TIMKOSTNADSTAB.PERSONALKOD = TIDREGITAB.PERSONALKOD AND 
            TIMKOSTNADSTAB.PRISTYP = "INTERNPRI" 
            USE-INDEX PRISPERS NO-LOCK NO-ERROR.                             
         END.
      END.
      ASSIGN               
      nytid = lonantal.
      RUN TIMSEK.P.
      lonantal = (sekunder / 3600).		 
      lonbeloppv = (TIMKOSTNADSTAB.PRISA + (TIMKOSTNADSTAB.PRISA * multi)) * lonantal.
      IF SUBSTRING(LONTILL.TYPKOD,9,5) = "EJIND" THEN inder = FALSE. 
      ELSE inder = TRUE.              
   END.
   ELSE IF typover =  "OVE" THEN DO:    
      FIND FIRST TIMKOSTNADSTAB 
      WHERE TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIMKOSTNADSTAB.PRISTYP = 'TOT.PRIS.' USE-INDEX PRISPERS NO-LOCK NO-ERROR. 
      IF PERSONALTAB.OMRADE NE AONRTAB.OMRADE THEN DO:  
         IF globforetag = "NORD" THEN DO:                                      
            FIND FIRST TIMKOSTNADSTAB WHERE 
            TIMKOSTNADSTAB.PERSONALKOD = TIDREGITAB.PERSONALKOD AND 
            TIMKOSTNADSTAB.PRISTYP = "INTERNPRI" 
            USE-INDEX PRISPERS NO-LOCK NO-ERROR.                             
         END.
      END.
      nytid = lonantal.
      RUN TIMSEK.P.
      lonantal = (sekunder / 3600).	                           		 
      ovbelopp = (TIMKOSTNADSTAB.PRISA + (TIMKOSTNADSTAB.PRISA * multi)) * lonantal.       
      inder = FALSE.
   END. 
   ELSE IF typover = "TRA" THEN DO:
      trabelopp = lonantal * LONTILL.ERSATTNING.
      inder = FALSE.
   END.
   ELSE DO:
      FIND FIRST LONTILL WHERE LONTILL.KOD = kodanst AND
      LONTILL.LONTILLAGG = lonkod USE-INDEX LON NO-LOCK NO-ERROR.
      IF AVAILABLE LONTILL THEN DO:
         IF LONTILL.ENHET = "KR" THEN DO:
            lonbeloppv = lonantal.
            lonantal = 0.
         END.
         ELSE IF LONTILL.ENHET = "TI" THEN DO:
            nytid = lonantal.
            RUN TIMSEK.P.
            lonantal = (sekunder / 3600). 
            lonbeloppv = lonantal * LONTILL.ERSATTNING.
         END.
         ELSE DO:
             lonbeloppv = lonantal * LONTILL.ERSATTNING.           
         END.         
         IF globforetag  = "NORD" THEN DO:
            IF lonkod = "FRUK" OR 
            lonkod = "LUNC" OR 
            lonkod = "MIDD" OR 
            lonkod = "FRLU" OR
            lonkod = "FRMI" OR 
            lonkod = "LUMI" OR 
            lonkod = "FLMI" THEN DO:
               lonkod = "850".
               lonbeloppv = lonbeloppv * -1 * LONTILL.ERSATTNING.
            END.
         END.
         IF globforetag = 'GRAN'  OR 
         globforetag = 'ROSL' OR globforetag = 'MALA'  THEN DO:                                                   
            IF lonkod = "720" OR 
            lonkod = "721" OR 
            lonkod = "722" OR 
            lonkod = "726" OR
            lonkod = "727" OR 
            lonkod = "728" OR 
            lonkod = "729" THEN DO:               
               lonbeloppv = lonbeloppv * -1.
            END.
         END.
      END.         
   END.
   IF kollvecka NE "" THEN DO:
      IF SUBSTRING(LONTILL.TYPKOD,1,3) = "EJE" THEN musz = musz.
      ELSE DO:     
         CREATE ejlon.
         ASSIGN
         ejlon.AONR = TIDREGITAB.AONR 
         ejlon.DATUM = TIDREGITAB.DATUM 
         ejlon.DELNR = TIDREGITAB.DELNR 
         ejlon.LONKOST = lonbeloppv + ovbelopp
         ejlon.PERSONALKOD = TIDREGITAB.PERSONALKOD 
         ejlon.PRISTYP = TIDREGITAB.PRISTYP 
         ejlon.TYPKOD = LONTILL.TYPKOD 
         ejlon.VECKOKORD = kollvecka.
      END.
   END.   
   resbelopp = lonbeloppv.                                   
END PROCEDURE.  
PROCEDURE tra_UI:   
   typover = "TRA".
   FIND FIRST TRAKTATAB WHERE TRAKTATAB.TRAAVTAL = traav AND
   TRAKTATAB.TRAKTKOD = trakod USE-INDEX TRAKTKOD NO-LOCK NO-ERROR. 
   IF AVAILABLE TRAKTATAB THEN DO:
      trabelopp = traantal * TRAKTATAB.ERSATTNING.         
   END.       
END PROCEDURE.	
/*SUB TILL SUMMERING/ÅR*/         
PROCEDURE skapa_UI:
   IF typover = "EJE" THEN RETURN.
   DO TRANSACTION:      
      IF typover = "RES" OR typover = "REL" THEN DO:
         FIND FIRST tilltab WHERE tilltab.DATUM = regdatum AND 
         tilltab.PERSONALKOD = TIDREGITAB.PERSONALKOD AND 
         tilltab.AONR = TIDREGITAB.AONR AND tilltab.DELNR = TIDREGITAB.DELNR AND
         tilltab.PRISTYP = "RESTID..."   
         USE-INDEX PERSONALKOD EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE tilltab THEN DO: 
            CREATE tilltab.                   
         END.
         ASSIGN                          
         tilltab.OMRADE = omrpers 
         tilltab.PERSONALKOD = TIDREGITAB.PERSONALKOD  
         tilltab.AONR = TIDREGITAB.AONR  
         tilltab.DELNR = TIDREGITAB.DELNR  
         tilltab.PRISTYP = "RESTID..."   
         tilltab.DATUM = regdatum
         tilltab.TBELOPP = tilltab.TBELOPP + trabelopp 
         tilltab.OBELOPP = tilltab.OBELOPP + ovbelopp
         tilltab.LBELOPP = tilltab.LBELOPP + resbelopp.
      END.
      ELSE DO:
         FIND FIRST tilltab WHERE tilltab.DATUM = regdatum AND 
         tilltab.PERSONALKOD = TIDREGITAB.PERSONALKOD AND 
         tilltab.AONR = TIDREGITAB.AONR AND tilltab.DELNR = TIDREGITAB.DELNR AND
         tilltab.PRISTYP = TIDREGITAB.PRISTYP   
         USE-INDEX PERSONALKOD EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE tilltab THEN DO: 
            CREATE tilltab.                   
         END.
         ASSIGN                          
         tilltab.OMRADE = omrpers 
         tilltab.PERSONALKOD = TIDREGITAB.PERSONALKOD  
         tilltab.AONR = TIDREGITAB.AONR  
         tilltab.DELNR = TIDREGITAB.DELNR  
         tilltab.PRISTYP = TIDREGITAB.PRISTYP   
         tilltab.DATUM = regdatum
         tilltab.TBELOPP = tilltab.TBELOPP + trabelopp 
         tilltab.OBELOPP = tilltab.OBELOPP + ovbelopp
         tilltab.LBELOPP = tilltab.LBELOPP + resbelopp.        
      END.
      IF inder = TRUE THEN DO:
         tilltab.IKOST = tilltab.IKOST + resbelopp.
      END.      
   END.                                     
END PROCEDURE.	   
PROCEDURE nattpris_UI:
   IF globforetag = "NORD" THEN DO:
      IF nattpers NE TIDREGITAB.PERSONALKOD THEN DO:
         ASSIGN
         nattaoomr = " "
         nattaonr = " "
         nattdelnr = 0.
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = TIDREGITAB.PERSONALKOD
         USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
         nattpers = TIDREGITAB.PERSONALKOD.
         IF nattaonr = TIDREGITAB.AONR AND nattdelnr = TIDREGITAB.DELNR THEN DO:
            nattaonr = nattaonr.
         END.
         ELSE DO:
            FIND FIRST AONRTAB WHERE 
            AONRTAB.AONR = TIDREGITAB.AONR AND AONRTAB.DELNR = TIDREGITAB.DELNR 
            USE-INDEX AONR NO-LOCK NO-ERROR.
            ASSIGN 
            nattaoomr = AONRTAB.OMRADE
            nattaonr = TIDREGITAB.AONR 
            nattdelnr = TIDREGITAB.DELNR.
         END.    
         IF nattaoomr NE " " THEN DO:                        
            IF PERSONALTAB.OMRADE NE AONRTAB.OMRADE THEN DO:                                
               FIND FIRST TIMKOSTNADSTAB WHERE 
               TIMKOSTNADSTAB.PERSONALKOD = TIDREGITAB.PERSONALKOD AND 
               TIMKOSTNADSTAB.PRISTYP = "INTERNPRI" 
               USE-INDEX PRISPERS NO-LOCK NO-ERROR.
               nattpris = TIMKOSTNADSTAB.PRISA.                  
            END.
            ELSE DO:
               nattpris = TIDREGITAB.PRIS.
            END.     
         END. 
         ELSE DO:
            nattpris = TIDREGITAB.PRIS.
         END.           
      END.
      ELSE DO:  
         IF nattaonr = TIDREGITAB.AONR AND nattdelnr = TIDREGITAB.DELNR THEN DO:
            nattaonr = nattaonr.
         END.                   
         ELSE DO:
            FIND FIRST AONRTAB WHERE 
            AONRTAB.AONR = TIDREGITAB.AONR AND AONRTAB.DELNR = TIDREGITAB.DELNR 
            USE-INDEX AONR NO-LOCK NO-ERROR.
            ASSIGN   
            nattaoomr = AONRTAB.OMRADE
            nattaonr = TIDREGITAB.AONR 
            nattdelnr = TIDREGITAB.DELNR.
         END.    
         IF nattaoomr NE " " THEN DO:                        
            IF PERSONALTAB.OMRADE NE AONRTAB.OMRADE THEN DO:                                    
               FIND FIRST TIMKOSTNADSTAB WHERE 
               TIMKOSTNADSTAB.PERSONALKOD = TIDREGITAB.PERSONALKOD AND 
               TIMKOSTNADSTAB.PRISTYP = "INTERNPRI" 
               USE-INDEX PRISPERS NO-LOCK NO-ERROR.
               nattpris = TIMKOSTNADSTAB.PRISA.                  
            END.
            ELSE DO:
               nattpris = TIDREGITAB.PRIS.
            END.     
         END. 
         ELSE DO:
            nattpris = TIDREGITAB.PRIS.
         END.   
      END.    
   END.
   ELSE DO: 
      nattpris = TIDREGITAB.PRIS.
   END.
END PROCEDURE.
PROCEDURE nattpris2_UI.
   IF globforetag = "NORD" THEN DO:
      FIND FIRST AONRTAB WHERE 
      AONRTAB.AONR = TIDREGITAB.AONR AND AONRTAB.DELNR = TIDREGITAB.DELNR 
      USE-INDEX AONR NO-LOCK NO-ERROR.
      IF omrpers = AONRTAB.OMRADE THEN DO:
         nattpris = totpristim.
      END.    
      ELSE DO:                                 
         FIND FIRST TIMKOSTNADSTAB WHERE 
         TIMKOSTNADSTAB.PERSONALKOD = TIDREGITAB.PERSONALKOD AND 
         TIMKOSTNADSTAB.PRISTYP = "INTERNPRI" 
         USE-INDEX PRISPERS NO-LOCK NO-ERROR.
         nattpris = TIMKOSTNADSTAB.PRISA.                  
      END.
   END.         
   ELSE DO: 
      nattpris = totpristim.
   END.
END PROCEDURE.	   

PROCEDURE pers_UI:
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod 
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.  
   IF AVAILABLE PERSONALTAB THEN DO:
      FIND FIRST TIMKOSTNADSTAB 
      WHERE TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIMKOSTNADSTAB.PRISTYP = 'TOT.PRIS.' USE-INDEX PRISPERS NO-LOCK NO-ERROR.                  
      ASSIGN
      kodanst = ANSTFORMTAB.KOD                                                
      totpristim = TIMKOSTNADSTAB.PRISA 
      omrpers = PERSONALTAB.OMRADE      
      traav = PERSONALTAB.TRAAVTAL.      
   END.   
END PROCEDURE.
PROCEDURE sumtid_UI:   
   OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.VECKOKORD NE "" AND 
   TIDREGITAB.TIDLOG = TRUE AND MONTH(TIDREGITAB.DATUM) = 5 AND 
   TIDREGITAB.AONR = "210179" NO-LOCK.   
   GET FIRST tidq NO-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB):
      regdatum = TIDREGITAB.DATUM.
      IF pkod NE TIDREGITAB.PERSONALKOD THEN DO:
         pkod = TIDREGITAB.PERSONALKOD.
         RUN pers_UI.                                   
      END.                                     
      RUN nattpris_UI.
      IF globforetag = "SOLE" THEN RUN prissatt_UI.
      IF TIDREGITAB.PRISTYP = 'RESTID...' AND TIDREGITAB.LONTILLAGG NE " " 
      THEN DO TRANSACTION: 
         RUN noll_UI.	                            
         ASSIGN  
         reskod = TIDREGITAB.LONTILLAGG
         nytid = TIDREGITAB.LONTILLANTAL.
         /*nattpris = TIDREGITAB.PRIS. SÄTTS I NATTPRIS_UI*/ 
         RUN TIMSEK.P.
         resantal = (sekunder / 3600).
         RUN res_UI.		 
         RUN skapa_UI.   
         RUN noll_UI.
         IF TIDREGITAB.TRAKTKOD NE " " THEN DO:
            ASSIGN
            trakod = TIDREGITAB.TRAKTKOD
            traantal = TIDREGITAB.TRAKTANTAL.           
            RUN tra_UI.	    
            RUN skapa_UI.   	    
         END. 
         RUN restid_UI.         	            	       
      END.	
      ELSE DO:    
         /*
         IF TIDREGITAB.LAGBAS = TRUE THEN DO TRANSACTION: 
            RUN noll_UI.	                            
            ASSIGN        
            nytid = TIDREGITAB.LONTILLANTAL.      
            RUN TIMSEK.P.
            FIND FIRST LONTILL WHERE LONTILL.KOD = kodanst AND
            LONTILL.LONTILLAGG = TIDREGITAB.LONTILLAGG USE-INDEX LON NO-LOCK NO-ERROR.
            IF AVAILABLE LONTILL THEN DO:         
               lonantal = (sekunder / 3600).
               resbelopp = lonantal * LONTILL.ERSATTNING. 
               RUN skapa_UI.      
            END.       	          	      	       
         END.	 
            */     	
         RUN noll_UI.         	                                    
         IF TIDREGITAB.OKOD1 = " " AND TIDREGITAB.OKOD2 = " " AND TIDREGITAB.OKOD3 = " " THEN DO:   
            tidtim = DECIMAL(SUBSTRING(STRING(TIDREGITAB.TOTALT,"99.99"),1,2)).
            tid100 = DECIMAL(SUBSTRING(STRING(TIDREGITAB.TOTALT,"99.99"),4,2)) / 60.
            tidtim = tidtim + tid100.     
         END.
         ELSE DO:
            otidtim = DECIMAL(SUBSTRING(STRING(TIDREGITAB.TOTALT,"99.99"),1,2)).
            tid100 = DECIMAL(SUBSTRING(STRING(TIDREGITAB.TOTALT,"99.99"),4,2)) / 60.
            otidtim = otidtim + tid100.     
         END.   
         otidtim1 = DECIMAL(SUBSTRING(STRING(TIDREGITAB.OANT1,"99.99"),1,2)).
         tid100 = DECIMAL(SUBSTRING(STRING(TIDREGITAB.OANT1,"99.99"),4,2)) / 60.
         otidtim1 = otidtim1 + tid100.      
         otidtim2 = DECIMAL(SUBSTRING(STRING(TIDREGITAB.OANT2,"99.99"),1,2)).
         tid100 = DECIMAL(SUBSTRING(STRING(TIDREGITAB.OANT2,"99.99"),4,2)) / 60.
         otidtim2 = otidtim2 + tid100.      
         otidtim3 = DECIMAL(SUBSTRING(STRING(TIDREGITAB.OANT3,"99.99"),1,2)).
         tid100 = DECIMAL(SUBSTRING(STRING(TIDREGITAB.OANT3,"99.99"),4,2)) / 60.
         otidtim3 = otidtim3 + tid100.      
         CREATE ekoforst.
         ASSIGN
         ekoforst.OMRADE = omrpers
         ekoforst.PERSONALKOD = TIDREGITAB.PERSONALKOD 
         ekoforst.AONR = TIDREGITAB.AONR
         ekoforst.DELNR = TIDREGITAB.DELNR                  
         ekoforst.PRIS = TIDREGITAB.PRIS 
         ekoforst.DATUM = TIDREGITAB.DATUM          
         ekoforst.PRISTYP = TIDREGITAB.PRISTYP  
         ekoforst.BELOPP = nattpris
         ekoforst.OBELOPP = nattpris 
         ekoforst.ANTAL = tidtim  
         ekoforst.OTIMMAR = otidtim      
         ekoforst.OANT1 = otidtim1    
         ekoforst.OANT2 = otidtim2    
         ekoforst.OANT3 = otidtim3    
         ekoforst.OKOD1 = TIDREGITAB.OKOD1 
         ekoforst.OKOD2 = TIDREGITAB.OKOD2 
         ekoforst.OKOD3 = TIDREGITAB.OKOD3                                
         ekoforst.TRAKTKOD = TIDREGITAB.TRAKTKOD 
         ekoforst.TRAKTANTAL = TIDREGITAB.TRAKTANTAL
         ekoforst.TRAAVTAL = traav.              
         /*ekoforst.LONTILLAGG = TIDREGITAB.LONTILLAGG 
         ekoforst.LONTILLANTAL = TIDREGITAB.LONTILLANTAL         */
         IF globforetag = "NORD" THEN ekoforst.OTIMMAR = otidtim1 + otidtim2 + otidtim3.
         IF TIDREGITAB.OKOD1 NE " " THEN ASSIGN ekoforst.BELOPP = 0.
         IF TIDREGITAB.OKOD2 NE " " THEN ASSIGN ekoforst.BELOPP = 0.
         IF TIDREGITAB.OKOD3 NE " " THEN ASSIGN ekoforst.BELOPP = 0.
         ASSIGN ekoforst.BELOPP = ekoforst.ANTAL * ekoforst.BELOPP. 
         IF ekoforst.PRISTYP = "RESTID..." THEN DO:
            ASSIGN 
            /*ekoforst.PRISTYP = "TOT.PRIS." */
            ekoforst.OTIMMAR = 0.         
            RUN restid_UI.
         END.                                
      END. /* EJ RESTID MED LÖNT*/
      GET NEXT tidq NO-LOCK.
   END.       
   CLOSE QUERY tidq.           
   ASSIGN   
   pkod = " "  
   arrhjsum = 0
   arrhjsum2 = 0
   arrhjsum3 = 0.    
   FOR EACH ekoforst BREAK BY ekoforst.DATUM BY ekoforst.PERSONALKOD BY ekoforst.AONR BY 
   ekoforst.DELNR BY ekoforst.PRISTYP:
      IF pkod NE ekoforst.PERSONALKOD THEN DO:
         pkod = ekoforst.PERSONALKOD. 
         RUN pers_UI.
      END.
      IF ekoforst.OANT1 > 0  THEN DO:      
         CREATE overtidhj.
         ASSIGN                          
         overtidhj.PERSONALKOD = ekoforst.PERSONALKOD 
         overtidhj.PRIS = ekoforst.PRIS     
         overtidhj.PRISTYP = ekoforst.PRISTYP
         overtidhj.DATUM = ekoforst.DATUM 
         overtidhj.AONR = ekoforst.AONR 
         overtidhj.DELNR = ekoforst.DELNR 
         overtidhj.BELOPP = ekoforst.OBELOPP * ekoforst.OANT1 
         overtidhj.OVERANTAL = ekoforst.OANT1 
         overtidhj.OVERTIDTILL = ekoforst.OKOD1.                
         IF globforetag = "NORD" THEN DO:
            ASSIGN
            ovkod = ekoforst.OKOD1
            ovantal = ekoforst.OANT1
            nattpris = ekoforst.OBELOPP.        
            RUN over_UI.  
            overtidhj.BELOPP = ovbelopp.                          
         END.
         IF globforetag = "SOLE" THEN DO:
            ASSIGN
            ovkod = ekoforst.OKOD1
            ovantal = ekoforst.OANT1
            nattpris = ekoforst.OBELOPP.        
            RUN oversole_UI.  
            overtidhj.BELOPP = ovbelopp.                          
         END.  
      END. 
      IF ekoforst.OANT2 > 0  THEN DO:      
         CREATE overtidhj.      
         ASSIGN
         overtidhj.PERSONALKOD = ekoforst.PERSONALKOD 
         overtidhj.PRIS = ekoforst.PRIS     
         overtidhj.PRISTYP = ekoforst.PRISTYP 
         overtidhj.DATUM = ekoforst.DATUM
         overtidhj.AONR = ekoforst.AONR 
         overtidhj.DELNR = ekoforst.DELNR 
         overtidhj.BELOPP = ekoforst.OBELOPP * ekoforst.OANT2   
         overtidhj.OVERANTAL = ekoforst.OANT2
         overtidhj.OVERTIDTILL = ekoforst.OKOD2.
         IF globforetag = "NORD" THEN DO:
            ASSIGN
            ovkod = ekoforst.OKOD2
            ovantal = ekoforst.OANT2
            nattpris = ekoforst.OBELOPP.            
            RUN over_UI.  
            overtidhj.BELOPP = ovbelopp.                          
         END.
         IF globforetag = "SOLE" THEN DO:
            ASSIGN
            ovkod = ekoforst.OKOD2
            ovantal = ekoforst.OANT2
            nattpris = ekoforst.OBELOPP.        
            RUN overbelopp_UI.  
            overtidhj.BELOPP = ovbelopp.                          
         END.                      
      END.                    
      IF ekoforst.OANT3 > 0  THEN DO:      
         CREATE overtidhj.           
         ASSIGN               
         overtidhj.PERSONALKOD = ekoforst.PERSONALKOD 
         overtidhj.PRIS = ekoforst.PRIS     
         overtidhj.PRISTYP = ekoforst.PRISTYP
         overtidhj.DATUM = ekoforst.DATUM 
         overtidhj.AONR = ekoforst.AONR 
         overtidhj.DELNR = ekoforst.DELNR 
         overtidhj.BELOPP = ekoforst.OBELOPP * ekoforst.OANT3
         overtidhj.OVERANTAL = ekoforst.OANT3
         overtidhj.OVERTIDTILL = ekoforst.OKOD3. 
         IF globforetag = "NORD" THEN DO:
            ASSIGN
            ovkod = ekoforst.OKOD3
            ovantal = ekoforst.OANT3
            nattpris = ekoforst.OBELOPP.              
            RUN over_UI.  
            overtidhj.BELOPP = ovbelopp.                          
         END.
         IF globforetag = "SOLE" THEN DO:
            ASSIGN
            ovkod = ekoforst.OKOD3
            ovantal = ekoforst.OANT3
            nattpris = ekoforst.OBELOPP.        
            RUN overbelopp_UI.  
            overtidhj.BELOPP = ovbelopp.                          
         END.                          
      END.                                                             
      ACCUMULATE ekoforst.BELOPP 
      (TOTAL BY ekoforst.DATUM BY ekoforst.PERSONALKOD BY ekoforst.AONR BY 
      ekoforst.DELNR BY ekoforst.PRISTYP). 
      ACCUMULATE ekoforst.ANTAL 
      (TOTAL BY ekoforst.DATUM BY ekoforst.PERSONALKOD BY ekoforst.AONR BY 
      ekoforst.DELNR BY ekoforst.PRISTYP).  
      ACCUMULATE ekoforst.OTIMMAR 
      (TOTAL BY ekoforst.DATUM BY ekoforst.PERSONALKOD BY ekoforst.AONR BY 
      ekoforst.DELNR BY ekoforst.PRISTYP).     
      IF LAST-OF(ekoforst.PRISTYP) THEN DO:
         CREATE slutsum.
         ASSIGN      
         slutsum.OMRADE = ekoforst.OMRADE 
         slutsum.PERSONALKOD = ekoforst.PERSONALKOD 
         slutsum.PRIS = ekoforst.PRIS     
         slutsum.PRISTYP = ekoforst.PRISTYP 
         slutsum.DATUM = ekoforst.DATUM
         slutsum.AONR = ekoforst.AONR
         slutsum.DELNR = ekoforst.DELNR         
         slutsum.BELOPP = (ACCUM TOTAL ekoforst.BELOPP) - arrhjsum                       
         slutsum.ANTAL = (ACCUM TOTAL ekoforst.ANTAL) - arrhjsum2  
         slutsum.OTIMMAR = (ACCUM TOTAL ekoforst.OTIMMAR) - arrhjsum3.  
         arrhjsum = ACCUM TOTAL ekoforst.BELOPP.  
         arrhjsum2 = ACCUM TOTAL ekoforst.ANTAL.  
         arrhjsum3 = ACCUM TOTAL ekoforst.OTIMMAR.        
      END. 
   END.                        
   ASSIGN
   arrhjsum = 0
   arrhjsum2 = 0 
   arrhjsum3 = 0.   
   FOR EACH overtidhj BREAK BY overtidhj.DATUM BY overtidhj.PERSONALKOD BY
   overtidhj.AONR BY overtidhj.DELNR BY overtidhj.OVERTIDTILL BY overtidhj.PRISTYP:
      ACCUMULATE overtidhj.BELOPP 
      (TOTAL BY overtidhj.DATUM BY overtidhj.PERSONALKOD BY overtidhj.AONR BY 
      overtidhj.DELNR BY overtidhj.OVERTIDTILL BY overtidhj.PRISTYP). 
      ACCUMULATE overtidhj.OVERANTAL 
      (TOTAL BY overtidhj.DATUM BY overtidhj.PERSONALKOD BY overtidhj.AONR BY
      overtidhj.DELNR BY overtidhj.OVERTIDTILL BY overtidhj.PRISTYP).
      IF LAST-OF(overtidhj.PRISTYP) THEN DO:
         CREATE oversum.
         ASSIGN         
         oversum.PERSONALKOD = overtidhj.PERSONALKOD 
         oversum.PRIS = overtidhj.PRIS     
         oversum.PRISTYP = overtidhj.PRISTYP
         oversum.DATUM = overtidhj.DATUM 
         oversum.AONR = overtidhj.AONR
         oversum.DELNR = overtidhj.DELNR 
         oversum.OVERTIDTILL = overtidhj.OVERTIDTILL
         oversum.BELOPP = (ACCUM TOTAL overtidhj.BELOPP) - arrhjsum 
         oversum.ANTAL = (ACCUM TOTAL overtidhj.OVERANTAL) - arrhjsum2.                          
         arrhjsum = ACCUM TOTAL overtidhj.BELOPP.
         arrhjsum2 = ACCUM TOTAL overtidhj.OVERANTAL.                                          
      END.   
   END.         
   ASSIGN        
   arrhjsum = 0 
   arrhjsum2 = 0.    
   FOR EACH oversum BREAK BY oversum.DATUM BY oversum.PERSONALKOD BY oversum.AONR BY  
   oversum.DELNR BY oversum.PRISTYP:           
      ACCUMULATE oversum.BELOPP (TOTAL BY oversum.DATUM BY oversum.PERSONALKOD BY
      oversum.AONR BY oversum.DELNR BY oversum.PRISTYP). 
      ACCUMULATE oversum.ANTAL (TOTAL BY oversum.DATUM BY oversum.PERSONALKOD BY
      oversum.AONR BY oversum.DELNR BY oversum.PRISTYP).
      IF LAST-OF(oversum.PRISTYP) THEN DO:
         FIND FIRST slutsum WHERE slutsum.DATUM = oversum.DATUM AND slutsum.PERSONALKOD = oversum.PERSONALKOD 
         AND slutsum.AONR = oversum.AONR AND slutsum.DELNR = oversum.DELNR AND
         slutsum.PRISTYP = oversum.PRISTYP
         USE-INDEX AONR NO-ERROR.
         IF AVAILABLE slutsum THEN DO:
            ASSIGN slutsum.OBELOPP = (ACCUM TOTAL oversum.BELOPP) - arrhjsum
            slutsum.OANTAL = (ACCUM TOTAL oversum.ANTAL) - arrhjsum2.     
            arrhjsum = ACCUM TOTAL oversum.BELOPP.    
            arrhjsum2 = ACCUM TOTAL oversum.ANTAL.                                
          END.
       END.  
    END.        
    ASSIGN         
    arrhjsum = 0
    arrhjsum2 = 0.    
    FOR EACH ekoforst BREAK BY 
    ekoforst.DATUM BY ekoforst.PERSONALKOD BY ekoforst.AONR BY ekoforst.DELNR BY 
    ekoforst.TRAKTKOD:
       ACCUMULATE ekoforst.TRAKTANTAL 
      (TOTAL BY ekoforst.DATUM BY ekoforst.PERSONALKOD BY ekoforst.AONR BY 
      ekoforst.DELNR BY ekoforst.TRAKTKOD).
      IF LAST-OF(ekoforst.TRAKTKOD) THEN DO:
         CREATE traktsum.                    
         ASSIGN 
         traktsum.PERSONALKOD = ekoforst.PERSONALKOD 
         traktsum.PRIS = ekoforst.PRIS     
         traktsum.PRISTYP = ekoforst.PRISTYP 
         traktsum.DATUM = ekoforst.DATUM
         traktsum.AONR = ekoforst.AONR
         traktsum.DELNR = ekoforst.DELNR 
         traktsum.TRAKTKOD = ekoforst.TRAKTKOD
         traktsum.TRAAVTAL = ekoforst.TRAAVTAL
         traktsum.ANTAL = (ACCUM TOTAL ekoforst.TRAKTANTAL) - arrhjsum.      
         arrhjsum = ACCUM TOTAL ekoforst.TRAKTANTAL.                                                             
      END.   
   END.        
   ASSIGN         
   arrhjsum = 0
   arrhjsum2 = 0. 
   FOR EACH traktsum BREAK BY traktsum.DATUM BY traktsum.PERSONALKOD BY traktsum.AONR BY traktsum.DELNR:
      FIND FIRST TRAKTATAB WHERE TRAKTATAB.TRAKTKOD = traktsum.TRAKTKOD AND
      TRAKTATAB.TRAAVTAL = traktsum.TRAAVTAL USE-INDEX TRAKT NO-LOCK NO-ERROR.
      IF AVAILABLE TRAKTATAB THEN DO:
         ASSIGN traktsum.BELOPP = TRAKTATAB.ERSATTNING * traktsum.ANTAL.
      END.
      ACCUMULATE traktsum.BELOPP (TOTAL BY traktsum.DATUM BY traktsum.PERSONALKOD BY 
      traktsum.AONR BY traktsum.DELNR).
      ACCUMULATE traktsum.ANTAL (TOTAL BY traktsum.DATUM BY traktsum.PERSONALKOD BY 
      traktsum.AONR BY traktsum.DELNR).
      IF LAST-OF(traktsum.DELNR) THEN DO:
         FIND FIRST slutsum WHERE slutsum.DATUM = traktsum.DATUM AND 
         slutsum.PERSONALKOD = traktsum.PERSONALKOD 
         AND slutsum.AONR = traktsum.AONR AND slutsum.DELNR = traktsum.DELNR 
         USE-INDEX AONR NO-ERROR.
         IF AVAILABLE slutsum THEN DO:
            ASSIGN slutsum.TBELOPP = (ACCUM TOTAL traktsum.BELOPP) - arrhjsum
            slutsum.TANTAL = (ACCUM TOTAL traktsum.ANTAL) - arrhjsum2.      
            arrhjsum = ACCUM TOTAL traktsum.BELOPP.       
            arrhjsum2 = ACCUM TOTAL traktsum.ANTAL.                                 
         END.
      END.   
   END.           
   FOR EACH slutsum:
      IF SLUTSUM.PERSONALKOD NE "806" THEN DO:     
         CC = CC + slutsum.BELOPP.
         DISPLAY "QQ" SLUTSUM.PERSONALKOD  slutsum.BELOPP CC WITH FRAME DD DOWN.             
      END.
   END.                                      
END PROCEDURE.
PROCEDURE sumtill_UI: 
   OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.VECKOKORD NE "" AND MONTH(TIDREGITAB.DATUM) = 5 AND 
   TIDREGITAB.TIDLOG = FALSE AND TIDREGITAB.AONR = "210179" NO-LOCK.   
   GET FIRST tidq NO-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB):
      /*ÖVERTIDTILLÄGG*/
      /*ÖVERTIDS KOD FÅR BELOPPET ENKELÖTID OCH KVALÖTID * ANTAL*/      
      IF TIDREGITAB.AONR = "" THEN regdatum = regdatum.
      ELSE DO:         
         regdatum = TIDREGITAB.DATUM.	       
         IF pkod NE TIDREGITAB.PERSONALKOD THEN DO:
            pkod = TIDREGITAB.PERSONALKOD.
            RUN pers_UI.
         END.
         RUN noll_UI.
         RUN nattpris2_UI.
         
         IF TIDREGITAB.OKOD1 NE " " THEN DO: 
            ASSIGN            	      
            ovkod = TIDREGITAB.OKOD1
            nytid = TIDREGITAB.OANT1.
            RUN TIMSEK.P.
            ovantal = (sekunder / 3600).
            RUN over_UI.		      
            RUN skapa_UI.   
         END.	  
         RUN noll_UI.
         IF TIDREGITAB.OKOD2 NE " " THEN DO: 
            ASSIGN            	
            ovkod = TIDREGITAB.OKOD2
            nytid = TIDREGITAB.OANT2.
            RUN TIMSEK.P.
            ovantal = (sekunder / 3600).	      
            RUN over_UI.      
            RUN skapa_UI.     		  
         END.                 
         RUN noll_UI.
         IF TIDREGITAB.OKOD3 NE " " THEN DO: 
            ASSIGN            	      
            ovkod = TIDREGITAB.OKOD3
            nytid = TIDREGITAB.OANT3.
            RUN TIMSEK.P.
            ovantal = (sekunder / 3600).
            RUN over_UI.      
            RUN skapa_UI.  	    		  
         END.        
         RUN noll_UI.
         IF TIDREGITAB.LONTILLAGG NE " " THEN DO:
            ASSIGN	       
            lonkod = TIDREGITAB.LONTILLAGG
            lonantal = TIDREGITAB.LONTILLANTAL.
            RUN lon_UI.
            RUN skapa_UI.   		    
         END.
         RUN noll_UI.
         IF TIDREGITAB.TRAKTKOD NE " " THEN DO:
            ASSIGN
            trakod = TIDREGITAB.TRAKTKOD
            traantal = TIDREGITAB.TRAKTANTAL.
            RUN tra_UI.
            RUN skapa_UI.   	    
         END.    
      END.
      GET NEXT tidq NO-LOCK.		 
   END.
   CLOSE QUERY tidq.    
   FOR EACH tilltab USE-INDEX PERSONALKOD NO-LOCK:
      IF tilltab.PERSONALKOD NE "806" THEN DO:
      CC = (CC  + tilltab.LBELOPP) - tilltab.IKOST.
      DISPLAY "WW" TILLTAB.PERSONALKOD tilltab.LBELOPP tilltab.IKOST  CC WITH FRAME CC DOWN.                  
      END.
   END.             
END PROCEDURE.	
PROCEDURE prissatt_UI.
   IF TIDREGITAB.PRISTYP = "TOT.PRIS." THEN DO:
      ASSIGN
      kvalpris = TIDREGITAB.PRIS + 172 * 1 
      enkpris = TIDREGITAB.PRIS + 172 * 0.5.
   END.
   IF TIDREGITAB.PRISTYP = "DEB.KOM.." THEN DO:
      ASSIGN
      kvalpris = TIDREGITAB.PRIS + 172 * 1 
      enkpris = TIDREGITAB.PRIS + 172 * 0.5.
   END.
   /*LASTBIL*/
   IF TIDREGITAB.PRISTYP = "LASTBIL.." THEN DO:
      ASSIGN
      kvalpris = TIDREGITAB.PRIS + 172 * 1 
      enkpris = TIDREGITAB.PRIS + 172 * 0.5.
   END.
   /*SKYLIFT*/
   IF TIDREGITAB.PRISTYP = "SKYLIFT.." THEN DO:
      ASSIGN
      kvalpris = TIDREGITAB.PRIS + 172 * 1 
      enkpris = TIDREGITAB.PRIS + 172 * 0.5.
   END.                        
   IF TIDREGITAB.PRISTYP = "GRÄVARE.." THEN DO:
      ASSIGN
      kvalpris = TIDREGITAB.PRIS + 40 
      enkpris = TIDREGITAB.PRIS + 40.
   END.
   IF TIDREGITAB.PRISTYP = "GIRAFF..." THEN DO:
      ASSIGN
      kvalpris = TIDREGITAB.PRIS + 40
      enkpris = TIDREGITAB.PRIS + 40.
   END.
END PROCEDURE.
PROCEDURE oversole_UI:
   ASSIGN
   ovbelopp = 0
   multi = 0.   
   FIND FIRST OTID WHERE OTID.DATUM = TIDREGITAB.DATUM AND
   OTID.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
   OTID.AONR = TIDREGITAB.AONR AND OTID.DELNR = TIDREGITAB.DELNR AND
   OTID.START = TIDREGITAB.START AND OTID.SLUT = TIDREGITAB.SLUT
   USE-INDEX OTID NO-LOCK NO-ERROR.
   IF NOT AVAILABLE OTID THEN DO:
      RUN overbelopp_UI.      
   END.       
   ELSE DO:
      IF TIDREGITAB.OKOD1 = OTID.KODF AND TIDREGITAB.OANT1 = OTID.ANT1 THEN DO:
         FIND FIRST OVERKOD WHERE
         OVERKOD.OVERTIDTILL = OTID.KODE2 AND
         OVERKOD.KOD = ANSTFORMTAB.KOD
         USE-INDEX OVER NO-LOCK NO-ERROR.
         IF AVAILABLE OVERKOD THEN DO:
            nytid = OTID.ANT2.
            RUN TIMSEK.P.
            ovantal = (sekunder / 3600).
            ovkod = OTID.KODE2.
            RUN overbelopp_UI.
         END.
         IF OTID.KODE3 NE "" THEN DO:
            FIND FIRST OVERKOD WHERE
            OVERKOD.OVERTIDTILL = OTID.KODE3 AND
            OVERKOD.KOD = ANSTFORMTAB.KOD
            USE-INDEX OVER NO-LOCK NO-ERROR.
            IF NOT AVAILABLE OVERKOD THEN RETURN.
            nytid = OTID.ANT3.
            RUN TIMSEK.P.
            ovantal = (sekunder / 3600).
            ovkod = OTID.KODE3.
            RUN overbelopp_UI.         
         END.
      END.
      ELSE DO:
         RUN overbelopp_UI.
      END.                      
   END.                                   
END PROCEDURE.
PROCEDURE overbelopp_UI:
   FIND FIRST OVERKOD WHERE OVERKOD.KOD = kodanst AND
   OVERKOD.OVERTIDTILL = ovkod 
   USE-INDEX OVER NO-LOCK NO-ERROR.
   IF NOT AVAILABLE OVERKOD THEN RETURN.  
   IF OVERKOD.ENKEL = 'KVAL' THEN ASSIGN ovbelopp = (ovantal * kvalpris).
                             ELSE ASSIGN ovbelopp = (ovantal * enkpris).   
END PROCEDURE.



