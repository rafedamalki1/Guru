 /* XSUMDAGSPEC.P SUMMER TIDREGTABAR/DAG   */
 /*ALTERNATIVA KÖRNINGAR SÖK PÅ nollkor*/
/* RESTID 
RESTID KAN KOMMA MED LÖNETILLÄGG ELLER MED ÖVERTIDSTILLÄGG
MED LÖNETILLÄGG:
res_UI BERÄKNAR KOSTNADEN.
skapa_UI SKAPAR EN tilltab MED PRISTYP 'RESTID...' OCH FÅR LÖNEKOSTNADEN FRÅN res_UI.
restid_UI SKAPAR EN restid MED PRISTYP 'RESTID...' OCH FÅR TIMMAR = RESTIDSTIMMAR.
MED ÖVERTID:
ekforst SKAPAS.
ANTAL = 0 
OTIMMAR = 0
OANT1 BERÄKNAS   
OANT2 BERÄKNAS
OANT3 BERÄKNAS
restid_UI SKAPAR EN restid MED PRISTYP 'RESTID...' OCH FÅR TIMMAR = RESTIDSTIMMAR.
over_UI BERÄKNAR KOSTNADEN.
DAGS FÖR SUMMERING:        
ekforst OCH ÖVRIGA TABELER SUMMERAS TILL  slutsum PER PERSON AONR DATUM OCH PRISTYP.
VARJE slutsum SKAPAR EN NY SUMTIDDAG.
VARJE restid  SKAPAR EN NY SUMTIDDAG.
ALLTSÅ HAMNAR TIMMAR RESTID OCH ÖVERTIDSKOSTNAD EJ PÅ SAMMA REGISTRERING.
RESTIDSTIMMAR MED LÖNETILLÄGG OCH MED ÖVERTID FÅR ALLTSÅ VAR SIN REGISTRERING OM MAN 
HAR RESTID MED LÖNETILLÄGG OCH MED ÖVERTID SAMMA DAG.
ALLTSÅ KAN DET FINNAS TRE SUMTIDDAG 
EN MED TIMMAR FÖR LÖNETILLÄGSRESTID
EN MED TIMMAR FÖR ÖVERTIDSTILLÄGG
EN MED KOSTNADER FÖR ÖVERTIDSTILLÄGG
sumtill_UI SUMMERAR LÖNETILLÄGGSKOSTNADERNA.         
VARJE tilltab SÖKER UPP EN SUMTIDDAG MED SAMMA PERSON AONR DATUM OCH PRISTYP
OCH SUMMERAR IN KOSTNADERNA.
ALLTSÅ KAN LÖNETILÄGGSKOSTNDER FÖR RESTID HAMNA PÅ NÅGON AV DE OVAN NÄMDA SUMTIDDAG 
DÄRFÖR SÖKES DEN SUMTID SOM HAR ÖVERTIDKOSTNAD MAN FÅR DÅ:
EN MED TIMMAR FÖR LÖNETILLÄGSRESTID
EN MED TIMMAR FÖR ÖVERTIDSTILLÄGG
EN MED KOSTNADER FÖR ÖVERTIDSTILLÄGG OCH KOSTNADER FÖR LÖNETILLÄGG
FINS DET BARA ÖVERTID FÅS TVÅ POSTER:                             
EN MED TIMMAR FÖR ÖVERTIDSTILLÄGG
EN MED KOSTNADER FÖR ÖVERTIDSTILLÄGG.
FINS DET BARA LÖNETILLÄGG FÅS EN POST:                             
EN MED TIMMAR FÖR LÖNETILLÄGSRESTID OCH MED KOSTNADER FÖR LÖNETILLÄGSRESTID.

*/
&Scoped-define NEW NEW
DEFINE {&NEW} SHARED VARIABLE globanv AS CHARACTER NO-UNDO. 
DEFINE {&NEW} SHARED VARIABLE globanvavdnr AS INTEGER NO-UNDO. 
DEFINE {&NEW} SHARED VARIABLE globanvpkod AS CHARACTER NO-UNDO. 
DEFINE {&NEW} SHARED VARIABLE globallm AS LOGICAL NO-UNDO. 
DEFINE {&NEW} SHARED VARIABLE globniv AS INTEGER NO-UNDO. 
DEFINE {&NEW} SHARED VARIABLE globallpers AS LOGICAL NO-UNDO. 
DEFINE {&NEW} SHARED VARIABLE globallao AS LOGICAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE globavd AS INTEGER NO-UNDO. 
DEFINE {&NEW} SHARED VARIABLE globomr AS CHARACTER NO-UNDO. 
DEFINE {&NEW} SHARED VARIABLE globlos AS CHARACTER NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE globpersnamn AS CHARACTER NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE globforetag AS CHARACTER NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE globsidl AS INTEGER NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE globsids AS INTEGER NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE plusaonr AS CHARACTER NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE plusdnr AS INTEGER NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE plusrec AS RECID  NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE plustidrec AS RECID  NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE plustid AS DECIMAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE plusdval AS LOGICAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE succelval AS LOGICAL NO-UNDO.     
DEFINE NEW SHARED VARIABLE appcon AS LOGICAL NO-UNDO.


DEFINE NEW SHARED VARIABLE varforetypval AS INTEGER EXTENT 100 NO-UNDO.     
DEFINE NEW SHARED VARIABLE varforetypchar AS CHARACTER EXTENT 100 NO-UNDO.     

{REGVAR.I}
DEFINE NEW SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE printer LIKE SKRIVARDEF.UTSKRIFTSTYP NO-UNDO.
DEFINE NEW SHARED VARIABLE printer1 LIKE SKRIVARDEF.SKRIVARID NO-UNDO.
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
DEFINE VARIABLE ejtid AS CHARACTER NO-UNDO.
DEFINE VARIABLE ejmedvar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE bustart3 LIKE TIDREGITAB.START NO-UNDO. 


DEFINE TEMP-TABLE ekoforst
   FIELD BEFATTNING AS CHARACTER 
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
   FIELD BEFATTNING AS CHARACTER
   FIELD OMRADE LIKE PERSONALTAB.OMRADE 
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD PRIS LIKE TIDREGITAB.PRIS     
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP
   FIELD DATUM LIKE TIDREGITAB.DATUM 
   FIELD ARTAL AS INTEGER 
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
   FIELD BEFATTNING AS CHARACTER
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
   FIELD BEFATTNING AS CHARACTER
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
   FIELD BEFATTNING AS CHARACTER
   FIELD OMRADE LIKE PERSONALTAB.OMRADE                                    
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD  
   FIELD AONR LIKE TIDREGITAB.AONR  
   FIELD DELNR LIKE TIDREGITAB.DELNR       
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP   
   FIELD DATUM LIKE TIDREGITAB.DATUM            
   FIELD ARTAL AS INTEGER
   FIELD TBELOPP AS DECIMAL
   FIELD OBELOPP AS DECIMAL
   FIELD LBELOPP AS DECIMAL    
   FIELD IKOST LIKE SUMTID.IKOSTNAD 
   FIELD TYPKOD LIKE LONTILL.TYPKOD   
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD AONR DELNR PRISTYP.    
DEFINE TEMP-TABLE overtidhj 
   FIELD BEFATTNING AS CHARACTER
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
   FIELD BEFATTNING AS CHARACTER
   FIELD OMRADE LIKE PERSONALTAB.OMRADE 
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD DATUM LIKE TIDREGITAB.DATUM         
   FIELD ARTAL AS INTEGER
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP 
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR 
   FIELD ANTAL AS DECIMAL LABEL "RTIMMAR"                
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD AONR DELNR ASCENDING.     
DEFINE TEMP-TABLE ejlon
   FIELD BEFATTNING AS CHARACTER
   FIELD AONR LIKE SUMEJLON.AONR 
   FIELD DATUM LIKE SUMEJLON.DATUM 
   FIELD DELNR LIKE SUMEJLON.DELNR 
   FIELD LONKOST LIKE SUMEJLON.LONKOST 
   FIELD PERSONALKOD LIKE SUMEJLON.PERSONALKOD 
   FIELD PRISTYP LIKE SUMEJLON.PRISTYP 
   FIELD TYPKOD LIKE SUMEJLON.TYPKOD 
   FIELD VECKOKORD LIKE SUMEJLON.VECKOKORD
   INDEX AONR IS PRIMARY AONR DELNR DATUM PERSONALKOD PRISTYP TYPKOD.
DEFINE VARIABLE befvar LIKE  PERSONALTAB.BEFATTNING NO-UNDO.
DEFINE VARIABLE enkpris AS DECIMAL NO-UNDO.
DEFINE VARIABLE kvalpris AS DECIMAL NO-UNDO.   
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
DEFINE VARIABLE aonrtvar LIKE AONRTAB.AONR NO-UNDO.  
DEFINE VARIABLE delnrtvar LIKE AONRTAB.DELNR NO-UNDO.  
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
DEFINE VARIABLE varprisi LIKE TIDREGITAB.PRIS NO-UNDO.
DEFINE VARIABLE varprisiproj LIKE TIDREGITAB.PRIS NO-UNDO.
DEFINE VARIABLE sumtidrec AS RECID NO-UNDO.
DEFINE VARIABLE debkredvar AS INTEGER NO-UNDO.

DEFINE BUFFER pbuff FOR PERSONALTAB.
DEFINE BUFFER tidbuff FOR TIDREGITAB.
DEFINE BUFFER sumfbuff FOR SUMTID.
DEFINE BUFFER veckobuff FOR VECKONATT.
DEFINE QUERY sumq FOR SUMTIDDAG.

DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
DEFINE VARIABLE tidq AS HANDLE.
DEFINE VARIABLE bh AS HANDLE.
DEFINE VARIABLE b2h AS HANDLE.     
CREATE BUFFER bh FOR TABLE "TIDREGITAB".
CREATE BUFFER b2h FOR TABLE "PERSONALTAB".
CREATE QUERY tidq.
FIND FIRST TIDREGITAB NO-LOCK NO-ERROR.
{SOKDEF.I}

FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG. 
RUN veckakord_UI.

pkod = " ".
EMPTY TEMP-TABLE ekoforst NO-ERROR. 
EMPTY TEMP-TABLE slutsum NO-ERROR. 
EMPTY TEMP-TABLE oversum NO-ERROR. 
EMPTY TEMP-TABLE traktsum NO-ERROR. 
EMPTY TEMP-TABLE tilltab NO-ERROR. 
EMPTY TEMP-TABLE overtidhj NO-ERROR. 
EMPTY TEMP-TABLE restid NO-ERROR. 

RUN namn_UI.          /*NAMN SUMTIDDAG*/



/*SUMMERIG/DAG*/
   
   
/* **********************  Internal Procedures  *********************** */
PROCEDURE veckakord_UI:
   
   ASSIGN 
   kollvecka = "FELQ"
   pkod = " ".
   EMPTY TEMP-TABLE ekoforst NO-ERROR. 
   EMPTY TEMP-TABLE slutsum NO-ERROR. 
   EMPTY TEMP-TABLE oversum NO-ERROR. 
   EMPTY TEMP-TABLE traktsum NO-ERROR. 
   EMPTY TEMP-TABLE tilltab NO-ERROR. 
   EMPTY TEMP-TABLE overtidhj NO-ERROR. 
   EMPTY TEMP-TABLE restid NO-ERROR. 
    
   
   RUN sumtid_UI.   /*TID FÖR NYSS VECKOKÖRDA*/
   ASSIGN   
   pkod = " ".
   RUN sumtill_UI.    /*TILL FÖR NYSS VECKOKÖRDA*/  
   
END PROCEDURE.

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
         ejlon.BEFATTNING = TIDREGITAB.OVERTIDTILL
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
      restid.BEFATTNING = TIDREGITAB.OVERTIDTILL
      restid.OMRADE = PERSONALTAB.OMRADE 
      restid.PERSONALKOD = TIDREGITAB.PERSONALKOD 
      restid.DATUM = TIDREGITAB.DATUM     
      restid.ARTAL = YEAR(TIDREGITAB.DATUM)
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
      /*RESTID TILL OVERTID  MULTI + 1*/
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 1
      soktemp.SOKINT[1] = varforetypval[4]
      soktemp.SOKCHAR[2] = pkod
      soktemp.SOKCHAR[3] = 'RESTID...'
      soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL
      soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
      {SOKANROP.I}
      nytid = lonantal.
      RUN TIMSEK.P.
      lonantal = (sekunder / 3600).		 
      ovbelopp = (soktemp.SOKDECI[1] + (soktemp.SOKDECI[1] * multi)) * lonantal.        
      inder = FALSE.
   END. 
   ELSE IF typover = "REL" THEN DO:      
      /*RESTID TILL LONTILL MULT*/
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 1
      soktemp.SOKINT[1] = varforetypval[4]
      soktemp.SOKCHAR[2] = pkod
      soktemp.SOKCHAR[3] = 'RESTID...'
      soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL
      soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
      {SOKANROP.I}
      nytid = lonantal.
      RUN TIMSEK.P.
      lonantal = (sekunder / 3600).		 
      lonbeloppv = (soktemp.SOKDECI[1] * multi) * lonantal.                  
   END. 
   ELSE IF typover = "RE2" OR typover = "RE3" THEN DO:
      /*re2 = LÖNETILL MED MULTI TILL LÖNTILL MULTI + 1*/
      /*re3 = LÖNETILL MED MULTI TILL LÖNTILL MULTI*/
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 1
      soktemp.SOKINT[1] = varforetypval[4]
      soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
      soktemp.SOKCHAR[3] = 'TOT.PRIS.'
      soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL
      soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
      {SOKANROP.I}
      /*BAKAB*/   
      {INTDEB.I}
      IF globforetag = "NORD" OR globforetag = "ESAN" OR globforetag = "ESMA" THEN DO:
         IF SUBSTRING(PERSONALTAB.OMRADE,1,1) = "4" AND SUBSTRING(AONRTAB.OMRADE,1,1) = "4" THEN DO:
            musz = musz.            
         END.
         ELSE IF PERSONALTAB.OMRADE NE AONRTAB.OMRADE THEN DO:           
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = varforetypval[4]
            soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
            soktemp.SOKCHAR[3] = "INTERNPRI"
            soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL
            soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
            {SOKANROP.I}
            {INTDEB.I}            
         END.
      END.
      ASSIGN               
      nytid = lonantal.
      RUN TIMSEK.P.
      lonantal = (sekunder / 3600).		 
      lonbeloppv = (soktemp.SOKDECI[1] + (soktemp.SOKDECI[1] * multi)) * lonantal.                    
      IF typover = "RE3" THEN lonbeloppv = soktemp.SOKDECI[1] * multi * lonantal.                    
   END.
   ELSE IF typover = "OVE" THEN DO:    
      /*LÖNETILL MED MULTI TILL ÖVERTID MULTI + 1*/
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 1
      soktemp.SOKINT[1] = varforetypval[4]
      soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
      soktemp.SOKCHAR[3] = 'TOT.PRIS.'
      soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL
      soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
      {SOKANROP.I}
      /*BAKAB*/
      {INTDEB.I}   
      IF globforetag = "NORD" OR globforetag = "ESAN" OR globforetag = "ESMA" THEN DO:
         IF SUBSTRING(PERSONALTAB.OMRADE,1,1) = "4" AND SUBSTRING(AONRTAB.OMRADE,1,1) = "4" THEN DO:
            musz = musz.            
         END.
         ELSE IF PERSONALTAB.OMRADE NE AONRTAB.OMRADE THEN DO:           
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = varforetypval[4]
            soktemp.SOKCHAR[2] = TIDREGITAB.PERSONALKOD
            soktemp.SOKCHAR[3] = "INTERNPRI"
            soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL
            soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
            {SOKANROP.I}
            {INTDEB.I}            
         END.
      END.
      nytid = lonantal.
      RUN TIMSEK.P.
      lonantal = (sekunder / 3600).	                      
      ovbelopp = (soktemp.SOKDECI[1] + (soktemp.SOKDECI[1] * multi)) * lonantal.       
      inder = FALSE.
   END. 
   ELSE IF typover = "OV2" THEN DO:    
      /*LÖNETILL MED MULTI TILL ÖVERTID OBS ENDAST MULTI*/
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 1
      soktemp.SOKINT[1] = varforetypval[4]
      soktemp.SOKCHAR[2] = TIDREGITAB.PERSONALKOD
      soktemp.SOKCHAR[3] = 'TOT.PRIS.'
      soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL
      soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
      {SOKANROP.I}
      /*BAKAB*/
      {INTDEB.I}   
      IF globforetag = "NORD" OR globforetag = "ESAN" OR globforetag = "ESMA" THEN DO:
         IF SUBSTRING(PERSONALTAB.OMRADE,1,1) = "4" AND SUBSTRING(AONRTAB.OMRADE,1,1) = "4" THEN DO:
            musz = musz.            
         END.
         ELSE IF PERSONALTAB.OMRADE NE AONRTAB.OMRADE THEN DO:           
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = varforetypval[4]
            soktemp.SOKCHAR[2] = TIDREGITAB.PERSONALKOD
            soktemp.SOKCHAR[3] = 'INTERNPRI'
            soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL
            soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
            {SOKANROP.I}
            {INTDEB.I}
            
         END.
      END.
      nytid = lonantal.
      RUN TIMSEK.P.
      lonantal = (sekunder / 3600).	                           		 
      ovbelopp = (soktemp.SOKDECI[1] * multi) * lonantal.       
      inder = FALSE.
   END.
   ELSE IF typover = "TRA" THEN DO:
      trabelopp = lonantal * LONTILL.ERSATTNING.
      inder = FALSE.
   END.
   ELSE DO:
      /*LÖN TILL LÖN*/
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
         IF globforetag  = "SUND" THEN DO:
            IF lonkod = "8379" OR 
            lonkod = "8388" OR 
            lonkod = "7211" OR 
            lonkod = "7212" THEN DO:
               lonbeloppv = lonbeloppv * -1.
            END.
         END.
         IF globforetag  = "NORD" OR globforetag = "ESAN" OR globforetag = "ESMA"
         OR globforetag = "ETA" THEN DO:
            IF lonkod = "FRUK" OR 
            lonkod = "LUNC" OR 
            lonkod = "MIDD" OR 
            lonkod = "FRLU" OR
            lonkod = "FRMI" OR 
            lonkod = "LUMI" OR 
            lonkod = "FLMI" THEN DO:
               lonkod = "850".
               lonbeloppv = lonbeloppv * -1.
            END.
         END.
         IF globforetag = 'GRAN' OR globforetag = "GADM"  OR 
         globforetag = "GRIT" OR globforetag = "GKAL"   THEN DO:                                                   
            IF lonkod = "720" OR 
            lonkod = "721" OR 
            lonkod = "722" OR 
            lonkod = "723" OR 
            lonkod = "724" OR 
            lonkod = "725" OR 
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
         ejlon.BEFATTNING = TIDREGITAB.OVERTIDTILL
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
   traantal = traantal * debkredvar.
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
         tilltab.BEFATTNING = TIDREGITAB.OVERTIDTILL
         tilltab.OMRADE = omrpers 
         tilltab.PERSONALKOD = TIDREGITAB.PERSONALKOD  
         tilltab.AONR = TIDREGITAB.AONR  
         tilltab.DELNR = TIDREGITAB.DELNR  
         tilltab.PRISTYP = "RESTID..."   
         tilltab.DATUM = regdatum
         tilltab.ARTAL = YEAR(regdatum)
         tilltab.TBELOPP = tilltab.TBELOPP + trabelopp 
         tilltab.OBELOPP = tilltab.OBELOPP + ovbelopp
         tilltab.LBELOPP = tilltab.LBELOPP + resbelopp.
      END.
      ELSE DO:
         FIND FIRST tilltab WHERE tilltab.DATUM = regdatum AND 
         tilltab.PERSONALKOD = TIDREGITAB.PERSONALKOD AND 
         tilltab.AONR = TIDREGITAB.AONR AND tilltab.DELNR = TIDREGITAB.DELNR AND
         tilltab.PRISTYP = TIDREGITAB.PRISTYP  AND 
         tilltab.BEFATTNING = TIDREGITAB.OVERTIDTILL 
         USE-INDEX PERSONALKOD EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE tilltab THEN DO: 
            CREATE tilltab.                   
         END.
         ASSIGN   
         tilltab.BEFATTNING = TIDREGITAB.OVERTIDTILL
         tilltab.OMRADE = omrpers 
         tilltab.PERSONALKOD = TIDREGITAB.PERSONALKOD  
         tilltab.AONR = TIDREGITAB.AONR  
         tilltab.DELNR = TIDREGITAB.DELNR  
         tilltab.PRISTYP = TIDREGITAB.PRISTYP   
         tilltab.DATUM = regdatum
         tilltab.ARTAL = YEAR(regdatum)
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
   DEFINE INPUT PARAMETER tidpervar LIKE TIDREGITAB.PERSONALKOD NO-UNDO.
   DEFINE INPUT PARAMETER tidaonrvar LIKE TIDREGITAB.AONR NO-UNDO.
   DEFINE INPUT PARAMETER tiddelnrvar LIKE TIDREGITAB.DELNR NO-UNDO.
   DEFINE INPUT PARAMETER tidprisvar LIKE TIDREGITAB.PRIS NO-UNDO.
   DEFINE INPUT PARAMETER tiddatvar AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER tidbefvar AS CHARACTER NO-UNDO.
   IF globforetag = "NORD" OR globforetag = "ESAN" OR globforetag = "ESMA"
   OR globforetag = "ETA" THEN DO:
      IF nattpers NE tidpervar THEN DO:
         ASSIGN
         nattaoomr = " "
         nattaonr = " "
         nattdelnr = 0.
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = tidpervar
         USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
         nattpers = tidpervar.
         IF nattaonr = tidaonrvar AND nattdelnr = tiddelnrvar THEN DO:
            nattaonr = nattaonr.
         END.
         ELSE DO:
            FIND FIRST AONRTAB WHERE 
            AONRTAB.AONR = tidaonrvar AND AONRTAB.DELNR = tiddelnrvar 
            USE-INDEX AONR NO-LOCK NO-ERROR.
            ASSIGN 
            nattaoomr = AONRTAB.OMRADE
            nattaonr = tidaonrvar 
            nattdelnr = tiddelnrvar.
         END.    
         IF nattaoomr NE " " THEN DO:          
            /*BAKAB*/
            {INTDEB.I}               
            IF SUBSTRING(PERSONALTAB.OMRADE,1,1) = "4" AND SUBSTRING(AONRTAB.OMRADE,1,1) = "4" THEN DO:
               nattpris = tidprisvar.            
            END.                     
            ELSE IF PERSONALTAB.OMRADE NE AONRTAB.OMRADE THEN DO:                                
               {SOKSTART.I}         
               ASSIGN                
               soktemp.SOKVAL = 1
               soktemp.SOKINT[1] = varforetypval[4]
               soktemp.SOKCHAR[2] = tidpervar
               soktemp.SOKCHAR[3] = 'INTERNPRI'
               soktemp.SOKCHAR[4] = tidbefvar
               soktemp.SOKDATE[1] = tiddatvar.
               {SOKANROP.I}              
               nattpris = soktemp.SOKDECI[1].                 
               {INTDEB.I}
            END.
            ELSE DO:
               nattpris = tidprisvar.
            END.     
         END. 
         ELSE DO:
            nattpris = tidprisvar.
         END.           
      END.
      ELSE DO:  
         IF nattaonr = tidaonrvar AND nattdelnr = tiddelnrvar THEN DO:
            nattaonr = nattaonr.
         END.                   
         ELSE DO:
            FIND FIRST AONRTAB WHERE 
            AONRTAB.AONR = tidaonrvar AND AONRTAB.DELNR = tiddelnrvar 
            USE-INDEX AONR NO-LOCK NO-ERROR.
            ASSIGN   
            nattaoomr = AONRTAB.OMRADE
            nattaonr = tidaonrvar 
            nattdelnr = tiddelnrvar.
         END.    
         IF nattaoomr NE " " THEN DO:          
            /*BAKAB*/
            {INTDEB.I}               
            IF SUBSTRING(PERSONALTAB.OMRADE,1,1) = "4" AND SUBSTRING(AONRTAB.OMRADE,1,1) = "4" THEN DO:
               nattpris = tidprisvar.            
            END.
            ELSE IF PERSONALTAB.OMRADE NE AONRTAB.OMRADE THEN DO:                                    
               {SOKSTART.I}         
               ASSIGN                
               soktemp.SOKVAL = 1
               soktemp.SOKINT[1] = varforetypval[4]
               soktemp.SOKCHAR[2] = tidpervar
               soktemp.SOKCHAR[3] = 'INTERNPRI'
               soktemp.SOKCHAR[4] = tidbefvar
               soktemp.SOKDATE[1] = tiddatvar.
               {SOKANROP.I}
               nattpris = soktemp.SOKDECI[1].    
                {INTDEB.I}                
            END.
            ELSE DO:
               nattpris = tidprisvar.
            END.     
         END. 
         ELSE DO:
            nattpris = tidprisvar.
         END.   
      END.    
   END.
   ELSE DO: 
      nattpris = tidprisvar.
   END.
END PROCEDURE.
PROCEDURE nattpris2_UI. 
   DEFINE INPUT PARAMETER tidpervar LIKE TIDREGITAB.PERSONALKOD NO-UNDO.
   DEFINE INPUT PARAMETER tidaonrvar LIKE TIDREGITAB.AONR NO-UNDO.
   DEFINE INPUT PARAMETER tiddelnrvar LIKE TIDREGITAB.DELNR NO-UNDO.
   DEFINE INPUT PARAMETER tidprisvar LIKE TIDREGITAB.PRIS NO-UNDO.
   DEFINE INPUT PARAMETER tiddatvar AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER tidbefvar AS CHARACTER NO-UNDO.
   IF globforetag = "NORD" OR globforetag = "ESAN" OR globforetag = "ESMA"
   OR globforetag = "ETA" THEN DO:
      FIND FIRST AONRTAB WHERE 
      AONRTAB.AONR = tidaonrvar AND AONRTAB.DELNR = tiddelnrvar 
      USE-INDEX AONR NO-LOCK NO-ERROR.
      /*BAKAB*/
      {INTDEB.I}         
      IF SUBSTRING(omrpers,1,1) = "4" AND SUBSTRING(AONRTAB.OMRADE,1,1) = "4" THEN DO:
         nattpris = totpristim.            
      END.
      ELSE IF omrpers = AONRTAB.OMRADE THEN DO:
         nattpris = totpristim.
      END.    
      ELSE DO:
         {SOKSTART.I}         
         ASSIGN                
         soktemp.SOKVAL = 1
         soktemp.SOKINT[1] = varforetypval[4]
         soktemp.SOKCHAR[2] = tidpervar
         soktemp.SOKCHAR[3] = 'INTERNPRI'
         soktemp.SOKCHAR[4] = tidbefvar
         soktemp.SOKDATE[1] = tiddatvar.
         {SOKANROP.I}
         nattpris = soktemp.SOKDECI[1].            
         {INTDEB.I}        
      END.
   END.         
   ELSE DO: 
      nattpris = totpristim.
   END.
END PROCEDURE.	   

PROCEDURE pers_UI:
   DEFINE INPUT PARAMETER tidpervar LIKE TIDREGITAB.PERSONALKOD NO-UNDO.
   DEFINE INPUT PARAMETER tiddatvar AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER funkvar AS CHARACTER NO-UNDO.
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod 
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.  
   IF AVAILABLE PERSONALTAB THEN DO:
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 1
      soktemp.SOKINT[1] = varforetypval[4]
      soktemp.SOKCHAR[2] = tidpervar
      soktemp.SOKCHAR[3] = 'TOT.PRIS.'
      soktemp.SOKCHAR[4] = PERSONALTAB.BEFATTNING
      soktemp.SOKDATE[1] = tiddatvar.
      {SOKANROP.I}
      ASSIGN
      befvar =  PERSONALTAB.BEFATTNING
      kodanst = ANSTFORMTAB.KOD                                                
      totpristim = soktemp.SOKDECI[1] 
      omrpers = PERSONALTAB.OMRADE      
      traav = PERSONALTAB.TRAAVTAL.      
   END.   
END PROCEDURE.

PROCEDURE sumtid_UI:
    /*
   nollkor
   OPEN QUERY tidq FOR EACH TIDREGITAB WHERE YEAR(TIDREGITAB.DATUM) = vilketartal AND
   TIDREGITAB.VECKOKORD NE "" AND    
   TIDREGITAB.TIDLOG = TRUE NO-LOCK BY TIDREGITAB.VECKOKORD BY
   TIDREGITAB.AONR BY TIDREGITAB.DELNR BY TIDREGITAB.PERSONALKOD.   
   */
   OPEN QUERY tidq FOR EACH TIDREGITAB WHERE 
   TIDREGITAB.VECKOKORD = kollvecka AND    
   TIDREGITAB.TIDLOG = TRUE NO-LOCK BY TIDREGITAB.VECKOKORD BY
   TIDREGITAB.AONR BY TIDREGITAB.DELNR BY TIDREGITAB.PERSONALKOD.   
   GET FIRST tidq NO-LOCK.        
   DO WHILE AVAILABLE(TIDREGITAB):
      regdatum = TIDREGITAB.DATUM.

      IF pkod NE TIDREGITAB.PERSONALKOD THEN DO:
         ejtid = "".
         pkod = TIDREGITAB.PERSONALKOD.
         RUN pers_UI (INPUT TIDREGITAB.PERSONALKOD, INPUT TIDREGITAB.DATUM,INPUT "sumtid_UI").
         aonrtvar = "".
      END.
      IF aonrtvar = TIDREGITAB.AONR AND delnrtvar = TIDREGITAB.DELNR THEN musz = musz.
      ELSE DO:
         ASSIGN
         aonrtvar = TIDREGITAB.AONR 
         delnrtvar = TIDREGITAB.DELNR.
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = TIDREGITAB.AONR AND 
         AONRTAB.DELNR = TIDREGITAB.DELNR
         NO-LOCK NO-ERROR.
         IF AVAILABLE AONRTAB THEN DO:
            IF globforetag = "GRAN"  THEN DO:
               IF befvar = "MONTÖR" THEN DO:
                  RUN TLAGAUTO.P (INPUT "NATT", INPUT RECID(AONRTAB), INPUT "MONTTID").
                  DO TRANSACTION:
                     FIND FIRST AONRTIDLAGE WHERE 
                     AONRTIDLAGE.AONR = AONRTAB.AONR AND 
                     AONRTIDLAGE.DELNR = AONRTAB.DELNR AND
                     AONRTIDLAGE.IDTIDLAG = "MONTTID"
                     EXCLUSIVE-LOCK NO-ERROR.
                     IF TIDREGITAB.DATUM < AONRTIDLAGE.DATUM1 THEN DO:
                        AONRTIDLAGE.DATUM1 = TIDREGITAB.DATUM.
                     END.
                  END.
               END.
               ELSE IF befvar = "BEREDARE" THEN DO:
                  RUN TLAGAUTO.P (INPUT "NATT", INPUT RECID(AONRTAB), INPUT "BERTID").
                  DO TRANSACTION:
                     FIND FIRST AONRTIDLAGE WHERE 
                     AONRTIDLAGE.AONR = AONRTAB.AONR AND 
                     AONRTIDLAGE.DELNR = AONRTAB.DELNR AND
                     AONRTIDLAGE.IDTIDLAG = "BERTID"
                     EXCLUSIVE-LOCK NO-ERROR.
                     IF TIDREGITAB.DATUM < AONRTIDLAGE.DATUM1 THEN DO:
                        AONRTIDLAGE.DATUM1 = TIDREGITAB.DATUM.
                     END.
                  END.
               END.
            END.
         END.         
      END.
      /*ÄVEN ANDRA */               
      IF TIDREGITAB.TRAKTKOD NE "" THEN RUN traktsum_UI.
      IF TIDREGITAB.LONTILLAGG NE " " THEN RUN lonsum_UI.     
      RUN nattpris_UI (INPUT TIDREGITAB.PERSONALKOD, INPUT TIDREGITAB.AONR,
      INPUT TIDREGITAB.DELNR, INPUT TIDREGITAB.PRIS,INPUT TIDREGITAB.DATUM,
      INPUT TIDREGITAB.OVERTIDTILL).
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
         IF TIDREGITAB.LONTILLAGG NE " " THEN DO TRANSACTION: 
            ASSIGN	       
            lonkod = TIDREGITAB.LONTILLAGG
            lonantal = TIDREGITAB.LONTILLANTAL.
            RUN lon_UI.
            RUN skapa_UI.      	            	       
         END.   
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
         ekoforst.BEFATTNING = TIDREGITAB.OVERTIDTILL
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
         IF globforetag = "NORD" OR globforetag = "ESAN" OR globforetag = "ESMA"
         OR globforetag = "ETA" THEN DO:
            ekoforst.OTIMMAR = otidtim1 + otidtim2 + otidtim3.
         END.   
         IF TIDREGITAB.OKOD1 NE " " THEN ASSIGN ekoforst.BELOPP = 0.
         IF TIDREGITAB.OKOD2 NE " " THEN ASSIGN ekoforst.BELOPP = 0.
         IF TIDREGITAB.OKOD3 NE " " THEN ASSIGN ekoforst.BELOPP = 0.
         ASSIGN ekoforst.BELOPP = ekoforst.ANTAL * ekoforst.BELOPP. 
         IF ekoforst.PRISTYP = "RESTID..." THEN DO:              
            ekoforst.OBELOPP = TIDREGITAB.PRIS.
            ASSIGN 
            /*ekoforst.PRISTYP = "TOT.PRIS." */
            ekoforst.OTIMMAR = 0.         
            RUN restid_UI.
         END.                                
      END. /* EJ RESTID MED LÖNT*/
 
      GET NEXT tidq NO-LOCK.
 
   END. 
   
   RUN sumeraallt_UI.           
    
END PROCEDURE.
PROCEDURE sumtill_UI: 
   /*nollkor 
   OPEN QUERY tidq FOR EACH TIDREGITAB WHERE YEAR(TIDREGITAB.DATUM) = vilketartal AND
   TIDREGITAB.VECKOKORD NE "" AND    
   TIDREGITAB.TIDLOG = FALSE NO-LOCK 
   BY TIDREGITAB.VECKOKORD BY  TIDREGITAB.AONR BY TIDREGITAB.DELNR BY TIDREGITAB.PERSONALKOD.
   */ 
   OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.VECKOKORD = kollvecka AND    
   TIDREGITAB.TIDLOG = FALSE NO-LOCK 
   BY TIDREGITAB.VECKOKORD BY  TIDREGITAB.AONR BY TIDREGITAB.DELNR BY TIDREGITAB.PERSONALKOD.
   GET FIRST tidq NO-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB):
      /*ÖVERTIDTILLÄGG*/
      /*ÖVERTIDS KOD FÅR BELOPPET ENKELÖTID OCH KVALÖTID * ANTAL*/      
      IF TIDREGITAB.AONR = "" THEN regdatum = regdatum.
      ELSE DO:         
         regdatum = TIDREGITAB.DATUM.	       
         IF pkod NE TIDREGITAB.PERSONALKOD THEN DO:
            pkod = TIDREGITAB.PERSONALKOD.
            RUN pers_UI (INPUT TIDREGITAB.PERSONALKOD, INPUT TIDREGITAB.DATUM,INPUT "sumtill_UI").
         END.
         RUN noll_UI.
         RUN nattpris2_UI (INPUT TIDREGITAB.PERSONALKOD, INPUT TIDREGITAB.AONR,
         INPUT TIDREGITAB.DELNR, INPUT TIDREGITAB.PRIS,INPUT TIDREGITAB.DATUM,
         INPUT TIDREGITAB.OVERTIDTILL).
         
         IF TIDREGITAB.TRAKTKOD NE "" THEN RUN traktsum_UI.
         IF TIDREGITAB.LONTILLAGG NE " " THEN RUN lonsum_UI.         
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
   
   RUN summertill_UI.    
                
END PROCEDURE.	

PROCEDURE traktsum_UI.
/*SUMTRAKT ENDAST FÖR FAKTURERING alla traktamenten*/   
   IF kollvecka = " " THEN RETURN.
   FIND FIRST TRAKTFLER WHERE TRAKTFLER.TRAAVTAL = traav AND 
   TRAKTFLER.TRAKTKOD = TIDREGITAB.TRAKTKOD  
   USE-INDEX FLTRAKT NO-LOCK NO-ERROR.                      
   CREATE SUMTRAKT.
   ASSIGN
   SUMTRAKT.PERSONALKOD = TIDREGITAB.PERSONALKOD      
   SUMTRAKT.AONR = TIDREGITAB.AONR
   SUMTRAKT.DELNR = TIDREGITAB.DELNR
   SUMTRAKT.DATUM = TIDREGITAB.DATUM
   SUMTRAKT.VECKOKORD = TIDREGITAB.VECKOKORD
   SUMTRAKT.TRAKTKOD = TIDREGITAB.TRAKTKOD
   SUMTRAKT.TRAKTANTAL = TIDREGITAB.TRAKTANTAL.
   IF AVAILABLE TRAKTFLER THEN SUMTRAKT.ENDAGS = FALSE.
   ELSE SUMTRAKT.ENDAGS = TRUE.    
END PROCEDURE.     
PROCEDURE lonsum_UI.   
   /*SUMLON ENDAST FÖR FAKTURERING 
    endast de lönetillägg som skall till fakturering*/
   IF kollvecka = " " THEN RETURN.    
   FIND FIRST LONTILL WHERE LONTILL.KOD = kodanst AND
   LONTILL.LONTILLAGG = TIDREGITAB.LONTILLAGG
   USE-INDEX LON NO-LOCK NO-ERROR.
   IF NOT AVAILABLE LONTILL THEN RETURN.      
   IF SUBSTRING(LONTILL.TYPKOD,5,3) NE "FAK" THEN RETURN.                   
   CREATE SUMLON.
   ASSIGN
   SUMLON.PERSONALKOD = TIDREGITAB.PERSONALKOD      
   SUMLON.AONR = TIDREGITAB.AONR
   SUMLON.DELNR = TIDREGITAB.DELNR
   SUMLON.DATUM = TIDREGITAB.DATUM
   SUMLON.VECKOKORD = TIDREGITAB.VECKOKORD
   SUMLON.LONTILLAGG = TIDREGITAB.LONTILLAGG
   SUMLON.LONTILLANTAL = TIDREGITAB.LONTILLANTAL.   
END PROCEDURE.

PROCEDURE sumeraallt_UI:
   ASSIGN   
   pkod = " "  
   arrhjsum = 0
   arrhjsum2 = 0
   arrhjsum3 = 0.    
   FOR EACH ekoforst BREAK BY ekoforst.DATUM BY ekoforst.PERSONALKOD BY ekoforst.BEFATTNING BY ekoforst.AONR BY 
   ekoforst.DELNR BY ekoforst.PRISTYP:
      IF pkod NE ekoforst.PERSONALKOD THEN DO:
         pkod = ekoforst.PERSONALKOD. 
         RUN pers_UI (INPUT ekoforst.PERSONALKOD, INPUT ekoforst.DATUM,INPUT "sumeraallt_UI").
      END.
      IF ekoforst.OANT1 NE 0  THEN DO:      
         CREATE overtidhj.
         ASSIGN 
         overtidhj.BEFATTNING = ekoforst.BEFATTNING
         overtidhj.PERSONALKOD = ekoforst.PERSONALKOD 
         overtidhj.PRIS = ekoforst.PRIS     
         overtidhj.PRISTYP = ekoforst.PRISTYP
         overtidhj.DATUM = ekoforst.DATUM 
         overtidhj.AONR = ekoforst.AONR 
         overtidhj.DELNR = ekoforst.DELNR 
         overtidhj.BELOPP = ekoforst.OBELOPP * ekoforst.OANT1 
         overtidhj.OVERANTAL = ekoforst.OANT1 
         overtidhj.OVERTIDTILL = ekoforst.OKOD1.                
         IF globforetag = "NORD" OR globforetag = "ESAN" OR globforetag = "ESMA"
         OR globforetag = "SUND" OR globforetag = "GKAL" OR globforetag = "LULE" THEN DO:
            ASSIGN
            ovkod = ekoforst.OKOD1
            ovantal = ekoforst.OANT1
            nattpris = ekoforst.OBELOPP.        
            RUN over_UI.  
            overtidhj.BELOPP = ovbelopp.                          
         END.
           
      END. 
      IF ekoforst.OANT2 NE 0  THEN DO:      
         CREATE overtidhj.      
         ASSIGN
         overtidhj.BEFATTNING = ekoforst.BEFATTNING
         overtidhj.PERSONALKOD = ekoforst.PERSONALKOD 
         overtidhj.PRIS = ekoforst.PRIS     
         overtidhj.PRISTYP = ekoforst.PRISTYP 
         overtidhj.DATUM = ekoforst.DATUM
         overtidhj.AONR = ekoforst.AONR 
         overtidhj.DELNR = ekoforst.DELNR 
         overtidhj.BELOPP = ekoforst.OBELOPP * ekoforst.OANT2   
         overtidhj.OVERANTAL = ekoforst.OANT2
         overtidhj.OVERTIDTILL = ekoforst.OKOD2.
         IF globforetag = "NORD" OR globforetag = "ESAN" OR globforetag = "ESMA"
         OR globforetag = "SUND" OR globforetag = "GKAL" OR globforetag = "LULE" THEN DO:
            ASSIGN
            ovkod = ekoforst.OKOD2
            ovantal = ekoforst.OANT2
            nattpris = ekoforst.OBELOPP.            
            RUN over_UI.  
            overtidhj.BELOPP = ovbelopp.                          
         END.
                               
      END.                    
      IF ekoforst.OANT3 NE 0  THEN DO:      
         CREATE overtidhj.           
         ASSIGN 
         overtidhj.BEFATTNING = ekoforst.BEFATTNING
         overtidhj.PERSONALKOD = ekoforst.PERSONALKOD 
         overtidhj.PRIS = ekoforst.PRIS     
         overtidhj.PRISTYP = ekoforst.PRISTYP
         overtidhj.DATUM = ekoforst.DATUM 
         overtidhj.AONR = ekoforst.AONR 
         overtidhj.DELNR = ekoforst.DELNR 
         overtidhj.BELOPP = ekoforst.OBELOPP * ekoforst.OANT3
         overtidhj.OVERANTAL = ekoforst.OANT3
         overtidhj.OVERTIDTILL = ekoforst.OKOD3. 
         IF globforetag = "NORD" OR globforetag = "ESAN" OR globforetag = "ESMA"
         OR globforetag = "SUND" OR globforetag = "GKAL" OR globforetag = "LULE" THEN DO:
            ASSIGN
            ovkod = ekoforst.OKOD3
            ovantal = ekoforst.OANT3
            nattpris = ekoforst.OBELOPP.              
            RUN over_UI.  
            overtidhj.BELOPP = ovbelopp.                          
         END.
         
      END.                                                             
      ACCUMULATE ekoforst.BELOPP 
      (TOTAL BY ekoforst.DATUM BY ekoforst.PERSONALKOD BY ekoforst.BEFATTNING BY ekoforst.AONR BY 
      ekoforst.DELNR BY ekoforst.PRISTYP). 
      ACCUMULATE ekoforst.ANTAL 
      (TOTAL BY ekoforst.DATUM BY ekoforst.PERSONALKOD BY ekoforst.BEFATTNING BY ekoforst.AONR BY 
      ekoforst.DELNR BY ekoforst.PRISTYP).  
      ACCUMULATE ekoforst.OTIMMAR 
      (TOTAL BY ekoforst.DATUM BY ekoforst.PERSONALKOD BY ekoforst.BEFATTNING BY ekoforst.AONR BY 
      ekoforst.DELNR BY ekoforst.PRISTYP).     
      IF LAST-OF(ekoforst.PRISTYP) THEN DO:
         CREATE slutsum.
         ASSIGN 
         slutsum.BEFATTNING = ekoforst.BEFATTNING
         slutsum.OMRADE = ekoforst.OMRADE 
         slutsum.PERSONALKOD = ekoforst.PERSONALKOD 
         slutsum.PRIS = ekoforst.PRIS     
         slutsum.PRISTYP = ekoforst.PRISTYP 
         slutsum.DATUM = ekoforst.DATUM
         slutsum.ARTAL = YEAR(ekoforst.DATUM) 
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
   FOR EACH overtidhj BREAK BY overtidhj.DATUM BY overtidhj.PERSONALKOD BY overtidhj.BEFATTNING BY
   overtidhj.AONR BY overtidhj.DELNR BY overtidhj.OVERTIDTILL BY overtidhj.PRISTYP:   
      ACCUMULATE overtidhj.BELOPP 
      (TOTAL BY overtidhj.DATUM BY overtidhj.PERSONALKOD BY overtidhj.BEFATTNING BY overtidhj.AONR BY 
      overtidhj.DELNR BY overtidhj.OVERTIDTILL BY overtidhj.PRISTYP). 
      ACCUMULATE overtidhj.OVERANTAL 
      (TOTAL BY overtidhj.DATUM BY overtidhj.PERSONALKOD BY overtidhj.BEFATTNING BY overtidhj.AONR BY
      overtidhj.DELNR BY overtidhj.OVERTIDTILL BY overtidhj.PRISTYP).
      IF LAST-OF(overtidhj.PRISTYP) THEN DO:
         CREATE oversum.
         ASSIGN         
         oversum.BEFATTNING = overtidhj.BEFATTNING
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
   FOR EACH oversum BREAK BY oversum.DATUM BY oversum.PERSONALKOD BY oversum.BEFATTNING BY oversum.AONR BY  
   oversum.DELNR BY oversum.PRISTYP:           
      ACCUMULATE oversum.BELOPP (TOTAL BY oversum.DATUM BY oversum.PERSONALKOD BY oversum.BEFATTNING BY
      oversum.AONR BY oversum.DELNR BY oversum.PRISTYP). 
      ACCUMULATE oversum.ANTAL (TOTAL BY oversum.DATUM BY oversum.PERSONALKOD  BY oversum.BEFATTNING BY 
      oversum.AONR BY oversum.DELNR BY oversum.PRISTYP).
      IF LAST-OF(oversum.PRISTYP) THEN DO:
         FIND FIRST slutsum WHERE slutsum.DATUM = oversum.DATUM AND slutsum.PERSONALKOD = oversum.PERSONALKOD 
         AND slutsum.AONR = oversum.AONR AND slutsum.DELNR = oversum.DELNR AND
         slutsum.PRISTYP = oversum.PRISTYP AND slutsum.BEFATTNING = oversum.BEFATTNING
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
      CREATE SUMTIDDAG.
      ASSIGN     
      SUMTIDDAG.BEFATTNING = slutsum.BEFATTNING
      SUMTIDDAG.OMRADE = slutsum.OMRADE
      SUMTIDDAG.VECKOKORD = kollvecka 
      SUMTIDDAG.PERSONALKOD = slutsum.PERSONALKOD 
      SUMTIDDAG.PRIS = slutsum.PRIS     
      SUMTIDDAG.DATUM = slutsum.DATUM
      SUMTIDDAG.AUTODATUM = TODAY
      SUMTIDDAG.AONR = slutsum.AONR   
      SUMTIDDAG.DELNR = slutsum.DELNR
      SUMTIDDAG.PRISTYP = slutsum.PRISTYP 
      SUMTIDDAG.TIMMAR = slutsum.ANTAL 
      SUMTIDDAG.OTIMMAR = slutsum.OTIMMAR  
      SUMTIDDAG.BELOPP = slutsum.BELOPP
      SUMTIDDAG.OBELOPP = slutsum.OBELOPP
      SUMTIDDAG.TBELOPP = slutsum.TBELOPP
      SUMTIDDAG.IKOSTNAD = slutsum.BELOPP.             
   END.                                  
   FOR EACH restid:     
      CREATE SUMTIDDAG.
      ASSIGN               
      SUMTIDDAG.BEFATTNING = restid.BEFATTNING
      SUMTIDDAG.OMRADE = restid.OMRADE 
      SUMTIDDAG.VECKOKORD = kollvecka                             
      SUMTIDDAG.PERSONALKOD = restid.PERSONALKOD               
      SUMTIDDAG.DATUM = restid.DATUM
      SUMTIDDAG.AUTODATUM = TODAY
      SUMTIDDAG.AONR = restid.AONR   
      SUMTIDDAG.DELNR = restid.DELNR
      SUMTIDDAG.PRISTYP = restid.PRISTYP 
      SUMTIDDAG.TIMMAR = restid.ANTAL.              
   END.   
END PROCEDURE.
PROCEDURE summertill_UI:
   FOR EACH tilltab USE-INDEX PERSONALKOD NO-LOCK:
      DO TRANSACTION:
         FIND FIRST SUMTIDDAG WHERE 
         SUMTIDDAG.DATUM = tilltab.DATUM AND 
         SUMTIDDAG.VECKOKORD = kollvecka AND 
         SUMTIDDAG.PERSONALKOD = tilltab.PERSONALKOD AND
         SUMTIDDAG.AONR = tilltab.AONR AND SUMTIDDAG.DELNR = tilltab.DELNR AND
         SUMTIDDAG.OMRADE = tilltab.OMRADE AND SUMTIDDAG.PRISTYP = tilltab.PRISTYP  
         USE-INDEX PERSONALKOD EXCLUSIVE-LOCK NO-ERROR.       
         IF NOT AVAILABLE SUMTIDDAG THEN DO:
            CREATE SUMTIDDAG.
            ASSIGN
            SUMTIDDAG.OMRADE = tilltab.OMRADE. 
         END.   
         ELSE DO:
            sumtidrec = RECID(SUMTIDDAG).
            IF SUMTIDDAG.PRISTYP = 'RESTID...'  AND SUMTIDDAG.OBELOPP = 0 THEN DO:
               OPEN QUERY sumq FOR EACH SUMTIDDAG WHERE 
               SUMTIDDAG.VECKOKORD = kollvecka AND 
               SUMTIDDAG.DATUM = tilltab.DATUM AND 
               SUMTIDDAG.PERSONALKOD = tilltab.PERSONALKOD AND
               SUMTIDDAG.AONR = tilltab.AONR AND 
               SUMTIDDAG.DELNR = tilltab.DELNR AND
               SUMTIDDAG.OMRADE = tilltab.OMRADE AND 
               SUMTIDDAG.PRISTYP = tilltab.PRISTYP  
               USE-INDEX PERSONALKOD NO-LOCK.
               GET FIRST sumq NO-LOCK.
               DO WHILE AVAILABLE(SUMTIDDAG):
                  IF SUMTIDDAG.OBELOPP NE 0 THEN sumtidrec = RECID(SUMTIDDAG).
                  GET NEXT sumq NO-LOCK.
               END.                  
            END.
            FIND SUMTIDDAG WHERE RECID(SUMTIDDAG) = sumtidrec EXCLUSIVE-LOCK NO-ERROR.
         END.
         ASSIGN                              
         SUMTIDDAG.VECKOKORD = kollvecka  
         SUMTIDDAG.PERSONALKOD = tilltab.PERSONALKOD    
         SUMTIDDAG.DATUM = tilltab.DATUM 
         SUMTIDDAG.AUTODATUM = TODAY
         SUMTIDDAG.AONR = tilltab.AONR   
         SUMTIDDAG.DELNR = tilltab.DELNR 
         SUMTIDDAG.PRISTYP = tilltab.PRISTYP    
         SUMTIDDAG.LONKOST = SUMTIDDAG.LONKOST + tilltab.LBELOPP
         SUMTIDDAG.OBELOPP = SUMTIDDAG.OBELOPP + tilltab.OBELOPP
         SUMTIDDAG.TBELOPP = SUMTIDDAG.TBELOPP + tilltab.TBELOPP
         SUMTIDDAG.IKOSTNAD = SUMTIDDAG.IKOSTNAD + tilltab.IKOST.
         /*
         SUMTIDDAG.IKOSTNAD = (SUMTIDDAG.IKOSTNAD + tilltab.LBELOPP) - tilltab.IKOST.
         */           
      END. 
   END.
END PROCEDURE.



PROCEDURE namn_UI: 
   RELEASE AONRTAB NO-ERROR.                  
   ASSIGN
   nattaonr = ""
   nattdelnr = 0.
   OPEN QUERY sumq FOR EACH SUMTIDDAG WHERE SUMTIDDAG.VECKOKORD = kollvecka NO-LOCK
   BY SUMTIDDAG.VECKOKORD BY SUMTIDDAG.AONR by SUMTIDDAG.DELNR.    
   DO TRANSACTION:   
      GET FIRST sumq EXCLUSIVE-LOCK.
      IF AVAILABLE SUMTIDDAG THEN DO:         
         IF NOT AVAILABLE AONRTAB THEN DO:
            ASSIGN  
            nattaonr = SUMTIDDAG.AONR 
            nattdelnr = SUMTIDDAG.DELNR. 
            FIND FIRST AONRTAB WHERE AONRTAB.AONR = SUMTIDDAG.AONR AND 
            AONRTAB.DELNR = SUMTIDDAG.DELNR
            USE-INDEX AONR NO-LOCK NO-ERROR. 
         END.            
         ELSE DO:                                                          
            IF nattaonr = SUMTIDDAG.AONR AND nattdelnr = SUMTIDDAG.DELNR 
            THEN nattaonr = nattaonr.
            ELSE DO:
               ASSIGN  
               nattaonr = SUMTIDDAG.AONR 
               nattdelnr = SUMTIDDAG.DELNR. 
               FIND FIRST AONRTAB WHERE AONRTAB.AONR = SUMTIDDAG.AONR AND 
               AONRTAB.DELNR = SUMTIDDAG.DELNR
               USE-INDEX AONR NO-LOCK NO-ERROR. 
            END.
         END.    
         IF AVAILABLE AONRTAB THEN DO: 
            ASSIGN 
            SUMTIDDAG.FASTAAONR = AONRTAB.FASTAAONR          
            SUMTIDDAG.GEOMRADE = AONRTAB.OMRADE 
            SUMTIDDAG.ORT = AONRTAB.ORT.
            IF AONRTAB.OMRADE = "" THEN ASSIGN SUMTIDDAG.GEOMRADE = SUMTIDDAG.OMRADE.           
         END.
      END.                                              
   END.   
   AONAMN2:
   REPEAT TRANSACTION:                       
      GET NEXT sumq EXCLUSIVE-LOCK.         
      IF NOT AVAILABLE SUMTIDDAG THEN LEAVE AONAMN2.
      ELSE DO:
         IF NOT AVAILABLE AONRTAB THEN DO:
            ASSIGN  
            nattaonr = SUMTIDDAG.AONR 
            nattdelnr = SUMTIDDAG.DELNR. 
            FIND FIRST AONRTAB WHERE AONRTAB.AONR = SUMTIDDAG.AONR AND 
            AONRTAB.DELNR = SUMTIDDAG.DELNR
            USE-INDEX AONR NO-LOCK NO-ERROR. 
         END.            
         ELSE DO:                                                          
            IF nattaonr = SUMTIDDAG.AONR AND nattdelnr = SUMTIDDAG.DELNR 
            THEN nattaonr = nattaonr.
            ELSE DO:
               ASSIGN  
               nattaonr = SUMTIDDAG.AONR 
               nattdelnr = SUMTIDDAG.DELNR. 
               FIND FIRST AONRTAB WHERE AONRTAB.AONR = SUMTIDDAG.AONR AND 
               AONRTAB.DELNR = SUMTIDDAG.DELNR
               USE-INDEX AONR NO-LOCK NO-ERROR. 
            END.
         END.    
         IF AVAILABLE AONRTAB THEN DO: 
            ASSIGN 
            SUMTIDDAG.FASTAAONR = AONRTAB.FASTAAONR          
            SUMTIDDAG.GEOMRADE = AONRTAB.OMRADE 
            SUMTIDDAG.ORT = AONRTAB.ORT.
            IF AONRTAB.OMRADE = "" THEN ASSIGN SUMTIDDAG.GEOMRADE = SUMTIDDAG.OMRADE.           
         END.
      END.     
   END.     
   CLOSE QUERY sumq.                        
   pkod = " ".
   OPEN QUERY sumq FOR EACH SUMTIDDAG WHERE SUMTIDDAG.VECKOKORD = kollvecka NO-LOCK 
   BY SUMTIDDAG.VECKOKORD BY SUMTIDDAG.PERSONALKOD.  
   DO TRANSACTION:   
      GET FIRST sumq EXCLUSIVE-LOCK.
      IF AVAILABLE SUMTIDDAG THEN DO:
         RUN satt_UI.          
      END. 
   END.          
   NAMN2:
   REPEAT TRANSACTION:                    
      GET NEXT sumq EXCLUSIVE-LOCK.          
      IF NOT AVAILABLE SUMTIDDAG THEN LEAVE NAMN2.
      ELSE DO:
         RUN satt_UI.                  
      END.                
   END.
END PROCEDURE.
PROCEDURE satt_UI:
   IF pkod NE SUMTIDDAG.PERSONALKOD THEN DO:
      pkod = SUMTIDDAG.PERSONALKOD.
      FIND FIRST PERSONALTAB WHERE 
      PERSONALTAB.PERSONALKOD = SUMTIDDAG.PERSONALKOD
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR. 
      IF AVAILABLE PERSONALTAB THEN DO:
         FIND FIRST ANSTFORMTAB WHERE 
         ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
         USE-INDEX ANSTF NO-LOCK NO-ERROR.         
      END. 
   END.
   IF AVAILABLE PERSONALTAB THEN DO:                          
      IF SUMTIDDAG.GEOMRADE = "" THEN SUMTIDDAG.GEOMRADE = PERSONALTAB.OMRADE.
      ASSIGN 
      SUMTIDDAG.FORNAMN = PERSONALTAB.FORNAMN 
      SUMTIDDAG.EFTERNAMN = PERSONALTAB.EFTERNAMN
      SUMTIDDAG.OMRADE = PERSONALTAB.OMRADE.
      befvar = SUBSTRING(SUMTIDDAG.BEFATTNING,1,20).
      SUMTIDDAG.BEFATTNING = "".
      IF befvar = "" THEN SUBSTRING(SUMTIDDAG.BEFATTNING,1,20) = PERSONALTAB.BEFATTNING .
      ELSE SUBSTRING(SUMTIDDAG.BEFATTNING,1,20) = befvar.
      ASSIGN
      SUBSTRING(SUMTIDDAG.BEFATTNING,21) = ANSTFORMTAB.KOD         
      SUMTIDDAG.PERSMASK = PERSONALTAB.PERSMASK.  
      IF globforetag = "NORD"OR globforetag = "ESAN" OR globforetag = "ESMA" THEN DO:
         {INTDEB.I}
         /*PROJ2 990517 000327*/
         {SOKSTART.I}
         ASSIGN
         soktemp.SOKVAL = 1
         soktemp.SOKINT[1] = varforetypval[4]
         soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
         soktemp.SOKCHAR[3] = "INTERNPRI"
         soktemp.SOKCHAR[4] = befvar
         soktemp.SOKDATE[1] = SUMTIDDAG.DATUM.
         {SOKANROP.I}
         varprisi = soktemp.SOKDECI[1].          
         ASSIGN SUMTIDDAG.PRISI = varprisi.
         {INTDEB.I}
      END.
      ELSE DO: 
         ASSIGN SUMTIDDAG.PRISI = SUMTIDDAG.PRIS.
      END.
   END.
END PROCEDURE.   


