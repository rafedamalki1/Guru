  /*LULEEKO.P*/
/*  
 BARA PERSONER MED BRAVO = TRUE 
   AUTO:
   TRAKTEMENTE "TRA" kostnad = antal * TRAKTATAB.ERSATTNING
   ÖVERTID "OVE"     kostnad = (TIDREGITAB.PRIS + (TIDREGITAB.PRIS * multi)) * antal.
   RESTID "RES"      kostnad = (TIDREGITAB.PRIS + (TIDREGITAB.PRIS* multi)) * antal.        
   RESTID "REL"      kostnad = (TIDREGITAB.PRIS * multi) * resantal. 
   ÖVERTID "OVE"     kostnad = (TIDFEL.PRIS + (TIDFEL.PRIS * multi)) * antal.
   RESTID "RES"      kostnad = (TIDFEL.PRIS + (TIDFEL.PRIS* multi)) * antal.        
   RESTID "REL"      kostnad = (TIDFEL.PRIS * multi) * resantal. 
   MAN: 
   LÖNETILLÄGG
      "EJE"  = KODER SOM EJ TAS MED
      "OVE"          kostnad = (TOT.PRIS. + (TOT.PRIS. * multi)) * antal.
      "OV2"          kostnad = (TOT.PRIS. * multi) * antal.
      LÖNETILLÄGG SOM SKALL TOLKAS SOM ÖVERTID
      "RES"          kostnad = (RESTID... + (RESTID... * multi)) * antal.        
      "REL"          kostnad = (RESTID... * multi) * resantal. 
      "RE2"          kostnad = (TOT.PRIS. + (TOT.PRIS. * multi)) * antal.        
      "RE3"          kostnad = (TOT.PRIS. * multi)) * antal.        
      "LON"
      "BIL"    SOM "LON" 
      om ENHET "KR"  kostnad = antal.
      om ENHET "TI"  kostnad = LONTILL.ERSATTNING * antal.
      övrigt  kostnad = LONTILL.ERSATTNING * antal.
       OBS ! MÅLTIDSAVDRAG HÅRDKODAS       
   ÖVERTID  
     "OVE"           kostnad = (TOT.PRIS. + (TOT.PRIS. * multi)) * antal.
   BEREDSKAP
                     kostnad = antal * BERKOD.ERSATTNING    
    
*/
{LESAMMAN.I}  
DEFINE INPUT PARAMETER feltider AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER vkdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER skarpvar AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER korvar AS CHARACTER NO-UNDO.
RUN sammut_UI (INPUT 1).
{SOKDEF.I}
&Scoped-define NEW NEW
{GLOBVAR2.I}
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO. 


DEFINE VARIABLE aoomrade LIKE AONRTAB.OMRADE NO-UNDO.          
DEFINE VARIABLE timtid AS DECIMAL FORMAT "99.99" NO-UNDO. 
DEFINE VARIABLE multi AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE berkostnad AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE totpristim AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE reskod LIKE TIDREGITAB.LONTILLAGG NO-UNDO. 
DEFINE VARIABLE resantal LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.
DEFINE VARIABLE resbelopp LIKE EKRAPPRESULT.ELONBELOPP NO-UNDO. 
DEFINE VARIABLE ovkod LIKE TIDREGITAB.OVERTIDTILL NO-UNDO. 
DEFINE VARIABLE ovantal LIKE TIDREGITAB.OVERANTAL NO-UNDO.
DEFINE VARIABLE ovbelopp LIKE EKRAPPRESULT.EOVERBELOPP NO-UNDO. 
DEFINE VARIABLE trakod LIKE TIDREGITAB.TRAKTKOD NO-UNDO. 
DEFINE VARIABLE traantal LIKE TIDREGITAB.TRAKTANTAL NO-UNDO.
DEFINE VARIABLE trabelopp LIKE EKRAPPRESULT.ETRAKTBELOPP NO-UNDO. 
DEFINE VARIABLE lonkod LIKE TIDREGITAB.LONTILLAGG NO-UNDO. 
DEFINE VARIABLE lonantal LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.
DEFINE VARIABLE lonbelopp LIKE EKRAPPRESULT.ELONBELOPP NO-UNDO. 
DEFINE VARIABLE berkodvar LIKE TIDREGITAB.BEREDSKAP NO-UNDO. 
DEFINE VARIABLE bbantal LIKE TIDREGITAB.BERANTAL NO-UNDO.
DEFINE VARIABLE berbelopp LIKE EKRAPPRESULT.EBERBELOPP NO-UNDO.
DEFINE VARIABLE pkod LIKE PERSONALTAB.PERSONALKOD NO-UNDO.
DEFINE VARIABLE typover LIKE EKRAPPRESULT.ERESULTENH NO-UNDO. 
DEFINE VARIABLE typdatum AS DATE NO-UNDO. 
DEFINE VARIABLE kodanst LIKE ANSTFORMTAB.KOD NO-UNDO.
DEFINE VARIABLE sattfore AS CHARACTER NO-UNDO.

DEFINE QUERY persq FOR PERSONALTAB.  
DEFINE NEW SHARED TEMP-TABLE foretemp NO-UNDO
   FIELD FTG AS CHARACTER
   INDEX FTG IS PRIMARY FTG.
DEFINE TEMP-TABLE ekoforst
   FIELD FTG AS CHARACTER
   FIELD ORT AS CHARACTER
   FIELD ENY LIKE EKRAPPRESULT.ENY 
   FIELD EPERSONALKOD LIKE EKRAPPRESULT.EPERSONALKOD 
   FIELD EPROJEKT LIKE EKRAPPRESULT.EPROJEKT
   FIELD DELNR LIKE AONRTAB.DELNR 
   FIELD EORG LIKE EKRAPPRESULT.EORG     
   FIELD EGEO LIKE EKRAPPRESULT.EGEO  
   FIELD FELDEBKRED AS LOGICAL
   FIELD EVERDATUM AS DATE 
   FIELD EOVERJA LIKE EKRAPPRESULT.EOVERJA
   FIELD EBELOPP LIKE  EKRAPPRESULT.EBELOPP 
   FIELD EANTAL LIKE EKRAPPRESULT.EANTAL 
   FIELD ETIMMAR LIKE EKRAPPRESULT.EANTAL                
   FIELD ELONTILLAGG LIKE EKRAPPRESULT.ELONTILLAGG 
   FIELD ELONTILLANTAL LIKE EKRAPPRESULT.ELONTILLAN
   FIELD ELONBELOPP LIKE EKRAPPRESULT.ELONBELOPP         
   FIELD PERSTYP AS CHARACTER         
   FIELD BEFATTNING AS CHARACTER   
   INDEX PERSORG IS PRIMARY EPERSONALKOD EORG EGEO EPROJEKT DELNR ASCENDING.
pkod = "".
DEFINE TEMP-TABLE tidertemp NO-UNDO LIKE TIDFEL.
DEFINE QUERY tidq FOR tidertemp.
OUTPUT TO VALUE(samvar) APPEND.
   PUT "LULEKO.P " STRING(TIME,"HH:MM") " " TODAY SKIP.
OUTPUT CLOSE.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
{FORESTYR.I}
OPEN QUERY persq 
FOR EACH PERSONALTAB WHERE PERSONALTAB.BRAVO = TRUE AND PERSONALTAB.ORGCHEF NE "" USE-INDEX PERSONALKOD NO-LOCK.
GET FIRST persq NO-LOCK.
DO WHILE AVAILABLE(PERSONALTAB):
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
   aoomrade = "".
   
   IF pkod NE PERSONALTAB.PERSONALKOD THEN DO:
      pkod = PERSONALTAB.PERSONALKOD.             
      FIND FIRST ANSTFORMTAB WHERE
      ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
      USE-INDEX ANSTF NO-LOCK NO-ERROR.
      kodanst = ANSTFORMTAB.KOD.
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
      FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
      FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
      sattfore = JURPERS.JUDID.
      FIND FIRST foretemp WHERE foretemp.FTG = sattfore  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE foretemp THEN DO:
         CREATE foretemp.
         foretemp.FTG = sattfore.
      END.
   END.  
   EMPTY TEMP-TABLE tidertemp NO-ERROR. 
   IF feltider = TRUE THEN DO:      
      IF korvar = "" THEN DO:      
         FOR EACH TIDFEL WHERE
         TIDFEL.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDFEL.SKICKA = TRUE AND SUBSTRING(TIDFEL.FELKORD,1,9) = korvar AND TIDFEL.DATUM <= vkdatum
         USE-INDEX PKOD NO-LOCK:
            CREATE tidertemp.
            BUFFER-COPY TIDFEL TO tidertemp.
         END.
      END.
      ELSE DO:
         FOR EACH TIDFEL WHERE
         TIDFEL.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDFEL.SKICKA = FALSE AND SUBSTRING(TIDFEL.FELKORD,1,9) = korvar AND TIDFEL.DATUM <= vkdatum
         USE-INDEX PKOD NO-LOCK:
            CREATE tidertemp.
            BUFFER-COPY TIDFEL TO tidertemp.
         END.
      END.
   END.
   ELSE DO:
      FOR EACH TIDREGITAB WHERE
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.GODKAND BEGINS "G" AND SUBSTRING(TIDREGITAB.VECKOKORD,1,9) = korvar AND
      TIDREGITAB.PRISTYP NE "FRÅNVARO." AND TIDREGITAB.DATUM <= vkdatum
      USE-INDEX PSTART NO-LOCK:
         CREATE tidertemp.
         BUFFER-COPY TIDREGITAB TO tidertemp.
      END.
 
   END.
   OPEN QUERY tidq FOR EACH tidertemp NO-LOCK.
   GET FIRST tidq NO-LOCK.
   DO WHILE AVAILABLE(tidertemp):
      totpristim = tidertemp.PRIS.
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = tidertemp.AONR AND
      AONRTAB.DELNR = tidertemp.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE AONRTAB THEN DO:
         aoomrade = PERSONALTAB.OMRADE. 
         IF tidertemp.AONR = "" THEN aoomrade = PERSONALTAB.OMRADE. 
         ELSE DO:
            OUTPUT TO "D:\ELPOOL\DELAD\PRO9S\felvk.txt" APPEND.
            PUT tidertemp.PERSONALKOD tidertemp.DATUM " AONR " tidertemp.AONR tidertemp.DELNR " FINNS EJ! KONTAKTA ELPOOL" SKIP. 
            OUTPUT CLOSE.            
         END.
      END.    
      IF AVAILABLE AONRTAB THEN DO:
         aoomrade = AONRTAB.OMRADE.   
         IF AONRTAB.OMRADE = "" THEN aoomrade = PERSONALTAB.OMRADE.   
         IF sattfore = "02" OR sattfore = "04" THEN DO:
            musz = TRUE.	        	      	       
         END.
         ELSE IF tidertemp.PRISTYP = 'FRÅNVARO.' THEN DO: 
            musz = TRUE.	        	      	       
         END.
         ELSE IF tidertemp.PRISTYP = 'RESTID...' THEN DO: 
            musz = TRUE.	        	      	       
         END.
         /*"EJ DEBI.."*/
         ELSE IF tidertemp.PRISTYP = 'EJ DEBI..' THEN DO: 
            musz = TRUE.	        	      	       
         END.
         ELSE IF tidertemp.PRISTYP = 'EJ.KOSTN.' THEN DO: 
            musz = TRUE.	        	      	       
         END.
         IF AONRTAB.PRISTYP = 'FRÅNVARO.' THEN DO: 
            musz = TRUE.	        	      	       
         END.
         ELSE IF AONRTAB.PRISTYP = 'RESTID...' THEN DO: 
            musz = TRUE.	        	      	       
         END.
         /*"EJ DEBI.."*/
         ELSE IF AONRTAB.PRISTYP = 'EJ DEBI..' THEN DO: 
            musz = TRUE.	        	      	       
         END.
         /*"EJ DEBI.."*/           
         ELSE IF AONRTAB.PRISTYP = 'EJ.KOSTN.' THEN DO: 
            musz = TRUE.	        	      	       
         END.
         IF musz = FALSE THEN DO:
            IF tidertemp.OVERTIDTILL = "" THEN tidertemp.OVERTIDTILL = PERSONALTAB.BEFATTNING.
            RUN tidmed_UI.        
         END.
      END.
      musz = FALSE.
      GET NEXT tidq NO-LOCK.    
   END.  /*ELSE DO*/
   GET NEXT persq NO-LOCK.
END.   /*FOR EACH*/     
FIND FIRST ekoforst NO-LOCK NO-ERROR.
IF AVAILABLE ekoforst THEN DO:
   OUTPUT TO VALUE(samvar) APPEND.
   PUT "START EKO2.P " STRING(TIME,"HH:MM") " " TODAY SKIP.
   OUTPUT CLOSE.
   RUN LULEEKO2.P (INPUT samvar,INPUT feltider,INPUT vkdatum,INPUT skarpvar,INPUT korvar,INPUT TABLE ekoforst).
END.
RUN sammut_UI (INPUT 2).
PROCEDURE tidmed_UI:
   IF MONTH(tidertemp.DATUM) = 12 THEN DO:
      typdatum = DATE(12,31,YEAR(tidertemp.DATUM)).
   END.
   ELSE DO:                    
      typdatum = DATE((MONTH(tidertemp.DATUM)+ 1),01,YEAR(tidertemp.DATUM)) - 1.
   END.
   
   IF tidertemp.OKOD1 NE " "      THEN musz = TRUE.
   ELSE IF tidertemp.OKOD2 NE " " THEN musz = TRUE.
   ELSE IF tidertemp.OKOD3 NE " " THEN musz = TRUE.
   /*TIMMAR OCH PENNGAR*/  
   IF musz = FALSE THEN DO: 
      FIND FIRST ekoforst WHERE
      ekoforst.FTG = sattfore AND
      ekoforst.PERSTYP = "" AND
      ekoforst.BEFATTNING = tidertemp.OVERTIDTILL AND
      ekoforst.ENY = FALSE AND
      ekoforst.EPERSONALKOD = tidertemp.PERSONALKOD AND 
      ekoforst.EORG = PERSONALTAB.OMRADE AND 
      ekoforst.EGEO = aoomrade AND
      ekoforst.EPROJEKT = tidertemp.AONR AND         
      ekoforst.DELNR = tidertemp.DELNR AND
      ekoforst.EVERDATUM = typdatum AND     
      ekoforst.FELDEBKRED = tidertemp.DEBET
      USE-INDEX PERSORG NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ekoforst THEN DO TRANSACTION:
         CREATE ekoforst.
         ASSIGN 
         ekoforst.FTG = sattfore 
         ekoforst.ORT = AONRTAB.ORT
         ekoforst.PERSTYP = ""
         ekoforst.BEFATTNING = tidertemp.OVERTIDTILL 
         ekoforst.ENY = FALSE
         ekoforst.EPERSONALKOD = tidertemp.PERSONALKOD
         ekoforst.EPROJEKT = tidertemp.AONR 
         ekoforst.DELNR = tidertemp.DELNR 
         ekoforst.EORG = PERSONALTAB.OMRADE
         ekoforst.FELDEBKRED = tidertemp.DEBET
         ekoforst.EGEO = aoomrade                         
         ekoforst.EVERDATUM = typdatum.
      END.                                          
      nytid = tidertemp.TOTALT.
      RUN TIMSEK.P.
      timtid = (sekunder / 3600).
      ASSIGN ekoforst.EBELOPP = ekoforst.EBELOPP +
      (tidertemp.PRIS * timtid)              
      ekoforst.ETIMMAR = ekoforst.ETIMMAR + timtid
      ekoforst.EANTAL = ekoforst.EANTAL + timtid.
      IF tidertemp.LONTILLAGG NE " " THEN DO:
         ASSIGN
         lonkod = tidertemp.LONTILLAGG
         lonantal = tidertemp.LONTILLANTAL.
         RUN lon_UI.                    
      END. 
   END.
   ELSE DO:
            /*ÖVERTIDTILLÄGG*/
      /*ÖVERTIDS KOD FÅR BELOPPET ENKELÖTID OCH KVALÖTID * ANTAL*/               
      musz = FALSE.
      ASSIGN
      ovbelopp = 0                    
      ovkod = ""
      ovantal = 0.
      IF tidertemp.OKOD1 NE " " THEN DO: 
         ASSIGN
         ovkod = tidertemp.OKOD1
         nytid = tidertemp.OANT1.
         RUN TIMSEK.P.
         ovantal = (sekunder / 3600). 
         RUN over_UI.        
         nytid = tidertemp.TOTALT.
         RUN TIMSEK.P.                  
         ASSIGN ekoforst.EANTAL = ekoforst.EANTAL + (sekunder / 3600).                              
      END.          
      ASSIGN
      ovbelopp = 0                    
      ovkod = ""
      ovantal = 0.
      IF tidertemp.OKOD2 NE " " THEN DO: 
         ASSIGN
         ovkod = tidertemp.OKOD2
         nytid = tidertemp.OANT2.
         RUN TIMSEK.P.
         ovantal = (sekunder / 3600).        
         RUN over_UI.   
         nytid = tidertemp.TOTALT.
         RUN TIMSEK.P.                  
         ASSIGN ekoforst.EANTAL = ekoforst.EANTAL + (sekunder / 3600).                           
      END.                 
      ASSIGN
      ovbelopp = 0
      ovkod = ""
      ovantal = 0.
      IF tidertemp.OKOD3 NE " " THEN DO: 
         ASSIGN
         ovkod = tidertemp.OKOD3
         nytid = tidertemp.OANT3.
         RUN TIMSEK.P.
         ovantal = (sekunder / 3600).
         RUN over_UI.                   
         nytid = tidertemp.TOTALT.
         RUN TIMSEK.P.                  
         ASSIGN ekoforst.EANTAL = ekoforst.EANTAL + (sekunder / 3600).                              
      END. 
      IF tidertemp.LONTILLAGG NE " " THEN DO:
         ASSIGN
         lonkod = tidertemp.LONTILLAGG
         lonantal = tidertemp.LONTILLANTAL.
         RUN lon_UI.                    
      END. 
   END.            
   
END PROCEDURE.
PROCEDURE over_UI:
   DEFINE VARIABLE enkekval AS CHARACTER NO-UNDO.
 
   multi = 0.
   FIND FIRST OVERKOD WHERE OVERKOD.KOD = kodanst AND
   OVERKOD.OVERTIDTILL = ovkod 
   USE-INDEX OVER NO-LOCK NO-ERROR.
   IF NOT AVAILABLE OVERKOD THEN RETURN.  
   ASSIGN
   enkekval = OVERKOD.ENKEL
   multi = OVERKOD.MULTIP
   typover = "OVE".
   IF multi = 0 THEN enkekval = "".
   FIND FIRST ekoforst WHERE      
   ekoforst.FTG = sattfore AND
   ekoforst.BEFATTNING = tidertemp.OVERTIDTILL AND
   ekoforst.PERSTYP = enkekval AND  
   ekoforst.ENY = FALSE AND
   ekoforst.EPERSONALKOD = tidertemp.PERSONALKOD AND
   ekoforst.EORG = PERSONALTAB.OMRADE AND 
   ekoforst.EGEO = aoomrade AND 
   ekoforst.EVERDATUM = typdatum AND
   ekoforst.EPROJEKT = tidertemp.AONR AND
   ekoforst.DELNR = tidertemp.DELNR AND
   ekoforst.ELONTILLAG = ovkod AND
   ekoforst.FELDEBKRED = tidertemp.DEBET
   USE-INDEX PERSORG EXCLUSIVE-LOCK NO-ERROR.  
   IF NOT AVAILABLE ekoforst THEN DO:
      CREATE ekoforst.
   END.
   IF tidertemp.PRISTYP = "RESTID..." THEN typover = "RES".  
   ovbelopp = (tidertemp.PRIS + (tidertemp.PRIS * multi)) * ovantal.
   IF tidertemp.PRIS = 0 AND tidertemp.TIDLOG = FALSE THEN DO:
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 1
      soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
      soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
      soktemp.SOKCHAR[3] = 'TOT.PRIS.'
      soktemp.SOKCHAR[4] = tidertemp.OVERTIDTILL 
      soktemp.SOKDATE[1] = tidertemp.DATUM.
      {SOKANROP.I}
      ovbelopp = (soktemp.SOKDECI[1] + (soktemp.SOKDECI[1] * multi)) * ovantal.
   END.         
   ASSIGN    
   ekoforst.FTG = sattfore 
   ekoforst.ORT = AONRTAB.ORT
   ekoforst.BEFATTNING = tidertemp.OVERTIDTILL 
   ekoforst.PERSTYP = enkekval
   ekoforst.ENY = FALSE
   ekoforst.EPERSONALKOD = tidertemp.PERSONALKOD 
   ekoforst.EPROJEKT = tidertemp.AONR 
   ekoforst.DELNR = tidertemp.DELNR 
   ekoforst.EORG = PERSONALTAB.OMRADE  
   ekoforst.FELDEBKRED = tidertemp.DEBET
   ekoforst.EGEO = aoomrade            
   ekoforst.EVERDATUM = typdatum
   ekoforst.ELONTILLAGG = ovkod
   ekoforst.ELONTILLANTAL = ekoforst.ELONTILLANTAL + ovantal  
   ekoforst.ELONBELOPP = ekoforst.ELONBELOPP + ovbelopp.                           
END PROCEDURE.

PROCEDURE lon_UI:
   IF musz = musz THEN RETURN.
   musz = FALSE.
   multi = 0.      
   typover = "LON".  
   FIND FIRST LONTILL WHERE LONTILL.KOD = kodanst AND
   LONTILL.LONTILLAGG = lonkod 
   USE-INDEX LON NO-LOCK NO-ERROR.
   IF NOT AVAILABLE LONTILL THEN RETURN.
   ASSIGN
   typover = SUBSTRING(LONTILL.TYPKOD,1,3)     
   multi =  LONTILL.MULTIP.     
   IF typover = "EJE" THEN RETURN.     
   ELSE IF typover = "OVE" THEN typover = typover.
   ELSE DO:
      RETURN.
   END.
   IF tidertemp.PRIS = 0 AND tidertemp.TIDLOG = FALSE THEN DO:
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 1
      soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
      soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
      soktemp.SOKCHAR[3] = 'TOT.PRIS.'
      soktemp.SOKCHAR[4] = tidertemp.OVERTIDTILL 
      soktemp.SOKDATE[1] = tidertemp.DATUM.
      {SOKANROP.I}
      totpristim = soktemp.SOKDECI[1].      
   END.         
   IF typover = "OVE" THEN DO:    
      nytid = lonantal.
      RUN TIMSEK.P.
      lonantal = (sekunder / 3600).                                                    
      lonbelopp = (totpristim + (totpristim * multi)) * lonantal. 
   END. 
   
   IF ekoforst.ELONTILLAGG = lonkod OR ekoforst.ELONTILLAGG = "" THEN DO:
      sattfore = sattfore.
   END.                                   
   ELSE DO:
      FIND FIRST ekoforst WHERE
      ekoforst.FTG = sattfore AND
      ekoforst.BEFATTNING = tidertemp.OVERTIDTILL AND
      ekoforst.ENY = FALSE AND
      ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD AND 
      ekoforst.EORG = PERSONALTAB.OMRADE AND 
      ekoforst.EGEO = aoomrade AND
      ekoforst.EVERDATUM = typdatum AND
      ekoforst.EPROJEKT = TIDREGITAB.AONR AND
      ekoforst.DELNR = TIDREGITAB.DELNR AND
      ekoforst.ELONTILLAGG = lonkod
      USE-INDEX PERSORG EXCLUSIVE-LOCK NO-ERROR.  
      IF NOT AVAILABLE ekoforst THEN DO:
         CREATE ekoforst.
      END.
   END.   
   ASSIGN 
   ekoforst.FTG = sattfore 
   ekoforst.ENY = FALSE
   ekoforst.BEFATTNING = tidertemp.OVERTIDTILL 
   ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD 
   ekoforst.EPROJEKT = TIDREGITAB.AONR 
   ekoforst.DELNR = TIDREGITAB.DELNR 
   ekoforst.EORG = PERSONALTAB.OMRADE  
   ekoforst.EGEO = aoomrade 
   ekoforst.ELONTILLAGG = lonkod       
   ekoforst.EVERDATUM = typdatum
   ekoforst.ELONTILLANTAL = ekoforst.ELONTILLANTAL + lonantal  
   ekoforst.ELONBELOPP = ekoforst.ELONBELOPP + lonbelopp. 
END PROCEDURE.                
 
