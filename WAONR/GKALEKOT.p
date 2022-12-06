  /*GKALEKOT.P*/
/*  
  formappvar = "M7\". 800 småland SEFAB 0880630   och Småländsk = 180
formappvar = "N6\". 100      "GKEAB"  0880610  Elnät = 100
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
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO. 
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO. 
DEFINE VARIABLE ekrid AS RECID EXTENT 50 NO-UNDO.
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
DEFINE TEMP-TABLE ekoforst
   FIELD FTG AS CHARACTER
   FIELD ENY LIKE EKRAPPRESULT.ENY 
   FIELD EPERSONALKOD LIKE EKRAPPRESULT.EPERSONALKOD 
   FIELD EPROJEKT LIKE EKRAPPRESULT.EPROJEKT
   FIELD DELNR LIKE AONRTAB.DELNR 
   FIELD EORG LIKE EKRAPPRESULT.EORG     
   FIELD EGEO LIKE EKRAPPRESULT.EGEO  
   FIELD FELDEBKRED AS LOGICAL
   FIELD EVERDATUM AS DATE 
   FIELD EOVERJA LIKE EKRAPPRESULT.EOVERJA
   FIELD ERESULTENH LIKE EKRAPPRESULT.ERESULTENH 
   FIELD EBELOPP LIKE  EKRAPPRESULT.EBELOPP 
   FIELD EANTAL LIKE EKRAPPRESULT.EANTAL 
   FIELD ETIMMAR LIKE EKRAPPRESULT.EANTAL                
   FIELD ELONTILLAGG LIKE EKRAPPRESULT.ELONTILLAGG 
   FIELD ELONTILLANTAL LIKE EKRAPPRESULT.ELONTILLANTAL    
   FIELD ELONBELOPP LIKE EKRAPPRESULT.ELONBELOPP         
   FIELD PERSTYP AS CHARACTER         
   INDEX PERSORG IS PRIMARY EPERSONALKOD EORG EGEO EPROJEKT DELNR ASCENDING.
pkod = "".
DEFINE TEMP-TABLE tidertemp NO-UNDO LIKE TIDFEL.
DEFINE QUERY tidq FOR tidertemp.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
{FORESTYR.I}
OPEN QUERY persq 
FOR EACH PERSONALTAB WHERE PERSONALTAB.BRAVO = TRUE USE-INDEX PERSONALKOD NO-LOCK.
GET FIRST persq NO-LOCK.
DO WHILE AVAILABLE(PERSONALTAB):
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
   FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
   FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
   IF JURPERS.JUDID = "GKEAB" THEN sattfore = "100".
   ELSE IF JURPERS.JUDID = "GSEAB" THEN sattfore = "180".
   ELSE IF JURPERS.JUDID = "KEV" THEN sattfore = "105".
   /*SÄVSJÖ*/
   ELSE IF JURPERS.JUDID = "SEAB" THEN sattfore = "150".
   ELSE sattfore = "999".
   IF sattfore = "999" THEN sattfore = sattfore.
   ELSE DO:
      aoomrade = "".
      persrec = RECID(PERSONALTAB).   
      IF pkod NE PERSONALTAB.PERSONALKOD THEN DO:
         pkod = PERSONALTAB.PERSONALKOD.        
         persrec = RECID(PERSONALTAB).
         FIND FIRST ANSTFORMTAB WHERE
         ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
         USE-INDEX ANSTF NO-LOCK NO-ERROR.
         kodanst = ANSTFORMTAB.KOD.
      END.  
      EMPTY TEMP-TABLE tidertemp NO-ERROR. 
      IF feltider = TRUE THEN DO:      
         IF korvar = "" THEN DO:      
            FOR EACH TIDFEL WHERE
            TIDFEL.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
            TIDFEL.SKICKA = TRUE AND TIDFEL.FELKORD = korvar AND TIDFEL.DATUM <= vkdatum
            USE-INDEX PKOD NO-LOCK:
               CREATE tidertemp.
               BUFFER-COPY TIDFEL TO tidertemp.
            END.
         END.
         ELSE DO:
            FOR EACH TIDFEL WHERE
            TIDFEL.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
            TIDFEL.SKICKA = FALSE AND TIDFEL.FELKORD = korvar AND TIDFEL.DATUM <= vkdatum
            USE-INDEX PKOD NO-LOCK:
               CREATE tidertemp.
               BUFFER-COPY TIDFEL TO tidertemp.
            END.
         END.
      END.
      ELSE DO:
         FOR EACH TIDREGITAB WHERE
         TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.GODKAND BEGINS "G" AND TIDREGITAB.VECKOKORD = korvar AND
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
               {AMERICANEUROPEAN.I}
               OUTPUT TO "d:\delad\SERVER\pro9s\felvk.txt" APPEND.
               PUT tidertemp.PERSONALKOD tidertemp.DATUM " AONR " tidertemp.AONR tidertemp.DELNR " FINNS EJ! KONTAKTA ELPOOL" SKIP. 
               OUTPUT CLOSE.  
               {EUROPEANAMERICAN.I}          
            END.
         END.    
         IF AVAILABLE AONRTAB THEN DO:
            aoomrade = AONRTAB.OMRADE.   
            IF AONRTAB.OMRADE = "" THEN aoomrade = PERSONALTAB.OMRADE.   
            IF tidertemp.PRISTYP = 'FRÅNVARO.' THEN DO: 
               musz = TRUE.	        	      	       
            END.
            ELSE IF tidertemp.PRISTYP = 'RESTID...' THEN DO: 
               musz = TRUE.	        	      	       
            END.
            /*"EJ DEBI.."*/
            ELSE IF tidertemp.PRISTYP = 'EJ DEBI..' THEN DO: 
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
            IF musz = FALSE THEN DO:
               RUN tidmed_UI.        
            END.
         END.
         musz = FALSE.
         GET NEXT tidq NO-LOCK.    
      END.  /*ELSE DO*/
   END.
   GET NEXT persq NO-LOCK.
END.   /*FOR EACH*/     


RUN GKALEKO2aswT.P (INPUT feltider,INPUT vkdatum,INPUT skarpvar,INPUT korvar,INPUT TABLE ekoforst).
RUN sammut_UI (INPUT 2).
PROCEDURE tidmed_UI:
   IF MONTH(TODAY) = 01 THEN DO:
      typdatum = DATE(12,31,YEAR(TODAY) - 1).
   END.
   ELSE DO:   
      typdatum = DATE((MONTH(TODAY)),01,YEAR(TODAY)) - 1.
   END.
   
   FIND FIRST ekoforst WHERE
   ekoforst.FTG = sattfore AND
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
      ekoforst.ENY = FALSE
      ekoforst.EPERSONALKOD = tidertemp.PERSONALKOD
      ekoforst.EPROJEKT = tidertemp.AONR 
      ekoforst.DELNR = tidertemp.DELNR 
      ekoforst.EORG = PERSONALTAB.OMRADE
      ekoforst.FELDEBKRED = tidertemp.DEBET
      ekoforst.EGEO = aoomrade                         
      ekoforst.EVERDATUM = typdatum.
   END.                                             
   ekrid[1] = RECID(ekoforst).
   /*RESTID*/                         
   IF tidertemp.PRISTYP = 'RESTID...' AND tidertemp.LONTILLAGG NE " " THEN DO TRANSACTION: 
      FIND ekoforst WHERE RECID(ekoforst) = ekrid[1]
      EXCLUSIVE-LOCK NO-ERROR.     
      ASSIGN
      resbelopp = 0                    
      reskod = ""
      resantal = 0
      reskod = tidertemp.LONTILLAGG
      nytid = tidertemp.LONTILLANTAL.
      RUN TIMSEK.P.
      resantal = (sekunder / 3600).
      RUN res_UI.                    
      /*TRAKTAMENTE*/ 
      FIND ekoforst WHERE RECID(ekoforst) = ekrid[1]
      EXCLUSIVE-LOCK NO-ERROR. 
      ASSIGN
      trabelopp = 0
      trakod = ""
      traantal = 0.
      IF tidertemp.TRAKTKOD NE " " THEN DO:
         ASSIGN
         trakod = tidertemp.TRAKTKOD
         traantal = tidertemp.TRAKTANTAL.
         IF  traantal > 0 THEN RUN tra_UI.
         ELSE DO:
            ASSIGN
            trabelopp = 0
            trakod = ""
            traantal = 0.                    
         END.
      END.                                                                          
   END.            
   ELSE DO:
      /*TIDREGISTRERING*/                                 
      /*TIMMAR OCH PENNGAR*/  
      DO TRANSACTION:
         FIND ekoforst WHERE RECID(ekoforst) = ekrid[1]
         EXCLUSIVE-LOCK NO-ERROR.                                    
         ASSIGN ekoforst.EOVERJA = FALSE. 
         IF tidertemp.OKOD1 NE " " THEN ASSIGN ekoforst.EOVERJA = TRUE.
         ELSE IF tidertemp.OKOD2 NE " " THEN ASSIGN ekoforst.EOVERJA = TRUE.
         ELSE IF tidertemp.OKOD3 NE " " THEN ASSIGN ekoforst.EOVERJA = TRUE.
         IF ekoforst.EOVERJA = FALSE AND tidertemp.TOTALT > 0 THEN DO:                  
            nytid = tidertemp.TOTALT.
            RUN TIMSEK.P.
            timtid = (sekunder / 3600).
            ASSIGN ekoforst.EBELOPP = ekoforst.EBELOPP +
            (tidertemp.PRIS * timtid)              
            ekoforst.ETIMMAR = ekoforst.ETIMMAR + timtid
            ekoforst.EANTAL = ekoforst.EANTAL + timtid.
         END.
      END.
      DO TRANSACTION:   
         /*ÖVERTIDTILLÄGG*/
         /*ÖVERTIDS KOD FÅR BELOPPET ENKELÖTID OCH KVALÖTID * ANTAL*/               
         FIND ekoforst WHERE RECID(ekoforst) = ekrid[1]
         EXCLUSIVE-LOCK NO-ERROR.   
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
         FIND ekoforst WHERE RECID(ekoforst) = ekrid[1]
         EXCLUSIVE-LOCK NO-ERROR.   
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
            /*
            nytid = tidertemp.TOTALT.
            RUN TIMSEK.P.                  
            ASSIGN ekoforst.EANTAL = ekoforst.EANTAL + (sekunder / 3600).  
            */                 
         END.                 
         FIND ekoforst WHERE RECID(ekoforst) = ekrid[1]
         EXCLUSIVE-LOCK NO-ERROR. 
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
            /*
            nytid = tidertemp.TOTALT.
            RUN TIMSEK.P.                  
            ASSIGN ekoforst.EANTAL = ekoforst.EANTAL + (sekunder / 3600).                      
            */
         END.           
      END.      
      DO TRANSACTION:       
         /*LONETILLAGG*/
         FIND ekoforst WHERE RECID(ekoforst) = ekrid[1]
         EXCLUSIVE-LOCK NO-ERROR.         
         ASSIGN
         lonbelopp = 0
         lonkod = ""
         lonantal = 0.
         IF tidertemp.LONTILLAGG NE " " THEN DO:
            ASSIGN
            lonkod = tidertemp.LONTILLAGG
            lonantal = tidertemp.LONTILLANTAL.
            RUN lon_UI.                    
         END.        
      END.
   END.
END PROCEDURE.
PROCEDURE res_UI:
   IF musz = musz THEN RETURN. 
   FIND FIRST LONTILL WHERE LONTILL.KOD = kodanst AND
   LONTILL.LONTILLAGG = reskod 
   USE-INDEX LON NO-LOCK NO-ERROR.
   IF NOT AVAILABLE LONTILL THEN RETURN.
   ASSIGN
   typover = SUBSTRING(LONTILL.TYPKOD,1,3)     
   multi =  LONTILL.MULTIP.                
   IF typover = "EJE" THEN RETURN.    
   IF typover = "REL" THEN resbelopp = (tidertemp.PRIS * multi) * resantal. 
   ELSE resbelopp = (tidertemp.PRIS + (tidertemp.PRIS * multi)) * resantal. 
   IF ekoforst.ELONTILLAGG = reskod OR ekoforst.ELONTILLAGG = "" THEN DO:
      persrec = persrec.
   END.                                   
   ELSE DO:
      FIND FIRST ekoforst WHERE
      ekoforst.FTG = sattfore AND
      ekoforst.ERESULTENH = typover AND
      ekoforst.ENY = FALSE AND
      ekoforst.EPERSONALKOD = tidertemp.PERSONALKOD AND   
      ekoforst.EORG = PERSONALTAB.OMRADE AND 
      ekoforst.EGEO = aoomrade AND              
      ekoforst.EVERDATUM = typdatum AND
      ekoforst.EPROJEKT = tidertemp.AONR AND
      ekoforst.DELNR = tidertemp.DELNR AND
      ekoforst.ELONTILLAGG = reskod AND
      ekoforst.FELDEBKRED = tidertemp.DEBET
      USE-INDEX PERSORG EXCLUSIVE-LOCK NO-ERROR.  
      IF NOT AVAILABLE ekoforst THEN DO:
         CREATE ekoforst.
      END.
   END.   
   ASSIGN 
   ekoforst.FTG = sattfore
   ekoforst.ENY = FALSE
   ekoforst.ERESULTENH = typover
   ekoforst.EPERSONALKOD = tidertemp.PERSONALKOD
   ekoforst.EPROJEKT = tidertemp.AONR 
   ekoforst.DELNR = tidertemp.DELNR 
   ekoforst.EORG = PERSONALTAB.OMRADE  
   ekoforst.FELDEBKRED = tidertemp.DEBET
   ekoforst.EGEO = aoomrade            
   ekoforst.EVERDATUM = typdatum 
   ekoforst.ELONTILLAGG = reskod
   ekoforst.ELONTILLANTAL = ekoforst.ELONTILLANTAL + resantal  
   ekoforst.ELONBELOPP = ekoforst.ELONBELOPP + resbelopp. 
END PROCEDURE.                 
PROCEDURE lon_UI:
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
   IF typover = "BIL" THEN musz = musz.
   ELSE IF typover = "RE3" THEN musz = musz.
   ELSE RETURN.
   IF typover = "BIL" THEN DO:
      FIND FIRST LONTILL WHERE LONTILL.KOD = kodanst AND
      LONTILL.LONTILLAGG = lonkod USE-INDEX LON NO-LOCK NO-ERROR.
      IF AVAILABLE LONTILL THEN DO:
         IF LONTILL.ENHET = "KR" THEN DO:
            lonbelopp = lonantal.
            lonantal = 0.
         END.
         ELSE IF LONTILL.ENHET = "TI" THEN DO:
            nytid = lonantal.
            RUN TIMSEK.P.
            lonantal = (sekunder / 3600). 
            lonbelopp = lonantal * LONTILL.ERSATTNING.
         END.
         ELSE DO:
             lonbelopp = lonantal * LONTILL.ERSATTNING.           
         END.
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
               lonbelopp = lonbelopp * -1.
         END.                                                  
      END.
      ELSE RETURN.    
   END.
   IF typover = "RE3" THEN DO:
      FIND FIRST LONTILL WHERE LONTILL.KOD = kodanst AND
      LONTILL.LONTILLAGG = lonkod USE-INDEX LON NO-LOCK NO-ERROR.
      IF AVAILABLE LONTILL THEN DO:
         {SOKSTART.I}
         ASSIGN
         soktemp.SOKVAL = 1
         soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
         soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
         soktemp.SOKCHAR[3] = 'TOT.PRIS.'
         soktemp.SOKCHAR[4] = tidertemp.OVERTIDTILL 
         soktemp.SOKDATE[1] = tidertemp.DATUM.
         {SOKANROP.I}
         nytid = lonantal.
         RUN TIMSEK.P.
         lonantal = (sekunder / 3600). 
         lonbelopp = soktemp.SOKDECI[1] * multi * lonantal.
      END.
   END.
   IF ekoforst.ELONTILLAGG = lonkod OR ekoforst.ELONTILLAGG = "" THEN DO:
      persrec = persrec.
   END.                                   
   ELSE DO:
      FIND FIRST ekoforst WHERE
      ekoforst.FTG = sattfore AND
      ekoforst.ERESULTENH = typover AND
      ekoforst.ENY = FALSE AND
      ekoforst.EPERSONALKOD = tidertemp.PERSONALKOD AND 
      ekoforst.EORG = PERSONALTAB.OMRADE AND 
      ekoforst.EGEO = aoomrade AND
      ekoforst.EVERDATUM = typdatum AND
      ekoforst.EPROJEKT = tidertemp.AONR AND
      ekoforst.DELNR = tidertemp.DELNR AND
      ekoforst.ELONTILLAGG = lonkod AND
      ekoforst.FELDEBKRED = tidertemp.DEBET
      USE-INDEX PERSORG EXCLUSIVE-LOCK NO-ERROR.  
      IF NOT AVAILABLE ekoforst THEN DO:
         CREATE ekoforst.
      END.
   END.   
   ASSIGN 
   ekoforst.FTG = sattfore
   ekoforst.ENY = FALSE
   ekoforst.ERESULTENH = typover
   ekoforst.EPERSONALKOD = tidertemp.PERSONALKOD 
   ekoforst.EPROJEKT = tidertemp.AONR 
   ekoforst.DELNR = tidertemp.DELNR 
   ekoforst.EORG = PERSONALTAB.OMRADE  
   ekoforst.FELDEBKRED = tidertemp.DEBET
   ekoforst.EGEO = aoomrade 
   ekoforst.ELONTILLAGG = lonkod       
   ekoforst.EVERDATUM = typdatum
   ekoforst.ELONTILLANTAL = ekoforst.ELONTILLANTAL + lonantal  
   ekoforst.ELONBELOPP = ekoforst.ELONBELOPP + lonbelopp. 
END PROCEDURE.                
PROCEDURE tra_UI:
  IF musz = musz THEN RETURN.
   musz = FALSE.
   multi = 0.         
   typover = "TRA".
   FIND FIRST TRAKTATAB WHERE TRAKTATAB.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
   TRAKTATAB.TRAKTKOD = trakod USE-INDEX TRAKTKOD NO-LOCK NO-ERROR.
   IF AVAILABLE TRAKTATAB THEN DO:
      trabelopp = traantal * TRAKTATAB.ERSATTNING.           
   END.    
   ELSE RETURN.    
   IF ekoforst.ELONTILLAGG = trakod OR ekoforst.ELONTILLAGG = "" THEN DO:
      persrec = persrec.
   END.                                   
   ELSE DO:
      FIND FIRST ekoforst WHERE
      ekoforst.FTG = sattfore AND
      ekoforst.ERESULTENH = typover AND
      ekoforst.ENY = FALSE AND
      ekoforst.EPERSONALKOD = tidertemp.PERSONALKOD AND
      ekoforst.EORG = PERSONALTAB.OMRADE AND 
      ekoforst.EGEO = aoomrade AND 
      ekoforst.EVERDATUM = typdatum AND
      ekoforst.EPROJEKT = tidertemp.AONR AND
      ekoforst.DELNR = tidertemp.DELNR AND
      ekoforst.ELONTILLAGG = trakod  AND
      ekoforst.FELDEBKRED = tidertemp.DEBET
      USE-INDEX PERSORG EXCLUSIVE-LOCK NO-ERROR.  
      IF NOT AVAILABLE ekoforst THEN DO:
         CREATE ekoforst.
      END.
   END.   
   ASSIGN 
   ekoforst.FTG = sattfore
   ekoforst.ENY = FALSE
   ekoforst.ERESULTENH = typover
   ekoforst.EPERSONALKOD = tidertemp.PERSONALKOD
   ekoforst.EPROJEKT = tidertemp.AONR 
   ekoforst.DELNR = tidertemp.DELNR 
   ekoforst.EORG = PERSONALTAB.OMRADE  
   ekoforst.FELDEBKRED = tidertemp.DEBET
   ekoforst.EGEO = aoomrade            
   ekoforst.EVERDATUM = typdatum
   ekoforst.ELONTILLAGG = trakod
   ekoforst.ELONTILLANTAL = ekoforst.ELONTILLANTAL + traantal  
   ekoforst.ELONBELOPP = ekoforst.ELONBELOPP + trabelopp. 
END PROCEDURE.                      
PROCEDURE over_UI:
   multi = 0.
   FIND FIRST OVERKOD WHERE OVERKOD.KOD = kodanst AND
   OVERKOD.OVERTIDTILL = ovkod 
   USE-INDEX OVER NO-LOCK NO-ERROR.
   IF NOT AVAILABLE OVERKOD THEN RETURN.  
   /*Inlagt 2005-06-21 av Lena mertid ska inte över, ligger med faktor 0*/
   IF OVERKOD.MULTIP = 0 THEN RETURN.
   ASSIGN
   multi = OVERKOD.MULTIP
   typover = "OVE".
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
   IF ekoforst.ELONTILLAGG = ovkod OR ekoforst.ELONTILLAGG = "" THEN DO:
      persrec = persrec.
   END.                                   
   ELSE DO:
      FIND FIRST ekoforst WHERE
      ekoforst.FTG = sattfore AND
      ekoforst.PERSTYP = OVERKOD.ENKEL AND  
      ekoforst.ERESULTENH = typover AND
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
   END.   
   ASSIGN 
   ekoforst.FTG = sattfore
   ekoforst.PERSTYP = OVERKOD.ENKEL
   ekoforst.ENY = FALSE
   ekoforst.ERESULTENH = typover
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
PROCEDURE bered_UI:      
   typover = "BER".
   FIND FIRST BERKOD WHERE BERKOD.BEREDSKAP = berkodvar AND
   BERKOD.BEREDSKAPSAVTAL = PERSONALTAB.BEREDSKAPSAVTAL
   NO-LOCK NO-ERROR.
   berbelopp = BERKOD.ERSATTNING * bbantal. 
   IF ekoforst.ELONTILLAGG = berkodvar OR ekoforst.ELONTILLAGG = "" THEN DO:
      persrec = persrec.
   END.                                   
   ELSE DO:
      FIND FIRST ekoforst WHERE
      ekoforst.FTG = sattfore AND
      ekoforst.ERESULTENH = typover AND
      ekoforst.ENY = FALSE AND
      ekoforst.EPERSONALKOD = tidertemp.PERSONALKOD AND
      ekoforst.EORG = PERSONALTAB.OMRADE AND 
      ekoforst.EGEO = aoomrade AND 
      ekoforst.EVERDATUM = typdatum AND
      ekoforst.EPROJEKT = tidertemp.AONR AND
      ekoforst.DELNR = tidertemp.DELNR AND
      ekoforst.ELONTILLAGG = berkodvar AND
      ekoforst.FELDEBKRED = tidertemp.DEBET
      USE-INDEX PERSORG EXCLUSIVE-LOCK NO-ERROR.  
      IF NOT AVAILABLE ekoforst THEN DO:
         CREATE ekoforst.
      END.
   END.   
   ASSIGN 
   ekoforst.FTG = sattfore
   ekoforst.ENY = FALSE
   ekoforst.ERESULTENH = typover
   ekoforst.EPERSONALKOD = tidertemp.PERSONALKOD  
   ekoforst.EPROJEKT = tidertemp.AONR 
   ekoforst.DELNR = tidertemp.DELNR 
   ekoforst.EORG = PERSONALTAB.OMRADE  
   ekoforst.FELDEBKRED = tidertemp.DEBET
   ekoforst.EGEO = aoomrade
   ekoforst.EVERDATUM = typdatum 
   ekoforst.ELONTILLAGG = berkodvar
   ekoforst.ELONTILLANTAL = ekoforst.ELONTILLANTAL + bbantal  
   ekoforst.ELONBELOPP = ekoforst.ELONBELOPP + berbelopp. 
END PROCEDURE.                       
 
