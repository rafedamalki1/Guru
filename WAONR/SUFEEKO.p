  /*SUFEEKO.P*/
/* BARA PERSONER MED BRAVO = TRUE 
   AUTO:
   TRAKTEMENTE "TRA" kostnad = antal * TRAKTATAB.ERSATTNING
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
&Scoped-define NEW NEW  
{LESAMMAN.I}  
DEFINE INPUT PARAMETER vkdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER korvar AS CHARACTER NO-UNDO.
RUN sammut_UI (INPUT 1).
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO. 
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.         

{REGVAR.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
{FORESTYR.I}
DEFINE VARIABLE ekrid AS RECID EXTENT 50 NO-UNDO.
DEFINE VARIABLE aoomrade LIKE AONRTAB.OMRADE NO-UNDO.          
DEFINE VARIABLE timtid AS DECIMAL FORMAT "99.99" NO-UNDO. 
DEFINE VARIABLE multi AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE berkostnad AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE totpristim AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE reskod LIKE TIDFEL.LONTILLAGG NO-UNDO. 
DEFINE VARIABLE resantal LIKE TIDFEL.LONTILLANTAL NO-UNDO.
DEFINE VARIABLE resbelopp LIKE EKRAPPRESULT.ELONBELOPP NO-UNDO. 
DEFINE VARIABLE ovkod LIKE TIDFEL.OVERTIDTILL NO-UNDO. 
DEFINE VARIABLE ovantal LIKE TIDFEL.OVERANTAL NO-UNDO.
DEFINE VARIABLE ovbelopp LIKE EKRAPPRESULT.EOVERBELOPP NO-UNDO. 
DEFINE VARIABLE trakod LIKE TIDFEL.TRAKTKOD NO-UNDO. 
DEFINE VARIABLE traantal LIKE TIDFEL.TRAKTANTAL NO-UNDO.
DEFINE VARIABLE trabelopp LIKE EKRAPPRESULT.ETRAKTBELOPP NO-UNDO. 
DEFINE VARIABLE lonkod LIKE TIDFEL.LONTILLAGG NO-UNDO. 
DEFINE VARIABLE lonantal LIKE TIDFEL.LONTILLANTAL NO-UNDO.
DEFINE VARIABLE lonbelopp LIKE EKRAPPRESULT.ELONBELOPP NO-UNDO. 
DEFINE VARIABLE berkodvar LIKE TIDFEL.BEREDSKAP NO-UNDO. 
DEFINE VARIABLE bbantal LIKE TIDFEL.BERANTAL NO-UNDO.
DEFINE VARIABLE berbelopp LIKE EKRAPPRESULT.EBERBELOPP NO-UNDO.
DEFINE VARIABLE pkod LIKE PERSONALTAB.PERSONALKOD NO-UNDO.
DEFINE VARIABLE typover LIKE EKRAPPRESULT.ERESULTENH NO-UNDO. 
DEFINE VARIABLE typdatum AS CHARACTER FORMAT "999999" NO-UNDO. 
DEFINE VARIABLE kodanst LIKE ANSTFORMTAB.KOD NO-UNDO.
DEFINE NEW SHARED TEMP-TABLE foretemp NO-UNDO
   FIELD FTG AS CHARACTER
   INDEX FTG IS PRIMARY FTG.

DEFINE VARIABLE sattfore AS CHARACTER NO-UNDO.
{SOKDEF.I}
DEFINE QUERY persq FOR PERSONALTAB.  
DEFINE QUERY tidq FOR TIDFEL.
DEFINE TEMP-TABLE ekoforst
   FIELD FTG AS CHARACTER
   FIELD ENY LIKE EKRAPPRESULT.ENY 
   FIELD EPERSONALKOD LIKE EKRAPPRESULT.EPERSONALKOD 
   FIELD EPROJEKT LIKE EKRAPPRESULT.EPROJEKT
   FIELD DELNR LIKE AONRTAB.DELNR 
   FIELD EORG LIKE EKRAPPRESULT.EORG     
   FIELD EGEO LIKE EKRAPPRESULT.EGEO 
   FIELD FELDEBKRED AS LOGICAL
   FIELD EVERDATUM LIKE EKRAPPRESULT.EVERDATUM
   FIELD EOVERJA LIKE EKRAPPRESULT.EOVERJA
   FIELD ERESULTENH LIKE EKRAPPRESULT.ERESULTENH 
   FIELD EBELOPP LIKE  EKRAPPRESULT.EBELOPP 
   FIELD EANTAL LIKE EKRAPPRESULT.EANTAL 
   FIELD ETIMMAR LIKE EKRAPPRESULT.EANTAL                
   FIELD ELONTILLAGG LIKE EKRAPPRESULT.ELONTILLAGG 
   FIELD ELONTILLANTAL LIKE EKRAPPRESULT.ELONTILLANTAL    
   FIELD ELONBELOPP LIKE EKRAPPRESULT.ELONBELOPP         
   INDEX PERSORG IS PRIMARY EPERSONALKOD EORG EGEO EPROJEKT DELNR ASCENDING.
pkod = "".     
OPEN QUERY persq 
FOR EACH PERSONALTAB WHERE PERSONALTAB.BRAVO = TRUE USE-INDEX PERSONALKOD NO-LOCK.
GET FIRST persq NO-LOCK.
DO WHILE AVAILABLE(PERSONALTAB):
   aoomrade = "".
   persrec = RECID(PERSONALTAB).   
   IF pkod NE PERSONALTAB.PERSONALKOD THEN DO:
      pkod = PERSONALTAB.PERSONALKOD.        
      persrec = RECID(PERSONALTAB).
      FIND FIRST ANSTFORMTAB WHERE
      ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
      USE-INDEX ANSTF NO-LOCK NO-ERROR.
      kodanst = ANSTFORMTAB.KOD.
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
      FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
      FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
      IF AVAILABLE JURPERS THEN DO:
         sattfore = JURPERS.JUDID.
      END.   
      ELSE DO:
         IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
            IF  OMRADETAB.OMRADE BEGINS "1" OR OMRADETAB.OMRADE BEGINS "5" THEN  sattfore = "elnat".
            IF  OMRADETAB.OMRADE BEGINS "3" OR OMRADETAB.OMRADE BEGINS "7" THEN  sattfore = "servanet".
            ELSE sattfore = "elnat".
         END.   
         IF Guru.Konstanter:globforetag = "SUND" THEN sattfore = "SEAB".
      END.   
      /*REKO ska inte över till ekonomi*/
      IF sattfore = "REKO" THEN.
      ELSE DO:      
         IF sattfore = "ELNÄT" THEN DO: 
            sattfore = "elnat".
         END.
         FIND FIRST foretemp WHERE foretemp.FTG = sattfore  NO-LOCK NO-ERROR.
         IF NOT AVAILABLE foretemp THEN DO:
            CREATE foretemp.
            foretemp.FTG = sattfore.         
         END.
      END.   
   END.
   IF sattfore = "REKO" THEN.
   ELSE DO:  
      IF korvar = "" THEN DO: 
         OPEN QUERY tidq FOR EACH TIDFEL WHERE
         TIDFEL.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDFEL.SKICKA = TRUE AND TIDFEL.FELKORD = "" AND
         TIDFEL.DATUM <= vkdatum NO-LOCK. 
      END.
      ELSE DO:
         OPEN QUERY tidq FOR EACH TIDFEL WHERE
         TIDFEL.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDFEL.SKICKA = FALSE AND TIDFEL.FELKORD = korvar AND
         TIDFEL.DATUM <= vkdatum NO-LOCK. 
      END.
      GET FIRST tidq NO-LOCK.
      DO WHILE AVAILABLE(TIDFEL):
         IF TIDFEL.PRISTYP = "EJ.KOSTN." THEN sattfore = sattfore.
         ELSE DO:      
            totpristim = TIDFEL.PRIS.
            FIND FIRST AONRTAB WHERE AONRTAB.AONR = TIDFEL.AONR AND
            AONRTAB.DELNR = TIDFEL.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
            IF NOT AVAILABLE AONRTAB THEN DO:
               aoomrade = PERSONALTAB.OMRADE. 
               IF TIDFEL.AONR = "" THEN aoomrade = PERSONALTAB.OMRADE. 
               ELSE DO:
                  MESSAGE "AONR" TIDFEL.AONR TIDFEL.DELNR "FINNS EJ! KONTAKTA ELPOOL"
                  VIEW-AS ALERT-BOX.
               END.
            END.    
            ELSE DO:
               aoomrade = AONRTAB.OMRADE.   
               IF AONRTAB.OMRADE = "" THEN aoomrade = PERSONALTAB.OMRADE.   
            END.                              
            typdatum = STRING(DATE(MONTH(TIDFEL.DATUM),01,YEAR(TIDFEL.DATUM)),"999999"). 
            FIND FIRST ekoforst WHERE
            ekoforst.FTG = sattfore AND
            ekoforst.ENY = FALSE AND
            ekoforst.EPERSONALKOD = TIDFEL.PERSONALKOD AND 
            ekoforst.EORG = PERSONALTAB.OMRADE AND 
            ekoforst.EGEO = aoomrade AND
            ekoforst.EPROJEKT = TIDFEL.AONR AND         
            ekoforst.DELNR = TIDFEL.DELNR AND
            ekoforst.EVERDATUM = typdatum AND
            ekoforst.FELDEBKRED = TIDFEL.DEBET
            USE-INDEX PERSORG NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ekoforst THEN DO TRANSACTION:
               CREATE ekoforst.
               ASSIGN 
               ekoforst.FTG = sattfore
               ekoforst.ENY = FALSE
               ekoforst.EPERSONALKOD = TIDFEL.PERSONALKOD
               ekoforst.EPROJEKT = TIDFEL.AONR 
               ekoforst.DELNR = TIDFEL.DELNR 
               ekoforst.EORG = PERSONALTAB.OMRADE  
               ekoforst.FELDEBKRED = TIDFEL.DEBET
               ekoforst.EGEO = aoomrade                         
               ekoforst.EVERDATUM = typdatum.
            END.                                             
            ekrid[1] = RECID(ekoforst).
            /*RESTID*/                         
            IF TIDFEL.PRISTYP = 'RESTID...' AND TIDFEL.LONTILLAGG NE " " THEN DO TRANSACTION: 
               FIND ekoforst WHERE RECID(ekoforst) = ekrid[1]
               EXCLUSIVE-LOCK NO-ERROR.     
               ASSIGN
               resbelopp = 0                    
               reskod = ""
               resantal = 0
               reskod = TIDFEL.LONTILLAGG
               nytid = TIDFEL.LONTILLANTAL.
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
               IF TIDFEL.TRAKTKOD NE " " THEN DO:
                  ASSIGN
                  trakod = TIDFEL.TRAKTKOD
                  traantal = TIDFEL.TRAKTANTAL.
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
                  IF TIDFEL.OKOD1 NE " " THEN ASSIGN ekoforst.EOVERJA = TRUE.
                  ELSE IF TIDFEL.OKOD2 NE " " THEN ASSIGN ekoforst.EOVERJA = TRUE.
                  ELSE IF TIDFEL.OKOD3 NE " " THEN ASSIGN ekoforst.EOVERJA = TRUE.
                  IF ekoforst.EOVERJA = FALSE AND TIDFEL.TOTALT > 0 THEN DO:                  
                     nytid = TIDFEL.TOTALT.
                     RUN TIMSEK.P.
                     timtid = (sekunder / 3600).
                     ASSIGN ekoforst.EBELOPP = ekoforst.EBELOPP +
                     (TIDFEL.PRIS * timtid)              
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
                  IF TIDFEL.OKOD1 NE " " THEN DO: 
                     ASSIGN
                     ovkod = TIDFEL.OKOD1
                     nytid = TIDFEL.OANT1.
                     RUN TIMSEK.P.
                     ovantal = (sekunder / 3600). 
                     RUN over_UI.        
                     nytid = TIDFEL.TOTALT.
                     RUN TIMSEK.P.                  
                     ASSIGN ekoforst.EANTAL = ekoforst.EANTAL + (sekunder / 3600).                              
                  END.          
                  FIND ekoforst WHERE RECID(ekoforst) = ekrid[1]
                  EXCLUSIVE-LOCK NO-ERROR.   
                  ASSIGN
                  ovbelopp = 0                    
                  ovkod = ""
                  ovantal = 0.
                  IF TIDFEL.OKOD2 NE " " THEN DO: 
                     ASSIGN
                     ovkod = TIDFEL.OKOD2
                     nytid = TIDFEL.OANT2.
                     RUN TIMSEK.P.
                     ovantal = (sekunder / 3600).        
                     RUN over_UI.                  
                  END.                 
                  FIND ekoforst WHERE RECID(ekoforst) = ekrid[1]
                  EXCLUSIVE-LOCK NO-ERROR. 
                  ASSIGN
                  ovbelopp = 0
                  ovkod = ""
                  ovantal = 0.
                  IF TIDFEL.OKOD3 NE " " THEN DO: 
                     ASSIGN
                     ovkod = TIDFEL.OKOD3
                     nytid = TIDFEL.OANT3.
                     RUN TIMSEK.P.
                     ovantal = (sekunder / 3600).
                     RUN over_UI.        
                  END.           
               END.
               DO TRANSACTION:              
                  /*TRAKTAMENTE*/ 
                  FIND ekoforst WHERE RECID(ekoforst) = ekrid[1]
                  EXCLUSIVE-LOCK NO-ERROR. 
                  ASSIGN
                  trabelopp = 0
                  trakod = ""
                  traantal = 0.
                  IF TIDFEL.TRAKTKOD NE " " THEN DO:
                     ASSIGN
                     trakod = TIDFEL.TRAKTKOD
                     traantal = TIDFEL.TRAKTANTAL.
                     RUN tra_UI.                    
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
                  IF TIDFEL.LONTILLAGG NE " " THEN DO:
                     ASSIGN
                     lonkod = TIDFEL.LONTILLAGG
                     lonantal = TIDFEL.LONTILLANTAL.
                     RUN lon_UI. 
                     /*santal*/
                     IF ekoforst.ERESULTENH = "OVE" THEN DO:
                        ekoforst.EANTAL = ekoforst.EANTAL + lonantal.
                     END.                    
                  END.        
               END.
               DO TRANSACTION:   
                  /*BEREDSKAP*/
                  FIND ekoforst WHERE RECID(ekoforst) = ekrid[1]
                  EXCLUSIVE-LOCK NO-ERROR.         
                  ASSIGN
                  berbelopp = 0
                  berkodvar = ""
                  bbantal = 0.
                  IF TIDFEL.BEREDSKAP NE " " THEN DO: 
                     ASSIGN
                     berkodvar = TIDFEL.BEREDSKAP
                     nytid = TIDFEL.BERANTAL.
                     RUN TIMSEK.P.
                     bbantal = (sekunder / 3600).
                     RUN bered_UI.                    
                  END.          
               END.
            END.
         END.
         GET NEXT tidq NO-LOCK.
      END.       
   END.  /*ELSE DO*/
   GET NEXT persq NO-LOCK.
END.   /*FOR EACH*/     

RUN SUFEEKON2.P (INPUT samvar,INPUT vkdatum,INPUT TABLE ekoforst).
RUN sammut_UI (INPUT 2).
PROCEDURE res_UI:
   RETURN. 
   /*FIND FIRST LONTILL WHERE LONTILL.KOD = kodanst AND
   LONTILL.LONTILLAGG = reskod 
   USE-INDEX LON NO-LOCK NO-ERROR.
   IF NOT AVAILABLE LONTILL THEN RETURN.
   ASSIGN
   typover = SUBSTRING(LONTILL.TYPKOD,1,3)     
   multi =  LONTILL.MULTIP.                
   IF typover = "EJE" THEN RETURN.    
   IF typover = "REL" THEN resbelopp = (TIDFEL.PRIS * multi) * resantal. 
   ELSE resbelopp = (TIDFEL.PRIS + (TIDFEL.PRIS * multi)) * resantal. 
   IF ekoforst.ELONTILLAGG = reskod OR ekoforst.ELONTILLAGG = "" THEN DO:
      persrec = persrec.
   END.                                   
   ELSE DO:
      FIND FIRST ekoforst WHERE
      ekoforst.FTG = sattfore AND
      ekoforst.ERESULTENH = typover AND
      ekoforst.ENY = FALSE AND
      ekoforst.EPERSONALKOD = TIDFEL.PERSONALKOD AND   
      ekoforst.EORG = PERSONALTAB.OMRADE AND 
      ekoforst.EGEO = aoomrade AND              
      ekoforst.EVERDATUM = typdatum AND
      ekoforst.EPROJEKT = TIDFEL.AONR AND
      ekoforst.DELNR = TIDFEL.DELNR AND
      ekoforst.ELONTILLAGG = reskod AND
      ekoforst.FELDEBKRED = TIDFEL.DEBET
      USE-INDEX PERSORG EXCLUSIVE-LOCK NO-ERROR.  
      IF NOT AVAILABLE ekoforst THEN DO:
         CREATE ekoforst.
      END.
   END.   
   ASSIGN 
   ekoforst.FTG = sattfore 
   ekoforst.ENY = FALSE
   ekoforst.ERESULTENH = typover
   ekoforst.EPERSONALKOD = TIDFEL.PERSONALKOD
   ekoforst.EPROJEKT = TIDFEL.AONR 
   ekoforst.DELNR = TIDFEL.DELNR 
   ekoforst.EORG = PERSONALTAB.OMRADE  
   ekoforst.FELDEBKRED = TIDFEL.DEBET  
   ekoforst.EGEO = aoomrade            
   ekoforst.EVERDATUM = typdatum 
   ekoforst.ELONTILLAGG = reskod
   ekoforst.ELONTILLANTAL = ekoforst.ELONTILLANTAL + resantal  
   ekoforst.ELONBELOPP = ekoforst.ELONBELOPP + resbelopp.*/ 
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
   IF typover = "EJE" THEN RETURN.    
   IF typover = "BIL" THEN typover = typover.
   ELSE IF typover = "OVE" THEN typover = typover.
   ELSE DO:
      RETURN.
   END.
   IF typover = "RES" THEN DO:
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 1
      soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
      soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
      soktemp.SOKCHAR[3] = 'RESTID...'
      soktemp.SOKCHAR[4] = TIDFEL.OVERTIDTILL
      soktemp.SOKDATE[1] = TIDFEL.DATUM.
      {SOKANROP.I}
      nytid = lonantal.
      RUN TIMSEK.P.
      lonantal = (sekunder / 3600).                 
      lonbelopp = (soktemp.SOKDECI[1] + (soktemp.SOKDECI[1] * multi)) * lonantal.        
   END. 
   ELSE IF typover = "REL" THEN DO:     
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 1
      soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
      soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
      soktemp.SOKCHAR[3] = 'RESTID...'
      soktemp.SOKCHAR[4] = TIDFEL.OVERTIDTILL
      soktemp.SOKDATE[1] = TIDFEL.DATUM.
      {SOKANROP.I}
      nytid = lonantal.
      RUN TIMSEK.P.
      lonantal = (sekunder / 3600).                 
      lonbelopp = (soktemp.SOKDECI[1] * multi) * lonantal.        
   END. 
   ELSE IF typover = "RE2" THEN DO:       
      ASSIGN
      soktemp.SOKVAL = 1
      soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
      soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
      soktemp.SOKCHAR[3] = 'TOT.PRIS.'
      soktemp.SOKCHAR[4] = TIDFEL.OVERTIDTILL
      soktemp.SOKDATE[1] = TIDFEL.DATUM.
      {SOKANROP.I}
      nytid = lonantal.
      RUN TIMSEK.P.
      lonantal = (sekunder / 3600).                 
      lonbelopp = (soktemp.SOKDECI[1] + (soktemp.SOKDECI[1] * multi)) * lonantal.        
   END.
   ELSE IF typover = "OVE" THEN DO:    
      nytid = lonantal.
      RUN TIMSEK.P.
      lonantal = (sekunder / 3600).                                                    
      lonbelopp = (totpristim + (totpristim * multi)) * lonantal. 
   END. 
   ELSE IF typover = "OV2" THEN DO:    
      nytid = lonantal.
      RUN TIMSEK.P.
      lonantal = (sekunder / 3600).                                                    
      lonbelopp = (totpristim * multi) * lonantal. 
   END.
   ELSE DO:
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
         IF lonkod = "8379" OR 
            lonkod = "8388" OR 
            lonkod = "7211" OR 
            lonkod = "7212" THEN DO:
               lonbelopp = lonbelopp * -1.
         END.                                                  
      END.
      ELSE RETURN.    
   END.
   /*santal*/
   IF ekoforst.ELONTILLAGG = lonkod /*OR ekoforst.ELONTILLAGG = ""*/ THEN DO:
      persrec = persrec.
   END.                                   
   ELSE DO:
      FIND FIRST ekoforst WHERE
      ekoforst.FTG = sattfore AND
      ekoforst.ERESULTENH = typover AND
      ekoforst.ENY = FALSE AND
      ekoforst.EPERSONALKOD = TIDFEL.PERSONALKOD AND 
      ekoforst.EORG = PERSONALTAB.OMRADE AND 
      ekoforst.EGEO = aoomrade AND
      ekoforst.EVERDATUM = typdatum AND
      ekoforst.EPROJEKT = TIDFEL.AONR AND
      ekoforst.DELNR = TIDFEL.DELNR AND
      ekoforst.ELONTILLAGG = lonkod AND
      ekoforst.FELDEBKRED = TIDFEL.DEBET
      USE-INDEX PERSORG EXCLUSIVE-LOCK NO-ERROR.  
      IF NOT AVAILABLE ekoforst THEN DO:
         CREATE ekoforst.
      END.
   END.   
   ASSIGN 
   ekoforst.FTG = sattfore 
   ekoforst.ENY = FALSE
   ekoforst.ERESULTENH = typover
   ekoforst.EPERSONALKOD = TIDFEL.PERSONALKOD 
   ekoforst.EPROJEKT = TIDFEL.AONR 
   ekoforst.DELNR = TIDFEL.DELNR 
   ekoforst.EORG = PERSONALTAB.OMRADE  
   ekoforst.FELDEBKRED = TIDFEL.DEBET
   ekoforst.EGEO = aoomrade 
   ekoforst.ELONTILLAGG = lonkod       
   ekoforst.EVERDATUM = typdatum
   ekoforst.ELONTILLANTAL = ekoforst.ELONTILLANTAL + lonantal  
   ekoforst.ELONBELOPP = ekoforst.ELONBELOPP + lonbelopp. 
END PROCEDURE.                
PROCEDURE tra_UI:
   RETURN.
/*   musz = FALSE.
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
      ekoforst.EPERSONALKOD = TIDFEL.PERSONALKOD AND
      ekoforst.EORG = PERSONALTAB.OMRADE AND 
      ekoforst.EGEO = aoomrade AND 
      ekoforst.EVERDATUM = typdatum AND
      ekoforst.EPROJEKT = TIDFEL.AONR AND
      ekoforst.DELNR = TIDFEL.DELNR AND
      ekoforst.ELONTILLAGG = trakod AND 
      ekoforst.FELDEBKRED = TIDFEL.DEBET
      USE-INDEX PERSORG EXCLUSIVE-LOCK NO-ERROR.  
      IF NOT AVAILABLE ekoforst THEN DO:
         CREATE ekoforst.
      END.
   END.   
   ASSIGN 
   ekoforst.FTG = sattfore 
   ekoforst.ENY = FALSE
   ekoforst.ERESULTENH = typover
   ekoforst.EPERSONALKOD = TIDFEL.PERSONALKOD
   ekoforst.EPROJEKT = TIDFEL.AONR 
   ekoforst.DELNR = TIDFEL.DELNR 
   ekoforst.EORG = PERSONALTAB.OMRADE  
   ekoforst.FELDEBKRED = TIDFEL.DEBET
   ekoforst.EGEO = aoomrade            
   ekoforst.EVERDATUM = typdatum
   ekoforst.ELONTILLAGG = trakod
   ekoforst.ELONTILLANTAL = ekoforst.ELONTILLANTAL + traantal  
   ekoforst.ELONBELOPP = ekoforst.ELONBELOPP + trabelopp.*/ 
END PROCEDURE.                      
PROCEDURE over_UI:
   multi = 0.
   FIND FIRST OVERKOD WHERE OVERKOD.KOD = kodanst AND
   OVERKOD.OVERTIDTILL = ovkod 
   USE-INDEX OVER NO-LOCK NO-ERROR.
   IF NOT AVAILABLE OVERKOD THEN RETURN.  
   ASSIGN
   multi = OVERKOD.MULTIP
   typover = "OVE".
   IF TIDFEL.PRISTYP = "RESTID..." THEN typover = "RES".  
   ovbelopp = (TIDFEL.PRIS + (TIDFEL.PRIS * multi)) * ovantal.
   IF TIDFEL.PRIS = 0 AND TIDFEL.TIDLOG = FALSE THEN DO:
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 1
      soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
      soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
      soktemp.SOKCHAR[3] = 'RESTID...'
      soktemp.SOKCHAR[4] = TIDFEL.OVERTIDTILL
      soktemp.SOKDATE[1] = TIDFEL.DATUM.
      {SOKANROP.I}      
      ovbelopp = (soktemp.SOKDECI[1] + (soktemp.SOKDECI[1] * multi)) * ovantal.
   END.   
   /*santal*/  
   IF ekoforst.ELONTILLAGG = ovkod /*OR ekoforst.ELONTILLAGG = ""*/ THEN DO:   
      persrec = persrec.
   END.                                   
   ELSE DO:
      FIND FIRST ekoforst WHERE
      ekoforst.FTG = sattfore AND
      ekoforst.ERESULTENH = typover AND
      ekoforst.ENY = FALSE AND
      ekoforst.EPERSONALKOD = TIDFEL.PERSONALKOD AND
      ekoforst.EORG = PERSONALTAB.OMRADE AND 
      ekoforst.EGEO = aoomrade AND 
      ekoforst.EVERDATUM = typdatum AND
      ekoforst.EPROJEKT = TIDFEL.AONR AND
      ekoforst.DELNR = TIDFEL.DELNR AND
      ekoforst.ELONTILLAG = ovkod AND
      ekoforst.FELDEBKRED = TIDFEL.DEBET
      USE-INDEX PERSORG EXCLUSIVE-LOCK NO-ERROR.  
      IF NOT AVAILABLE ekoforst THEN DO:
         CREATE ekoforst.
      END.
   END.   
   ASSIGN 
   ekoforst.FTG = sattfore 
   ekoforst.ENY = FALSE
   ekoforst.ERESULTENH = typover
   ekoforst.EPERSONALKOD = TIDFEL.PERSONALKOD 
   ekoforst.EPROJEKT = TIDFEL.AONR 
   ekoforst.DELNR = TIDFEL.DELNR 
   ekoforst.EORG = PERSONALTAB.OMRADE  
   ekoforst.FELDEBKRED = TIDFEL.DEBET
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
      ekoforst.EPERSONALKOD = TIDFEL.PERSONALKOD AND
      ekoforst.EORG = PERSONALTAB.OMRADE AND 
      ekoforst.EGEO = aoomrade AND 
      ekoforst.EVERDATUM = typdatum AND
      ekoforst.EPROJEKT = TIDFEL.AONR AND
      ekoforst.DELNR = TIDFEL.DELNR AND
      ekoforst.ELONTILLAGG = berkodvar AND
      ekoforst.FELDEBKRED = TIDFEL.DEBET   
      USE-INDEX PERSORG EXCLUSIVE-LOCK NO-ERROR.  
      IF NOT AVAILABLE ekoforst THEN DO:
         CREATE ekoforst.
      END.
   END.   
   ASSIGN 
   ekoforst.FTG = sattfore 
   ekoforst.ENY = FALSE
   ekoforst.ERESULTENH = typover
   ekoforst.EPERSONALKOD = TIDFEL.PERSONALKOD  
   ekoforst.EPROJEKT = TIDFEL.AONR 
   ekoforst.DELNR = TIDFEL.DELNR 
   ekoforst.EORG = PERSONALTAB.OMRADE  
   ekoforst.FELDEBKRED = TIDFEL.DEBET
   ekoforst.EGEO = aoomrade
   ekoforst.EVERDATUM = typdatum 
   ekoforst.ELONTILLAGG = berkodvar
   ekoforst.ELONTILLANTAL = ekoforst.ELONTILLANTAL + bbantal  
   ekoforst.ELONBELOPP = ekoforst.ELONBELOPP + berbelopp. 
END PROCEDURE.                       
 
