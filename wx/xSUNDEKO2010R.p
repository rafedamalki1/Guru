  /*G:\PRO9S\WX\XSUNDEKO2010R.P*/
/* BARA PERSONER MED BRAVO = TRUE 
   AUTO:
   TRAKTEMENTE "TRA" kostnad = antal * TRAKTATAB.ERSATTNING
   ÖVERTID "OVE"     kostnad = (TIDREGITAB.PRIS + (TIDREGITAB.PRIS * multi)) * antal.
   RESTID "RES"      kostnad = (TIDREGITAB.PRIS + (TIDREGITAB.PRIS* multi)) * antal.        
   RESTID "REL"      kostnad = (TIDREGITAB.PRIS * multi) * resantal. 
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
{LESAMMAN.I}  

   /*DEFINE INPUT PARAMETER vkdatum AS DATE NO-UNDO.*/

DEFINE INPUT PARAMETER vkdatum AS DATE NO-UNDO.
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
DEFINE VARIABLE totpristim AS DECIMAL FORMAT "->>99.99" NO-UNDO.
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
DEFINE VARIABLE typdatum AS CHARACTER FORMAT "999999" NO-UNDO. 
DEFINE VARIABLE kodanst LIKE ANSTFORMTAB.KOD NO-UNDO.
DEFINE VARIABLE sattfore AS CHARACTER NO-UNDO.
DEFINE VARIABLE diffpris AS DECIMAL FORMAT "->>99.99" NO-UNDO.
DEFINE QUERY persq FOR PERSONALTAB.  
DEFINE QUERY tidq FOR TIDREGITAB.
DEFINE NEW SHARED TEMP-TABLE foretemp NO-UNDO
   FIELD FTG AS CHARACTER
   INDEX FTG IS PRIMARY FTG.
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
FIND FIRST FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
{FORESTYR.I}
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
      sattfore = JURPERS.JUDID.
      IF sattfore = "ELNÄT" THEN DO: 
         sattfore = "elnat".
      END.
      FIND FIRST foretemp WHERE foretemp.FTG = sattfore  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE foretemp THEN DO:
         CREATE foretemp.
         foretemp.FTG = sattfore.         
      END.
   END.  
   
   OPEN QUERY tidq FOR EACH TIDREGITAB WHERE
   TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.GODKAND BEGINS "G" AND TIDREGITAB.VECKOKORD BEGINS korvar AND
   TIDREGITAB.PRISTYP NE "FRÅNVARO." AND TIDREGITAB.DATUM <= vkdatum AND TIDREGITAB.DATUM GE 01/01/2010 USE-INDEX PSTART NO-LOCK. 
   GET FIRST tidq NO-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB):
      diffpris = 0.
      IF TIDREGITAB.PRISTYP = "EJ.KOSTN." THEN sattfore = sattfore.
      ELSE DO:       
         
         RUN rattpris_UI.
         IF diffpris NE 0 THEN DO:         
            totpristim = diffpris.
            /*totpristim = TIDREGITAB.PRIS.*/
            FIND FIRST AONRTAB WHERE AONRTAB.AONR = TIDREGITAB.AONR AND
            AONRTAB.DELNR = TIDREGITAB.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
            IF NOT AVAILABLE AONRTAB THEN DO:
               aoomrade = PERSONALTAB.OMRADE. 
               IF TIDREGITAB.AONR = "" THEN aoomrade = PERSONALTAB.OMRADE. 
               ELSE DO:
                  MESSAGE "AONR" TIDREGITAB.AONR TIDREGITAB.DELNR "FINNS EJ! KONTAKTA ELPOOL"
                  VIEW-AS ALERT-BOX.
               END.
            END.    
            ELSE DO:
               aoomrade = AONRTAB.OMRADE.   
               IF AONRTAB.OMRADE = "" THEN aoomrade = PERSONALTAB.OMRADE.   
            END.                              
            typdatum = STRING(DATE(MONTH(TIDREGITAB.DATUM),01,YEAR(TIDREGITAB.DATUM)),"999999"). 
            FIND FIRST ekoforst WHERE
            ekoforst.FTG = sattfore AND
            ekoforst.ENY = FALSE AND
            ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD AND 
            ekoforst.EORG = PERSONALTAB.OMRADE AND 
            ekoforst.EGEO = aoomrade AND
            ekoforst.EPROJEKT = TIDREGITAB.AONR AND         
            ekoforst.DELNR = TIDREGITAB.DELNR AND
            ekoforst.EVERDATUM = typdatum      
            USE-INDEX PERSORG NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ekoforst THEN DO TRANSACTION:
               CREATE ekoforst.
               ASSIGN 
               ekoforst.FTG = sattfore 
               ekoforst.ENY = FALSE
               ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD
               ekoforst.EPROJEKT = TIDREGITAB.AONR 
               ekoforst.DELNR = TIDREGITAB.DELNR 
               ekoforst.EORG = PERSONALTAB.OMRADE  
               ekoforst.EGEO = aoomrade                         
               ekoforst.EVERDATUM = typdatum.
            END.                                             
            ekrid[1] = RECID(ekoforst).
            /*RESTID*/                         
            IF TIDREGITAB.PRISTYP = 'RESTID...' AND TIDREGITAB.LONTILLAGG NE " " THEN DO TRANSACTION: 
               FIND ekoforst WHERE RECID(ekoforst) = ekrid[1]
               EXCLUSIVE-LOCK NO-ERROR.     
               ASSIGN
               resbelopp = 0                    
               reskod = ""
               resantal = 0
               reskod = TIDREGITAB.LONTILLAGG
               nytid = TIDREGITAB.LONTILLANTAL.
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
               IF TIDREGITAB.TRAKTKOD NE " " THEN DO:
                  ASSIGN
                  trakod = TIDREGITAB.TRAKTKOD
                  traantal = TIDREGITAB.TRAKTANTAL.
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
                  IF TIDREGITAB.OKOD1 NE " " THEN ASSIGN ekoforst.EOVERJA = TRUE.
                  ELSE IF TIDREGITAB.OKOD2 NE " " THEN ASSIGN ekoforst.EOVERJA = TRUE.
                  ELSE IF TIDREGITAB.OKOD3 NE " " THEN ASSIGN ekoforst.EOVERJA = TRUE.
                  IF ekoforst.EOVERJA = FALSE AND TIDREGITAB.TOTALT > 0 THEN DO:                  
                     nytid = TIDREGITAB.TOTALT.
                     RUN TIMSEK.P.
                     timtid = (sekunder / 3600).
                     ASSIGN ekoforst.EBELOPP = ekoforst.EBELOPP +
                     (diffpris * timtid)    
                     /*(TIDREGITAB.PRIS * timtid)              */
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
                  IF TIDREGITAB.OKOD1 NE " " THEN DO: 
                     ASSIGN
                     ovkod = TIDREGITAB.OKOD1
                     nytid = TIDREGITAB.OANT1.
                     RUN TIMSEK.P.
                     ovantal = (sekunder / 3600). 
                     RUN over_UI.        
                     nytid = TIDREGITAB.TOTALT.
                     RUN TIMSEK.P.                  
                     ASSIGN ekoforst.EANTAL = ekoforst.EANTAL + (sekunder / 3600).                              
                  END.          
                  FIND ekoforst WHERE RECID(ekoforst) = ekrid[1]
                  EXCLUSIVE-LOCK NO-ERROR.   
                  ASSIGN
                  ovbelopp = 0                    
                  ovkod = ""
                  ovantal = 0.
                  IF TIDREGITAB.OKOD2 NE " " THEN DO: 
                     ASSIGN
                     ovkod = TIDREGITAB.OKOD2
                     nytid = TIDREGITAB.OANT2.
                     RUN TIMSEK.P.
                     ovantal = (sekunder / 3600).        
                     RUN over_UI.   
                     /*
                     nytid = TIDREGITAB.TOTALT.
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
                  IF TIDREGITAB.OKOD3 NE " " THEN DO: 
                     ASSIGN
                     ovkod = TIDREGITAB.OKOD3
                     nytid = TIDREGITAB.OANT3.
                     RUN TIMSEK.P.
                     ovantal = (sekunder / 3600).
                     RUN over_UI.        
                     /*
                     nytid = TIDREGITAB.TOTALT.
                     RUN TIMSEK.P.                  
                     ASSIGN ekoforst.EANTAL = ekoforst.EANTAL + (sekunder / 3600).                      
                     */
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
                  IF TIDREGITAB.TRAKTKOD NE " " THEN DO:
                     ASSIGN
                     trakod = TIDREGITAB.TRAKTKOD
                     traantal = TIDREGITAB.TRAKTANTAL.
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
                  IF TIDREGITAB.LONTILLAGG NE " " THEN DO:
                     ASSIGN
                     lonkod = TIDREGITAB.LONTILLAGG
                     lonantal = TIDREGITAB.LONTILLANTAL.
                     RUN lon_UI.                    
                  END.        
               END.
               /* BEREDSKAP BORTTAGET 2002-08-30 IER
               DO TRANSACTION:   
                  /*BEREDSKAP*/
                  FIND ekoforst WHERE RECID(ekoforst) = ekrid[1]
                  EXCLUSIVE-LOCK NO-ERROR.         
                  ASSIGN
                  berbelopp = 0
                  berkodvar = ""
                  bbantal = 0.
                  IF TIDREGITAB.BEREDSKAP NE " " THEN DO: 
                     ASSIGN
                     berkodvar = TIDREGITAB.BEREDSKAP
                     nytid = TIDREGITAB.BERANTAL.
                     RUN TIMSEK.P.
                     bbantal = (sekunder / 3600).
                     RUN bered_UI.                    
                  END.          
               END.*/
            END.
         END.
      END.
      GET NEXT tidq NO-LOCK.    
   END.  /*ELSE DO*/
   GET NEXT persq NO-LOCK.
END.   /*FOR EACH*/     
RUN sammut_UI (INPUT 2).
RUN XSUNDEKON2R.P (INPUT samvar,INPUT vkdatum,INPUT TABLE ekoforst).

PROCEDURE res_UI:
   IF globforetag = globforetag THEN RETURN. 
   FIND FIRST LONTILL WHERE LONTILL.KOD = kodanst AND
   LONTILL.LONTILLAGG = reskod 
   USE-INDEX LON NO-LOCK NO-ERROR.
   IF NOT AVAILABLE LONTILL THEN RETURN.
   ASSIGN
   typover = SUBSTRING(LONTILL.TYPKOD,1,3)     
   multi =  LONTILL.MULTIP.                
   IF typover = "EJE" THEN RETURN.    
   IF typover = "REL" THEN resbelopp = (diffpris * multi) * resantal. 
   ELSE resbelopp = (TIDREGITAB.PRIS + (diffpris * multi)) * resantal. 
   /*IF typover = "REL" THEN resbelopp = (TIDREGITAB.PRIS * multi) * resantal. 
   ELSE resbelopp = (TIDREGITAB.PRIS + (TIDREGITAB.PRIS * multi)) * resantal. */
   IF ekoforst.ELONTILLAGG = reskod OR ekoforst.ELONTILLAGG = "" THEN DO:
      persrec = persrec.
   END.                                   
   ELSE DO:
      FIND FIRST ekoforst WHERE
      ekoforst.FTG = sattfore AND
      ekoforst.ERESULTENH = typover AND
      ekoforst.ENY = FALSE AND
      ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD AND   
      ekoforst.EORG = PERSONALTAB.OMRADE AND 
      ekoforst.EGEO = aoomrade AND              
      ekoforst.EVERDATUM = typdatum AND
      ekoforst.EPROJEKT = TIDREGITAB.AONR AND
      ekoforst.DELNR = TIDREGITAB.DELNR AND
      ekoforst.ELONTILLAGG = reskod
      USE-INDEX PERSORG EXCLUSIVE-LOCK NO-ERROR.  
      IF NOT AVAILABLE ekoforst THEN DO:
         CREATE ekoforst.
      END.
   END.   
   ASSIGN 
   ekoforst.FTG = sattfore 
   ekoforst.ENY = FALSE
   ekoforst.ERESULTENH = typover
   ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD
   ekoforst.EPROJEKT = TIDREGITAB.AONR 
   ekoforst.DELNR = TIDREGITAB.DELNR 
   ekoforst.EORG = PERSONALTAB.OMRADE  
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
      soktemp.SOKINT[1] = varforetypval[4]
      soktemp.SOKCHAR[2] = TIDREGITAB.PERSONALKOD
      soktemp.SOKCHAR[3] = 'RESTID...'
      soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
      soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
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
      soktemp.SOKINT[1] = varforetypval[4]
      soktemp.SOKCHAR[2] = TIDREGITAB.PERSONALKOD
      soktemp.SOKCHAR[3] = 'RESTID...'
      soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
      soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
      {SOKANROP.I}
      nytid = lonantal.
      RUN TIMSEK.P.
      lonantal = (sekunder / 3600).                 
      lonbelopp = (soktemp.SOKDECI[1] * multi) * lonantal.        
   END. 
   ELSE IF typover = "RE2" THEN DO:       
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 1
      soktemp.SOKINT[1] = varforetypval[4]
      soktemp.SOKCHAR[2] = TIDREGITAB.PERSONALKOD
      soktemp.SOKCHAR[3] = 'TOT.PRIS.'
      soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
      soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
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
   IF ekoforst.ELONTILLAGG = lonkod OR ekoforst.ELONTILLAGG = "" THEN DO:
      persrec = persrec.
   END.                                   
   ELSE DO:
      FIND FIRST ekoforst WHERE
      ekoforst.FTG = sattfore AND
      ekoforst.ERESULTENH = typover AND
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
   ekoforst.ERESULTENH = typover
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
PROCEDURE tra_UI:
   IF globforetag = globforetag THEN RETURN.
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
      ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD AND
      ekoforst.EORG = PERSONALTAB.OMRADE AND 
      ekoforst.EGEO = aoomrade AND 
      ekoforst.EVERDATUM = typdatum AND
      ekoforst.EPROJEKT = TIDREGITAB.AONR AND
      ekoforst.DELNR = TIDREGITAB.DELNR AND
      ekoforst.ELONTILLAGG = trakod
      USE-INDEX PERSORG EXCLUSIVE-LOCK NO-ERROR.  
      IF NOT AVAILABLE ekoforst THEN DO:
         CREATE ekoforst.
      END.
   END.   
   ASSIGN 
   ekoforst.FTG = sattfore 
   ekoforst.ENY = FALSE
   ekoforst.ERESULTENH = typover
   ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD
   ekoforst.EPROJEKT = TIDREGITAB.AONR 
   ekoforst.DELNR = TIDREGITAB.DELNR 
   ekoforst.EORG = PERSONALTAB.OMRADE  
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
   ASSIGN
   multi = OVERKOD.MULTIP
   typover = "OVE".
   IF TIDREGITAB.PRISTYP = "RESTID..." THEN typover = "RES".  
   ovbelopp = (diffpris + (diffpris * multi)) * ovantal.
   /*ovbelopp = (TIDREGITAB.PRIS + (TIDREGITAB.PRIS * multi)) * ovantal.*/
   IF TIDREGITAB.PRIS = 0 AND TIDREGITAB.TIDLOG = FALSE THEN DO:
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 1
      soktemp.SOKINT[1] = varforetypval[4]
      soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
      soktemp.SOKCHAR[3] = 'TOT.PRIS.'
      soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
      soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
      {SOKANROP.I}
      ovbelopp = (soktemp.SOKDECI[1] + (soktemp.SOKDECI[1] * multi)) * ovantal.
   END.   
   IF ekoforst.ELONTILLAGG = ovkod OR ekoforst.ELONTILLAGG = "" THEN DO:
      persrec = persrec.
   END.                                   
   ELSE DO:
      FIND FIRST ekoforst WHERE
      ekoforst.FTG = sattfore AND
      ekoforst.ERESULTENH = typover AND
      ekoforst.ENY = FALSE AND
      ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD AND
      ekoforst.EORG = PERSONALTAB.OMRADE AND 
      ekoforst.EGEO = aoomrade AND 
      ekoforst.EVERDATUM = typdatum AND
      ekoforst.EPROJEKT = TIDREGITAB.AONR AND
      ekoforst.DELNR = TIDREGITAB.DELNR AND
      ekoforst.ELONTILLAG = ovkod
      USE-INDEX PERSORG EXCLUSIVE-LOCK NO-ERROR.  
      IF NOT AVAILABLE ekoforst THEN DO:
         CREATE ekoforst.
      END.
   END.   
   ASSIGN 
   ekoforst.FTG = sattfore 
   ekoforst.ENY = FALSE
   ekoforst.ERESULTENH = typover
   ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD 
   ekoforst.EPROJEKT = TIDREGITAB.AONR 
   ekoforst.DELNR = TIDREGITAB.DELNR 
   ekoforst.EORG = PERSONALTAB.OMRADE  
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
      ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD AND
      ekoforst.EORG = PERSONALTAB.OMRADE AND 
      ekoforst.EGEO = aoomrade AND 
      ekoforst.EVERDATUM = typdatum AND
      ekoforst.EPROJEKT = TIDREGITAB.AONR AND
      ekoforst.DELNR = TIDREGITAB.DELNR AND
      ekoforst.ELONTILLAGG = berkodvar
      USE-INDEX PERSORG EXCLUSIVE-LOCK NO-ERROR.  
      IF NOT AVAILABLE ekoforst THEN DO:
         CREATE ekoforst.
      END.
   END.   
   ASSIGN 
   ekoforst.FTG = sattfore 
   ekoforst.ENY = FALSE
   ekoforst.ERESULTENH = typover
   ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD  
   ekoforst.EPROJEKT = TIDREGITAB.AONR 
   ekoforst.DELNR = TIDREGITAB.DELNR 
   ekoforst.EORG = PERSONALTAB.OMRADE  
   ekoforst.EGEO = aoomrade
   ekoforst.EVERDATUM = typdatum 
   ekoforst.ELONTILLAGG = berkodvar
   ekoforst.ELONTILLANTAL = ekoforst.ELONTILLANTAL + bbantal  
   ekoforst.ELONBELOPP = ekoforst.ELONBELOPP + berbelopp. 
END PROCEDURE.           

PROCEDURE rattpris_UI:   
 IF TIDREGITAB.OVERTIDTILL = "SEAB Teknik" OR TIDREGITAB.OVERTIDTILL = "SEAB Planering" OR TIDREGITAB.OVERTIDTILL = "SEAB Nätutbyggnad" OR
 TIDREGITAB.OVERTIDTILL = "SEAB Nätdrift" OR TIDREGITAB.OVERTIDTILL = "SEAB Mät" OR TIDREGITAB.OVERTIDTILL = "SEAB Miljö" OR
 TIDREGITAB.OVERTIDTILL = "SEAB Mek" OR TIDREGITAB.OVERTIDTILL = "SEAB Marknad" OR TIDREGITAB.OVERTIDTILL = "SEAB Larm o Signal" OR
 TIDREGITAB.OVERTIDTILL = "SEAB Kundservice" OR TIDREGITAB.OVERTIDTILL = "Farligt avfall" OR TIDREGITAB.OVERTIDTILL = "SEAB El" OR
 TIDREGITAB.OVERTIDTILL = "SEAB Drift" OR TIDREGITAB.OVERTIDTILL = "SEAB Nätdokumentatio" OR TIDREGITAB.OVERTIDTILL = "SEAB Data" OR
 TIDREGITAB.OVERTIDTILL = "SEAB Proj/ Upphandl" OR TIDREGITAB.OVERTIDTILL = "SEAB Prod gemensamt" OR TIDREGITAB.OVERTIDTILL = "SEAB Instrument"    
   OR TIDREGITAB.OVERTIDTILL = "Avfallsbehandling" OR TIDREGITAB.OVERTIDTILL = "SEAB Arbetsmiljö"   THEN DO:
    FIND FIRST PERSONALPRIS WHERE 
    PERSONALPRIS.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
    PERSONALPRIS.BEFATTNING =  TIDREGITAB.OVERTIDTILL AND 
    PERSONALPRIS.STARTDATUM <= 01/01/2010  AND 
    PERSONALPRIS.SLUTDATUM >= 01/01/2010 NO-LOCK NO-ERROR.
    IF AVAILABLE PERSONALPRIS THEN DO:
       IF TIDREGITAB.PRISTYP = "TOT.PRIS." /*AND TIDREGITAB.PRIS > 0*/ THEN diffpris = PERSONALPRIS.PRIS - TIDREGITAB.PRIS .
       
    END.

 END.
 


END PROCEDURE.           
 
