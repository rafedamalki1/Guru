  /*ESINFAK.P*/
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE NEW SHARED VARIABLE bolag LIKE OMRADETAB.AVDELNINGNR NO-UNDO.
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.
/*DEFINE SHARED VARIABLE SEL_UPP AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE kollvecka LIKE VECKONATT.VECKOKORD NO-UNDO.
DEFINE SHARED VARIABLE valmanad AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE aonummer LIKE AONRTAB.AONR NO-UNDO.
DEFINE SHARED VARIABLE delnummer LIKE AONRTAB.DELNR NO-UNDO.
*/
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO. 
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE VARIABLE kolldat AS DATE NO-UNDO. 
DEFINE VARIABLE beravtvar LIKE PERSONALTAB.BEREDSKAPSAVTAL NO-UNDO.
DEFINE VARIABLE ekrid AS RECID EXTENT 50 NO-UNDO.
DEFINE VARIABLE aoomrade LIKE AONRTAB.OMRADE NO-UNDO.
DEFINE VARIABLE varfaktYP LIKE AONRKONTKOD.K3 NO-UNDO.          
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
DEFINE VARIABLE typdatum AS CHARACTER FORMAT "999999" NO-UNDO. 
DEFINE VARIABLE kodanst LIKE ANSTFORMTAB.KOD NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE ekoforst
   FIELD ENY LIKE EKRAPPRESULT.ENY 
   FIELD EPERSONALKOD LIKE EKRAPPRESULT.EPERSONALKOD 
   FIELD EPROJEKT LIKE EKRAPPRESULT.EPROJEKT
   FIELD DELNR LIKE AONRTAB.DELNR 
   FIELD K3 LIKE AONRKONTKOD.K3 
   FIELD EORG LIKE EKRAPPRESULT.EORG     
   FIELD EGEO LIKE EKRAPPRESULT.EGEO  
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
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
   
/*
IF MONTH(TODAY) = 01 THEN kolldat = DATE(12,31,YEAR(TODAY) - 1).
ELSE kolldat = DATE(MONTH(TODAY),01,YEAR(TODAY)) - 1.
*/
kolldat = 02/29/2000.
FIND FIRST INTERNFAKTKOLL WHERE INTERNFAKTKOLL.VECKOK = FALSE AND 
INTERNFAKTKOLL.VDATUM = kolldat NO-LOCK NO-ERROR.
IF NOT AVAILABLE INTERNFAKTKOLL THEN RETURN.
vkdatum = INTERNFAKTKOLL.VDATUM.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
IF FORETAG.FORETAG = "ESAN" THEN DO:
   bolag = 402.
   RUN infak_UI.
   RUN ESINFAK2.P.   
END.
ELSE IF FORETAG.FORETAG = "ESMA" THEN DO:
   bolag = 905.
   RUN infak_UI.
   RUN ESINFAK2.P.   
END.
OPEN QUERY intq FOR EACH INTERNFAKTKOLL WHERE INTERNFAKTKOLL.VECKOK = FALSE AND 
INTERNFAKTKOLL.VDATUM <= vkdatum NO-LOCK.
DO TRANSACTION:
   GET FIRST intq EXCLUSIVE-LOCK.
   INTERNFAKTKOLL.VECKOK = TRUE.
END.
REPEAT:
   DO TRANSACTION:
      GET NEXT intq EXCLUSIVE-LOCK.
      IF NOT AVAILABLE INTERNFAKTKOLL THEN LEAVE.
      INTERNFAKTKOLL.VECKOK = TRUE.
   END.
END.
PROCEDURE infak_UI:
   OPEN QUERY persq 
   FOR EACH PERSONALTAB WHERE PERSONALTAB.BRAVO = TRUE,
   EACH OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE AND 
   OMRADETAB.AVDELNINGNR = bolag 
   NO-LOCK.
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
         FIND FIRST TIMKOSTNADSTAB 
         WHERE TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIMKOSTNADSTAB.PRISTYP = 'TOT.PRIS.' USE-INDEX PRISPERS NO-LOCK NO-ERROR.
         totpristim = TIMKOSTNADSTAB.PRISA.       
         kodanst = ANSTFORMTAB.KOD.
         beravtvar = PERSONALTAB.BEREDSKAPSAVTAL.
      END.  
      OPEN QUERY tidq FOR EACH INTERNFAKTKOLL WHERE INTERNFAKTKOLL.VECKOK = FALSE AND 
      INTERNFAKTKOLL.VDATUM <= vkdatum NO-LOCK,
      EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.VECKOKORD = INTERNFAKTKOLL.VECKOKORD AND
      TIDREGITAB.PRISTYP NE "FRÅNVARO." 
      USE-INDEX PSTART NO-LOCK. 
      GET FIRST tidq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):   
         totpristim = TIDREGITAB.PRIS.
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = TIDREGITAB.AONR AND
         AONRTAB.DELNR = TIDREGITAB.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE AONRTAB THEN DO:
            aoomrade = PERSONALTAB.OMRADE. 
            varfaktyp = "".
         END.    
         ELSE DO:
            aoomrade = AONRTAB.OMRADE.   
            IF AONRTAB.OMRADE = "" THEN aoomrade = PERSONALTAB.OMRADE.   
            FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.AONR = TIDREGITAB.AONR AND
            AONRKONTKOD.DELNR = TIDREGITAB.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
            IF AVAILABLE AONRKONTKOD THEN varfaktyp = AONRKONTKOD.K3.
            ELSE varfaktyp = "".
         END.     
         /*ÄNDRAT 990517 PÅ BEGÄRAN FRÅN MARIE*/                        
         typdatum = STRING(DATE(MONTH(vkdatum),01,YEAR(vkdatum)),"99999999").
         /*typdatum = STRING(DATE(MONTH(TIDREGITAB.DATUM),01,YEAR(TIDREGITAB.DATUM)),"999999"). */
         /*BAKAB*/
         IF aoomrade = PERSONALTAB.OMRADE THEN musz = TRUE.
         ELSE IF globforetag = "ESMA" THEN DO:
            IF SUBSTRING(PERSONALTAB.OMRADE,1,1) = "4" AND SUBSTRING(AONRTAB.OMRADE,1,1) = "4" THEN DO:
               musz = TRUE.            
            END.
         END.
         IF musz = TRUE THEN musz = FALSE.   
         ELSE DO:
            FIND FIRST ekoforst WHERE
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
               ekoforst.ENY = FALSE
               ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD
               ekoforst.EPROJEKT = TIDREGITAB.AONR 
               ekoforst.DELNR = TIDREGITAB.DELNR 
               ekoforst.K3 = varfaktyp
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
                  IF traantal > 0 THEN RUN tra_UI.
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
                     (TIDREGITAB.PRIS * timtid)              
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
            END.
         END.
         GET NEXT tidq NO-LOCK.    
      END.  /*ELSE DO*/
      GET NEXT persq NO-LOCK.
   END.   /*FOR EACH*/     
END PROCEDURE.
PROCEDURE res_UI:                            /*DESSA KODER FINNS I N2.P OCH LON_UI*/   
   FIND FIRST LONTILL WHERE LONTILL.KOD = kodanst AND
   LONTILL.LONTILLAGG = reskod 
   USE-INDEX LON NO-LOCK NO-ERROR.
   IF NOT AVAILABLE LONTILL THEN RETURN.
   ASSIGN
   typover = SUBSTRING(LONTILL.TYPKOD,1,3)     
   multi =  LONTILL.MULTIP.                
   IF typover = "EJE" THEN RETURN.    
   IF typover = "REL" THEN resbelopp = (TIDREGITAB.PRIS * multi) * resantal. 
   ELSE resbelopp = (TIDREGITAB.PRIS + (TIDREGITAB.PRIS * multi)) * resantal. 
   IF ekoforst.ELONTILLAGG = reskod OR ekoforst.ELONTILLAGG = "" THEN DO:
      persrec = persrec.
   END.                                   
   ELSE DO:
      FIND FIRST ekoforst WHERE
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
   ekoforst.ENY = FALSE
   ekoforst.ERESULTENH = typover
   ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD
   ekoforst.EPROJEKT = TIDREGITAB.AONR 
   ekoforst.DELNR = TIDREGITAB.DELNR 
   ekoforst.K3 = varfaktyp
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
   IF typover = "RES" THEN DO:       
      FIND FIRST TIMKOSTNADSTAB 
      WHERE TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIMKOSTNADSTAB.PRISTYP = 'RESTID...' USE-INDEX PRISPERS NO-LOCK NO-ERROR.
      nytid = lonantal.
      RUN TIMSEK.P.
      lonantal = (sekunder / 3600).                 
      lonbelopp = (TIMKOSTNADSTAB.PRISA + (TIMKOSTNADSTA.PRISA * multi)) * lonantal.        
   END. 
   ELSE IF typover = "REL" THEN DO:      
      FIND FIRST TIMKOSTNADSTAB 
      WHERE TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIMKOSTNADSTAB.PRISTYP = 'RESTID...' USE-INDEX PRISPERS NO-LOCK NO-ERROR.
      nytid = lonantal.
      RUN TIMSEK.P.
      lonantal = (sekunder / 3600).                 
      lonbelopp = (TIMKOSTNADSTA.PRISA * multi) * lonantal.        
   END. 
   ELSE IF typover = "RE2" THEN DO:       
      FIND FIRST TIMKOSTNADSTAB 
      WHERE TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIMKOSTNADSTAB.PRISTYP = 'TOT.PRIS.' USE-INDEX PRISPERS NO-LOCK NO-ERROR.
      nytid = lonantal.
      RUN TIMSEK.P.
      lonantal = (sekunder / 3600).                 
      lonbelopp = (TIMKOSTNADSTAB.PRISA + (TIMKOSTNADSTA.PRISA * multi)) * lonantal.        
   END.
   ELSE IF typover = "OVE" THEN DO:    
      nytid = lonantal.
      RUN TIMSEK.P.
      lonantal = (sekunder / 3600).                                                    
      lonbelopp = (totpristim + (totpristim * multi)) * lonantal. 
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
         IF lonkod = "FRUK" OR 
         lonkod = "LUNC" OR 
         lonkod = "MIDD" OR 
         lonkod = "FRLU" OR
         lonkod = "FRMI" OR 
         lonkod = "LUMI" OR 
         lonkod = "FLMI" THEN DO:
            lonkod = "847".
            lonbelopp = lonbelopp * -1 * LONTILL.ERSATTNING.
         END.                                          
      END.
      ELSE RETURN.    
   END.
   IF ekoforst.ELONTILLAGG = lonkod OR ekoforst.ELONTILLAGG = "" THEN DO:
      persrec = persrec.
   END.                                   
   ELSE DO:
      FIND FIRST ekoforst WHERE
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
   ekoforst.ENY = FALSE
   ekoforst.ERESULTENH = typover
   ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD 
   ekoforst.EPROJEKT = TIDREGITAB.AONR 
   ekoforst.DELNR = TIDREGITAB.DELNR 
   ekoforst.K3 = varfaktyp
   ekoforst.EORG = PERSONALTAB.OMRADE  
   ekoforst.EGEO = aoomrade 
   ekoforst.ELONTILLAGG = lonkod       
   ekoforst.EVERDATUM = typdatum
   ekoforst.ELONTILLANTAL = ekoforst.ELONTILLANTAL + lonantal  
   ekoforst.ELONBELOPP = ekoforst.ELONBELOPP + lonbelopp. 
END PROCEDURE.                
PROCEDURE tra_UI:
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
   ekoforst.ENY = FALSE
   ekoforst.ERESULTENH = typover
   ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD
   ekoforst.EPROJEKT = TIDREGITAB.AONR 
   ekoforst.DELNR = TIDREGITAB.DELNR 
   ekoforst.K3 = varfaktyp
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
   ovbelopp = (TIDREGITAB.PRIS + (TIDREGITAB.PRIS * multi)) * ovantal.
   IF TIDREGITAB.PRIS = 0 AND TIDREGITAB.TIDLOG = FALSE THEN DO:
      FIND FIRST TIMKOSTNADSTAB 
      WHERE TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIMKOSTNADSTAB.PRISTYP = 'TOT.PRIS.' USE-INDEX PRISPERS NO-LOCK NO-ERROR.   
      ovbelopp = (TIMKOSTNADSTAB.PRISA + (TIMKOSTNADSTAB.PRISA * multi)) * ovantal.
   END.   
   IF ekoforst.ELONTILLAGG = ovkod OR ekoforst.ELONTILLAGG = "" THEN DO:
      persrec = persrec.
   END.                                   
   ELSE DO:
      FIND FIRST ekoforst WHERE
      ekoforst.ENY = FALSE AND
      ekoforst.ERESULTENH = typover AND
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
   ekoforst.ENY = FALSE
   ekoforst.ERESULTENH = typover
   ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD 
   ekoforst.EPROJEKT = TIDREGITAB.AONR 
   ekoforst.DELNR = TIDREGITAB.DELNR 
   ekoforst.K3 = varfaktyp
   ekoforst.EORG = PERSONALTAB.OMRADE  
   ekoforst.EGEO = aoomrade            
   ekoforst.EVERDATUM = typdatum
   ekoforst.ELONTILLAGG = ovkod
   ekoforst.ELONTILLANTAL = ekoforst.ELONTILLANTAL + ovantal  
   ekoforst.ELONBELOPP = ekoforst.ELONBELOPP + ovbelopp.   
END PROCEDURE.
 
