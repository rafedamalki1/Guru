  /*NORDEKO.P*/
DEFINE SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE kollvecka LIKE VECKONATT.VECKOKORD NO-UNDO.
DEFINE SHARED VARIABLE SEL_UPP AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valmanad AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE aonummer LIKE AONRTAB.AONR NO-UNDO.
DEFINE SHARED VARIABLE delnummer LIKE AONRTAB.DELNR NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO. 
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO. 
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
DEFINE VARIABLE berkod LIKE TIDREGITAB.BEREDSKAP NO-UNDO. 
DEFINE VARIABLE bbantal LIKE TIDREGITAB.BERANTAL NO-UNDO.
DEFINE VARIABLE berbelopp LIKE EKRAPPRESULT.EBERBELOPP NO-UNDO.
DEFINE VARIABLE pkod LIKE PERSONALTAB.PERSONALKOD NO-UNDO.
DEFINE VARIABLE typover LIKE EKRAPPRESULT.ERESULTENH NO-UNDO. 
DEFINE VARIABLE typdatum AS CHARACTER FORMAT "999999" NO-UNDO. 
DEFINE VARIABLE kodanst LIKE ANSTFORMTAB.KOD NO-UNDO.
DEFINE QUERY persq FOR PERSONALTAB.  
DEFINE QUERY tidq FOR TIDREGITAB.
DEFINE NEW SHARED TEMP-TABLE ekoforst
   FIELD ENY LIKE EKRAPPRESULT.ENY 
   FIELD EPERSONALKOD LIKE EKRAPPRESULT.EPERSONALKOD 
   FIELD EPROJEKT LIKE EKRAPPRESULT.EPROJEKT
   FIELD DELNR LIKE AONRTAB.DELNR 
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
gvisatidpermanad = TRUE.                     
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
      FIND FIRST TIMKOSTNADSTAB 
      WHERE TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIMKOSTNADSTAB.PRISTYP = 'TOT.PRIS.' USE-INDEX PRISPERS NO-LOCK NO-ERROR.
      totpristim = TIMKOSTNADSTAB.PRISA.       
      kodanst = ANSTFORMTAB.KOD.
   END.  
   IF aonummer = "" THEN DO:
      IF gvisatidpermanad = TRUE THEN DO:
         OPEN QUERY tidq FOR EACH TIDREGITAB WHERE
         TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.GODKAND NE "  " AND TIDREGITAB.VECKOKORD = "" AND
         TIDREGITAB.PRISTYP NE "FRÅNVARO." AND TIDREGITAB.DATUM <= vkdatum
         USE-INDEX PSTART NO-LOCK. 
      END.   
      ELSE DO:
         OPEN QUERY tidq FOR EACH TIDREGITAB WHERE
         TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.GODKAND NE "  " AND TIDREGITAB.VECKOKORD = "" AND
         TIDREGITAB.PRISTYP NE "FRÅNVARO." USE-INDEX PSTART NO-LOCK.
      END.   
   END.
   ELSE DO:     
      IF SEL_UPP = "ekonomi koll natt" THEN DO:
         OPEN QUERY tidq FOR EACH TIDREGITAB WHERE
         TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.AONR = aonummer AND 
         TIDREGITAB.DELNR = delnummer AND
         TIDREGITAB.VECKOKORD = kollvecka AND
         TIDREGITAB.PRISTYP NE "FRÅNVARO." USE-INDEX PSTART NO-LOCK.
      END.
      ELSE DO:
         OPEN QUERY tidq FOR EACH TIDREGITAB WHERE
         TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.AONR = aonummer AND 
         TIDREGITAB.DELNR = delnummer AND
         MONTH(TIDREGITAB.DATUM) = valmanad AND
         YEAR(TIDREGITAB.DATUM) = valar AND
         TIDREGITAB.VECKOKORD NE "" AND
         TIDREGITAB.PRISTYP NE "FRÅNVARO." USE-INDEX PSTART NO-LOCK.
      END.
   END.
   GET FIRST tidq NO-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB):
      totpristim = TIDREGITAB.PRIS.
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
         DO TRANSACTION:   
            /*BEREDSKAP*/
            FIND ekoforst WHERE RECID(ekoforst) = ekrid[1]
            EXCLUSIVE-LOCK NO-ERROR.         
            ASSIGN
            berbelopp = 0
            berkod = ""
            bbantal = 0.
            IF TIDREGITAB.BEREDSKAP NE " " THEN DO: 
               ASSIGN
               berkod = TIDREGITAB.BEREDSKAP
               nytid = TIDREGITAB.BERANTAL.
               RUN TIMSEK.P.
               bbantal = (sekunder / 3600).
               RUN bered_UI.                    
            END.          
         END.
      END.
      GET NEXT tidq NO-LOCK.    
   END.  /*ELSE DO*/
   GET NEXT persq NO-LOCK.
END.   /*FOR EACH*/     

RUN NORDEKO2.P.
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
            lonkod = "850".
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
   ekoforst.EORG = PERSONALTAB.OMRADE  
   ekoforst.EGEO = aoomrade            
   ekoforst.EVERDATUM = typdatum
   ekoforst.ELONTILLAGG = ovkod
   ekoforst.ELONTILLANTAL = ekoforst.ELONTILLANTAL + ovantal  
   ekoforst.ELONBELOPP = ekoforst.ELONBELOPP + ovbelopp.                           
END PROCEDURE.
PROCEDURE bered_UI:      
   multi = 0.
   IF berkod = "200" THEN multi = 0.110.              /*137/1243*/
   ELSE IF berkod = "201" THEN multi = 0.154.         /*137/889*/
   ELSE IF berkod = "202" THEN multi = 0.220.         /*137/622*/
   ELSE IF berkod = "203" THEN multi = 0.440.         /*137/311*/
   ELSE IF berkod = "204" THEN multi = 0.165.         /*137/830*/
   ELSE IF berkod = "205" THEN multi = 0.231.         /*137/592*/
   ELSE IF berkod = "206" THEN multi = 0.330.         /*137/415*/
   ELSE IF berkod = "207" THEN multi = 0.662.         /*137/207*/
   ELSE IF berkod = "210" THEN berkostnad = 14.00.
   ELSE IF berkod = "211" THEN berkostnad = 27.50.
   ELSE IF berkod = "212" THEN berkostnad = 21.00.
   ELSE IF berkod = "213" THEN berkostnad = 41.25.
   ELSE IF berkod = "215" THEN berkostnad = 34.50.
   ELSE IF berkod = "216" THEN berkostnad = 51.75.
   ELSE IF berkod = "260" THEN multi = 0.0685.         /*137/2000*/
   ELSE IF berkod = "261" THEN multi = 0.137.         /*137/1000*/
   ELSE IF berkod = "262" THEN multi = 0.274.         /*137/500*/
   ELSE IF berkod = "265" THEN multi = 0.103.         /*137/1333*/
   ELSE IF berkod = "266" THEN multi = 0.205.         /*137/667*/
   ELSE IF berkod = "267" THEN multi = 0.411.         /*137/333*/
   ELSE RETURN.      
   FIND FIRST TIMKOSTNADSTAB 
   WHERE TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIMKOSTNADSTAB.PRISTYP = 'TOT.PRIS.' USE-INDEX PRISPERS NO-LOCK NO-ERROR.  
   typover = "BER".
   IF multi = 0 THEN berbelopp = berkostnad * bbantal.
   ELSE berbelopp = (TIMKOSTNADSTAB.PRISA * multi) * bbantal. 
   IF ekoforst.ELONTILLAGG = berkod OR ekoforst.ELONTILLAGG = "" THEN DO:
      persrec = persrec.
   END.                                   
   ELSE DO:
      FIND FIRST ekoforst WHERE
      ekoforst.ENY = FALSE AND
      ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD AND
      ekoforst.EORG = PERSONALTAB.OMRADE AND 
      ekoforst.EGEO = aoomrade AND 
      ekoforst.EVERDATUM = typdatum AND
      ekoforst.ELONTILLAGG = berkod
      USE-INDEX PERSORG EXCLUSIVE-LOCK NO-ERROR.  
      IF NOT AVAILABLE ekoforst THEN DO:
         CREATE ekoforst.
      END.
   END.   
   ASSIGN 
   ekoforst.ENY = FALSE
   ekoforst.ERESULTENH = typover
   ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD  
   ekoforst.EORG = PERSONALTAB.OMRADE  
   ekoforst.EGEO = aoomrade
   ekoforst.EVERDATUM = typdatum 
   ekoforst.ELONTILLAGG = berkod
   ekoforst.ELONTILLANTAL = ekoforst.ELONTILLANTAL + bbantal  
   ekoforst.ELONBELOPP = ekoforst.ELONBELOPP + berbelopp. 
END PROCEDURE.                       
 
