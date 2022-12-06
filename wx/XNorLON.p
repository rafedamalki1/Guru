  /*NORDEKO.P*/
DEFINE NEW SHARED VARIABLE valmanad AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE valar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE aonummer LIKE AONRTAB.AONR NO-UNDO.
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
DEFINE VARIABLE berkod LIKE TIDREGITAB.BEREDSKAP NO-UNDO. 
DEFINE VARIABLE bbantal LIKE TIDREGITAB.BERANTAL NO-UNDO.
DEFINE VARIABLE berbelopp LIKE EKRAPPRESULT.EBERBELOPP NO-UNDO.
DEFINE VARIABLE pkod LIKE PERSONALTAB.PERSONALKOD NO-UNDO.
DEFINE VARIABLE typover LIKE EKRAPPRESULT.ERESULTENH NO-UNDO. 
DEFINE VARIABLE typdatum AS DATE NO-UNDO. 
DEFINE VARIABLE kodanst LIKE ANSTFORMTAB.KOD NO-UNDO.
DEFINE QUERY persq FOR PERSONALTAB.  
DEFINE QUERY tidq FOR TIDREGITAB.
DEFINE NEW SHARED TEMP-TABLE ekoforst
   FIELD ENY LIKE EKRAPPRESULT.ENY 
   FIELD EPERSONALKOD LIKE EKRAPPRESULT.EPERSONALKOD 
   FIELD EPROJEKT LIKE EKRAPPRESULT.EPROJEKT
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD VECKOKORD LIKE TIDREGITAB.VECKOKORD
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP
   FIELD EORG LIKE EKRAPPRESULT.EORG  
   FIELD EGEO LIKE EKRAPPRESULT.EGEO  
   FIELD EVERDATUM AS DATE
   FIELD EOVERJA LIKE EKRAPPRESULT.EOVERJA
   FIELD ERESULTENH LIKE EKRAPPRESULT.ERESULTENH 
   FIELD EBELOPP LIKE  EKRAPPRESULT.EBELOPP 
   FIELD EANTAL LIKE EKRAPPRESULT.EANTAL 
   FIELD ETIMMAR LIKE EKRAPPRESULT.EANTAL                
   FIELD ELONTILLAGG LIKE EKRAPPRESULT.ELONTILLAGG 
   FIELD ELONTILLANTAL LIKE EKRAPPRESULT.ELONTILLANTAL    
   FIELD ELONBELOPP LIKE EKRAPPRESULT.ELONBELOPP         
   INDEX PERSORG IS PRIMARY EPERSONALKOD EORG EGEO EPROJEKT ASCENDING.
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
      FIND FIRST TIMKOSTNADSTAB 
      WHERE TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIMKOSTNADSTAB.PRISTYP = 'TOT.PRIS.' USE-INDEX PRISPERS NO-LOCK NO-ERROR.
      totpristim = TIMKOSTNADSTAB.PRISA.       
      kodanst = ANSTFORMTAB.KOD.
   END.  
   OPEN QUERY tidq FOR EACH TIDREGITAB WHERE
   TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.GODKAND NE "  " AND TIDREGITAB.VECKOKORD NE "" AND
      TIDREGITAB.LONTILLAGG NE "" AND YEAR(TIDREGITAB.DATUM) = 1998
      USE-INDEX PSTART NO-LOCK.
   
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
      typdatum = TIDREGITAB.DATUM. 
      FIND FIRST ekoforst WHERE
      ekoforst.ENY = FALSE AND
      ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD AND 
      ekoforst.EORG = PERSONALTAB.OMRADE AND 
      ekoforst.EGEO = aoomrade AND
      ekoforst.EPROJEKT = TIDREGITAB.AONR AND 
      ekoforst.DELNR = TIDREGITAB.DELNR AND
      ekoforst.PRISTYP = TIDREGITAB.PRISTYP AND                
      ekoforst.EVERDATUM = typdatum      
      USE-INDEX PERSORG NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ekoforst THEN DO TRANSACTION:
         CREATE ekoforst.
         ASSIGN 
         ekoforst.ENY = FALSE
         ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD
         ekoforst.EPROJEKT = TIDREGITAB.AONR 
         ekoforst.EORG = PERSONALTAB.OMRADE  
         ekoforst.DELNR = TIDREGITAB.DELNR
         ekoforst.PRISTYP = TIDREGITAB.PRISTYP
         ekoforst.VECKOKORD = TIDREGITAB.VECKOKORD   
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
      END.            
      ELSE DO:      
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
      GET NEXT tidq NO-LOCK.    
   END.  /*ELSE DO*/
   GET NEXT persq NO-LOCK.
END.   /*FOR EACH*/     
FOR EACH ekoforst where ekoforst.ELONBELOPP > 0 NO-LOCK:
   DO TRANSACTION:
      FIND FIRST SUMEJLON WHERE SUMEJLON.AONR = ekoforst.EPROJEKT AND
      SUMEJLON.DELNR = ekoforst.DELNR AND SUMEJLON.DATUM = ekoforst.EVERDATUM AND
      SUMEJLON.PERSONALKOD = ekoforst.EPERSONALKOD AND
      SUMEJLON.PRISTYP = ekoforst.PRISTYP AND SUMEJLON.TYPKOD = ekoforst.ERESULTENH
      USE-INDEX TYPKOD EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE SUMEJLON THEN CREATE SUMEJLON.
      ASSIGN
      SUMEJLON.AONR = ekoforst.EPROJEKT
      SUMEJLON.DATUM = ekoforst.EVERDATUM 
      SUMEJLON.DELNR = ekoforst.DELNR 
      SUMEJLON.LONKOST = SUMEJLON.LONKOST + ekoforst.ELONBELOPP
      SUMEJLON.PERSONALKOD = ekoforst.EPERSONALKOD
      SUMEJLON.PRISTYP = ekoforst.PRISTYP
      SUMEJLON.TYPKOD = ekoforst.ERESULTENH
      SUMEJLON.VECKOKORD = ekoforst.VECKOKORD.
   END.
END.   
PROCEDURE res_UI:
   multi = 0.                                 /*DESSA KODER FINNS I N2.P OCH LON_UI*/
   typover = "OVE".
   IF reskod = "080" THEN DO:             /*137/240  0.830*/
      typover = "REL".
      multi = 0.571.                
   END.
   ELSE IF reskod = "081" THEN DO:
      typover = "REL".
      multi = 0.721.               /*137/190   0.571*/
   END.
   ELSE IF reskod = "082" THEN DO:  
      typover = "REL".
      multi = 0.830.                /*137/165 0.721*/ 
   END.                         
   ELSE IF reskod = "440" THEN multi = 0.457.        /*137/94*/
   ELSE IF reskod = "441" THEN multi = 0.903.        /*137/72*/
   ELSE IF reskod = "445" THEN multi = 0.5.         
   ELSE IF reskod = "446" THEN multi = 0.75.  
   ELSE IF reskod = "447" THEN multi = 1.0. 
   ELSE IF reskod = "448" THEN multi = 1.5. 
   ELSE RETURN.                     
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
      ekoforst.PRISTYP = TIDREGITAB.PRISTYP AND      
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
   ekoforst.DELNR = TIDREGITAB.DELNR
   ekoforst.PRISTYP = TIDREGITAB.PRISTYP
   ekoforst.VECKOKORD = TIDREGITAB.VECKOKORD
   ekoforst.EPROJEKT = TIDREGITAB.AONR 
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
   IF lonkod = "410" THEN DO:
      typover = "RE2".
      multi = 0.75. 
   END.
   ELSE IF lonkod = "411" THEN DO:
      typover = "RE2".
      multi = 0.90.
   END.
   ELSE IF lonkod = "412" THEN DO:
      typover = "RE2".
      multi = 0.75.
   END.
   ELSE IF lonkod = "413" THEN DO:
      typover = "RE2".
      multi = 0.90.
   END.
   ELSE IF lonkod = "414" THEN DO:
      typover = "RE2".
      multi = 0.254.          /*137/540*/   
   END.
   ELSE IF lonkod = "415" THEN DO:
      typover = "RE2".
      multi = 0.457.          /*137/300*/
   END.
   ELSE IF lonkod = "416" THEN DO:
      typover = "RE2".
      multi = 0.913.        /*137/150*/   
   END.
   ELSE IF lonkod = "491" THEN DO:
      typover = "RE2".
      multi = 0.    /*VET EJ*/
   END.
   ELSE IF lonkod = "492" THEN multi = 0.    /*VET EJ*/
   ELSE IF lonkod = "493" THEN multi = 0.    /*VET EJ*/   
   ELSE IF lonkod = "080" THEN DO:      /*DESSA KODER FINNS I N2.P OCH res_UI*/
      typover = "REL".
      multi = 0.571.               /*137/240*/
   END.
   ELSE IF lonkod = "081" THEN DO:
      typover = "REL".
       multi = 0.721.                /*137/190*/
   END.
   ELSE IF lonkod = "082" THEN DO:  
      typover = "REL".
      multi = 0.830.                      /*137/165*/ 
   END.                                      
   ELSE IF lonkod = "380" THEN DO:   /*DESSA KODER FINNS I N2.P over_UI*/
      typover = "OVE".
      multi = 0. 
   END. 
   ELSE IF lonkod = "334" THEN DO:   /*DESSA KODER FINNS I N2.P over_UI*/
      typover = "OVE".
      multi = 0. 
   END.
   ELSE IF lonkod = "435" THEN DO:
      typover = "OVE".
      multi = 0.5.    
   END.   
   ELSE IF lonkod = "436" THEN DO:
      typover = "OVE".
      multi = 0.75.   
   END.
   ELSE IF lonkod = "437" THEN DO:
      typover = "OVE".
      multi = 1.0.   
   END.
   ELSE IF lonkod = "438" THEN DO:
      typover = "OVE".
      multi = 1.5.                    
   END.
   ELSE IF lonkod = "450" THEN DO:
      typover = "OVE".
      multi = 0.254.                     /*137/540*/
   END.
   ELSE IF lonkod = "451" THEN DO:
      typover = "OVE".
      multi = 0.457.                     /*137/300*/
   END.
   ELSE IF lonkod = "452" THEN DO:
      typover = "OVE".
      multi = 0.913.
   END.
   ELSE IF lonkod = "440" THEN DO:  /*DESSA KODER FINNS I N2.P res_UI*/
      typover = "RES".
      multi = 0.457.               /*137/94*/
   END.
   ELSE IF lonkod = "441" THEN DO:
      typover = "RES".
      multi = 0.903.               /*137/72*/
   END.
   ELSE IF lonkod = "445" THEN DO:
      typover = "RES".
      multi = 0.5.         
   END.
   ELSE IF lonkod = "446" THEN DO:
      typover = "RES".
      multi = 0.75.  
   END.
   ELSE IF lonkod = "447" THEN DO:
      typover = "RES".
      multi = 1.0. 
   END.
   ELSE IF lonkod = "448" THEN DO:
      typover = "RES".
      multi = 1.5. 
   END.                        
   ELSE IF lonkod = "730" THEN DO:
      typover = "MIL".     
   END.
   ELSE IF lonkod = "731" THEN DO:
      typover = "MIL".     
   END.
   ELSE IF lonkod = "732" THEN DO:
      typover = "MIL".     
   END.   
   ELSE IF lonkod = "740" THEN DO:
      typover = "MIL".     
   END.
   ELSE IF lonkod = "741" THEN DO:
      typover = "MIL".     
   END.
   ELSE IF lonkod = "750" THEN DO:
      typover = "MIL".     
   END.
   ELSE IF lonkod = "751" THEN DO:
      typover = "MIL".     
   END.
   ELSE IF lonkod = "792" THEN DO:
      typover = "TRA".     
   END. 
   ELSE IF lonkod = "820" THEN DO:
      typover = "TRA".     
   END. 
   ELSE IF lonkod = "821" THEN DO:
      typover = "TRA".     
   END.
   ELSE IF lonkod = "822" THEN DO:
      typover = "TRA".     
   END.
   ELSE IF lonkod = "823" THEN DO:
      typover = "TRA".     
   END.
   ELSE IF lonkod = "840" THEN DO:
      typover = "TRA".     
   END.
   ELSE IF lonkod = "841" THEN DO:
      typover = "TRA".     
   END.
   ELSE IF lonkod = "842" THEN DO:
      typover = "TRA".     
   END.
   ELSE IF lonkod = "843" THEN DO:
      typover = "TRA".     
   END.
   ELSE IF lonkod = "844" THEN DO:
      typover = "TRA".     
   END.
   ELSE IF lonkod = "845" THEN DO:
      typover = "TRA".     
   END.
   ELSE IF lonkod = "855" THEN DO:
      typover = "TRA".     
   END.
   ELSE IF lonkod = "860" THEN DO:
      typover = "TRA".     
   END.                 
   ELSE IF lonkod = "861" THEN DO:
      typover = "TRA".     
   END.      
   ELSE RETURN.
   IF typover =  "RES" THEN DO:
      typover = "OVE". 
      FIND FIRST TIMKOSTNADSTAB 
      WHERE TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIMKOSTNADSTAB.PRISTYP = 'RESTID...' USE-INDEX PRISPERS NO-LOCK NO-ERROR.
      nytid = lonantal.
      RUN TIMSEK.P.
      lonantal = (sekunder / 3600).                 
      lonbelopp = (TIMKOSTNADSTAB.PRISA + (TIMKOSTNADSTA.PRISA * multi)) * lonantal.        
   END. 
   ELSE IF typover =  "REL" THEN DO:
      typover = "REL". 
      FIND FIRST TIMKOSTNADSTAB 
      WHERE TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIMKOSTNADSTAB.PRISTYP = 'RESTID...' USE-INDEX PRISPERS NO-LOCK NO-ERROR.
      nytid = lonantal.
      RUN TIMSEK.P.
      lonantal = (sekunder / 3600).                 
      lonbelopp = (TIMKOSTNADSTA.PRISA * multi) * lonantal.        
   END. 
   ELSE IF typover = "RE2" THEN DO:
      typover = "REL". 
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
      ekoforst.PRISTYP = TIDREGITAB.PRISTYP AND     
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
   ekoforst.PRISTYP = TIDREGITAB.PRISTYP
   ekoforst.VECKOKORD = TIDREGITAB.VECKOKORD
   ekoforst.EORG = PERSONALTAB.OMRADE  
   ekoforst.EGEO = aoomrade 
   ekoforst.ELONTILLAGG = lonkod       
   ekoforst.EVERDATUM = typdatum
   ekoforst.ELONTILLANTAL = ekoforst.ELONTILLANTAL + lonantal  
   ekoforst.ELONBELOPP = ekoforst.ELONBELOPP + lonbelopp. 
END PROCEDURE.                
