  /*ELIX.P*/
/*  
DEFINE SHARED VARIABLE kollvecka LIKE VECKONATT.VECKOKORD NO-UNDO.
DEFINE SHARED VARIABLE SEL_UPP AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valmanad AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE aonummer LIKE AONRTAB.AONR NO-UNDO.
DEFINE SHARED VARIABLE delnummer LIKE AONRTAB.DELNR NO-UNDO.
*/
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO. 
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO. 



DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.

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
DEFINE VARIABLE enkpris LIKE TIMKOSTNADSTAB.PRISA NO-UNDO.
DEFINE VARIABLE kvalpris LIKE TIMKOSTNADSTAB.PRISA NO-UNDO.
DEFINE QUERY persq FOR PERSONALTAB.  
DEFINE QUERY tidq FOR TIDREGITAB.
DEFINE NEW SHARED TEMP-TABLE ekoforst
   FIELD ENY LIKE EKRAPPRESULT.ENY 
   FIELD EPERSONALKOD LIKE EKRAPPRESULT.EPERSONALKOD 
   FIELD EPROJEKT LIKE EKRAPPRESULT.EPROJEKT 
   FIELD DELNR LIKE AONRTAB.DELNR
   FIELD EORG LIKE EKRAPPRESULT.EORG  
   FIELD EGEO LIKE EKRAPPRESULT.EGEO  
   FIELD ETRANSDATUM LIKE EKRAPPRESULT.ETRANSDATUM
   FIELD EOVERJA LIKE EKRAPPRESULT.EOVERJA
   FIELD ERESULTENH LIKE EKRAPPRESULT.ERESULTENH 
   FIELD EBELOPP LIKE  EKRAPPRESULT.EBELOPP    
   FIELD EANTAL LIKE EKRAPPRESULT.EANTAL 
   FIELD ETIMMAR LIKE EKRAPPRESULT.EANTAL 	       
   FIELD ELONTILLAGG LIKE EKRAPPRESULT.ELONTILLAGG 
   FIELD ELONTILLANTAL LIKE EKRAPPRESULT.ELONTILLANTAL    
   FIELD ELONBELOPP LIKE EKRAPPRESULT.ELONBELOPP
   FIELD ERGL LIKE EKRAPPRESULT.ERGL          
   FIELD EVERK LIKE EKRAPPRESULT.EVERK    
   FIELD EKOSTNADSSLAG LIKE EKRAPPRESULT.EKOSTNADSSLAG         
   FIELD ESATS% LIKE EKRAPPRESULT.ESATS%
   FIELD EDK LIKE EKRAPPRESULT.EDK
   FIELD EDEBKRED LIKE EKRAPPRESULT.EDEBKRED
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP         
   INDEX PERSORG IS PRIMARY EPERSONALKOD EORG EGEO EPROJEKT DELNR ASCENDING
   INDEX KOST ENY EKOSTNADSSLAG
   INDEX TRANSD ETRANSDATUM ENY EORG EKOSTNADSSLAG 
   INDEX PRIS EPERSONALKOD PRISTYP.
   

FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG. 
   
pkod = "".                          
OPEN QUERY persq 
FOR EACH PERSONALTAB WHERE PERSONALTAB.BRAVO = TRUE USE-INDEX PERSONALKOD NO-LOCK.
GET FIRST persq NO-LOCK.
DO WHILE AVAILABLE(PERSONALTAB):
   IF PERSONALTAB.OMRADE = "elavd" OR PERSONALTAB.OMRADE = "IT" THEN DO:
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
      TIDREGITAB.GODKAND NE "  " AND TIDREGITAB.VECKOKORD = "V846" AND
      TIDREGITAB.PRISTYP NE "FRÅNVARO." USE-INDEX PSTART NO-LOCK.
      GET FIRST tidq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):
         /*RESTID*/                         
         IF TIDREGITAB.PRISTYP = 'RESTID...' THEN musz = musz.
         ELSE IF TIDREGITAB.PRISTYP = 'EJ.KOSTN.' THEN musz = musz.
         ELSE IF TIDREGITAB.AONR = "" THEN musz = musz. 
         ELSE DO:     
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
            IF aoomrade = "elavd" OR aoomrade = "IT" THEN DO:
               typdatum = DATE(MONTH(TIDREGITAB.DATUM),01,YEAR(TIDREGITAB.DATUM)). 
               FIND FIRST ekoforst WHERE ekoforst.ENY = FALSE AND
               ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD AND 
               ekoforst.EORG = PERSONALTAB.OMRADE AND 
               ekoforst.EGEO = aoomrade AND
               ekoforst.EPROJEKT = TIDREGITAB.AONR AND         
               ekoforst.DELNR = TIDREGITAB.DELNR AND
               ekoforst.ETRANSDATUM = typdatum AND
               ekoforst.PRISTYP = TIDREGITAB.PRISTYP     
               USE-INDEX PERSORG NO-LOCK NO-ERROR.
               IF NOT AVAILABLE ekoforst THEN DO TRANSACTION:
                  CREATE ekoforst.
                  ASSIGN 
                  ekoforst.ENY = FALSE
                  ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD            
                  ekoforst.PRISTYP = TIDREGITAB.PRISTYP
                  ekoforst.EPROJEKT = TIDREGITAB.AONR 
                  ekoforst.DELNR = TIDREGITAB.DELNR 
                  ekoforst.EORG = PERSONALTAB.OMRADE  
                  ekoforst.EGEO = aoomrade                         
                  ekoforst.ETRANSDATUM = typdatum.
               END.                                             
               ekrid[1] = RECID(ekoforst).      
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
                  RUN prissatt_UI.
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
                     RUN overbelopp_UI. 
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
                     RUN overbelopp_UI.        
                     /*
                     nytid = TIDREGITAB.TOTALT.
                     RUN TIMSEK.P.                  
                     ASSIGN ekoforst.EANTAL = ekoforst.EANTAL + (sekunder / 3600).                      
                     */
                  END.           
               END.         
            END.
         END.
         GET NEXT tidq NO-LOCK.    
      END.  /*ELSE DO*/
   END.
   GET NEXT persq NO-LOCK.
END.   /*FOR EACH*/     
RUN ELIX2.P.
PROCEDURE overbelopp_UI:
   FIND FIRST OVERKOD WHERE OVERKOD.KOD = kodanst AND
   OVERKOD.OVERTIDTILL = ovkod 
   USE-INDEX OVER NO-LOCK NO-ERROR.
   IF NOT AVAILABLE OVERKOD THEN RETURN.  
   IF OVERKOD.ENKEL = 'KVAL' THEN ASSIGN ovbelopp = (ovantal * kvalpris).
                             ELSE ASSIGN ovbelopp = (ovantal * enkpris).
   IF ekoforst.ELONTILLAGG = ovkod OR ekoforst.ELONTILLAGG = "" THEN DO:
      persrec = persrec.
   END.                                   
   ELSE DO:
      FIND FIRST ekoforst WHERE
      ekoforst.ENY = FALSE AND
      ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD AND
      ekoforst.EORG = PERSONALTAB.OMRADE AND 
      ekoforst.EGEO = aoomrade AND 
      ekoforst.ETRANSDATUM = typdatum AND
      ekoforst.EPROJEKT = TIDREGITAB.AONR AND
      ekoforst.DELNR = TIDREGITAB.DELNR AND
      ekoforst.ELONTILLAG = ovkod AND
      ekoforst.PRISTYP = TIDREGITAB.PRISTYP
      USE-INDEX PERSORG EXCLUSIVE-LOCK NO-ERROR.  
      IF NOT AVAILABLE ekoforst THEN DO:
         CREATE ekoforst.
      END.
   END.   
   ASSIGN 
   ekoforst.ENY = FALSE
   ekoforst.ERESULTENH = typover
   ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD
   ekoforst.PRISTYP = TIDREGITAB.PRISTYP 
   ekoforst.EPROJEKT = TIDREGITAB.AONR 
   ekoforst.DELNR = TIDREGITAB.DELNR 
   ekoforst.EORG = PERSONALTAB.OMRADE  
   ekoforst.EGEO = aoomrade            
   ekoforst.ETRANSDATUM = typdatum
   ekoforst.ELONTILLAGG = ovkod
   ekoforst.ELONTILLANTAL = ekoforst.ELONTILLANTAL + ovantal  
   ekoforst.ELONBELOPP = ekoforst.ELONBELOPP + ovbelopp.
END PROCEDURE.
PROCEDURE over_UI:
   ASSIGN
   ovbelopp = 0
   multi = 0.
   ASSIGN
  /* multi = OVERKOD.MULTIP*/
   typover = "OVE".
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
PROCEDURE prissatt_UI.
   FIND FIRST TIMKOSTNADSTAB 
   WHERE TIMKOSTNADSTAB.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
   TIMKOSTNADSTAB.PRISTYP = 'TIM+LÖNBI' USE-INDEX PRISPERS NO-LOCK NO-ERROR.   
   IF TIDREGITAB.PRISTYP = "TOT.PRIS." THEN DO:
      ASSIGN
      kvalpris = TIDREGITAB.PRIS + TIMKOSTNADSTAB.PRISA * 1 
      enkpris = TIDREGITAB.PRIS + TIMKOSTNADSTAB.PRISA * 0.5.
   END.
   ELSE IF TIDREGITAB.PRISTYP = "DEB.KOM.." THEN DO:
      ASSIGN
      kvalpris = TIDREGITAB.PRIS + TIMKOSTNADSTAB.PRISA * 1 
      enkpris = TIDREGITAB.PRIS + TIMKOSTNADSTAB.PRISA * 0.5.
   END.
   /*LASTBIL*/
   ELSE IF TIDREGITAB.PRISTYP = "LASTBIL.." THEN DO:
      ASSIGN
      kvalpris = TIDREGITAB.PRIS + TIMKOSTNADSTAB.PRISA * 1 
      enkpris = TIDREGITAB.PRIS + TIMKOSTNADSTAB.PRISA * 0.5.
   END.
   /*SKYLIFT*/
   ELSE IF TIDREGITAB.PRISTYP = "SKYLIFT.." THEN DO:
      ASSIGN
      kvalpris = TIDREGITAB.PRIS + TIMKOSTNADSTAB.PRISA * 1 
      enkpris = TIDREGITAB.PRIS + TIMKOSTNADSTAB.PRISA * 0.5.
   END.                        
   ELSE IF TIDREGITAB.PRISTYP = "GRÄVARE.." THEN DO:
      ASSIGN
      kvalpris = TIDREGITAB.PRIS + 40 
      enkpris = TIDREGITAB.PRIS + 40.
   END.
   ELSE IF TIDREGITAB.PRISTYP = "GIRAFF..." THEN DO:
      ASSIGN
      kvalpris = TIDREGITAB.PRIS + 40
      enkpris = TIDREGITAB.PRIS + 40.
   END.
   ELSE DO:
      MESSAGE "NU BLEV DET FEL! DEBITERINGSTYPEN FINNS EJ"
      TIDREGITAB.PRISTYP TIDREGITAB.PERSONALKOD TIDREGITAB.DATUM TIDREGITAB.AONR
      VIEW-AS ALERT-BOX.
   END.
END PROCEDURE.
