/*DEFINE INPUT PARAMETER kordatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER koranv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/
/*DEFINE INPUT PARAMETER perioden AS INTEGER FORMAT "999" NO-UNDO.*/

DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.   
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.     
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE NEW SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.
DEFINE NEW SHARED VARIABLE tperiod AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.

DEFINE NEW SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE NEW SHARED VARIABLE regtotalt LIKE TIDREGITAB.TOTALT NO-UNDO.
DEFINE NEW SHARED VARIABLE frustarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE fruslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffestart AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffeslut AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchstarten LIKE ARBETSTIDTAB.LUNCHSTART NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchslutet LIKE ARBETSTIDTAB.LUNCHSLUT NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER NO-UNDO.    


DEFINE VARIABLE antal LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.   /*LÖN*/
DEFINE VARIABLE antalet LIKE PHJALP.PPAR8 NO-UNDO.   /*LÖN*/
DEFINE VARIABLE personal LIKE PERSONALTAB.PERSONALKOD.      /*LÖN*/
DEFINE VARIABLE pnr LIKE PERSONALTAB.PERSONNUMMER.      /*LÖN*/
DEFINE VARIABLE vecknr LIKE TIDREGITAB.VECKONUMMER.      /*LÖN*/
DEFINE VARIABLE overrapp1 AS CHARACTER FORMAT "X(70)".      /*LÖN*/
DEFINE VARIABLE overrapp2 AS CHARACTER FORMAT "X(70)".      /*ÖVERTID*/
DEFINE VARIABLE overrapp3 AS CHARACTER FORMAT "X(70)".      /*BEREEDSKAP*/
DEFINE VARIABLE overrapp4 AS CHARACTER FORMAT "X(70)".      /*TRAKTAMENTE*/
DEFINE VARIABLE rapphj1 AS CHARACTER FORMAT "X(9)".      /*HJÄLP ANTAL*/
DEFINE VARIABLE rapphj2 AS CHARACTER FORMAT "X(11)".      /*HJÄLP belopp*/
DEFINE VARIABLE datrapphj1 AS DATE FORMAT "999999".      /*HJÄLP datum*/
DEFINE VARIABLE rec AS RECID.
DEFINE VARIABLE para AS LOGICAL NO-UNDO.
DEFINE VARIABLE avtal1 LIKE PERSONALTAB.ANSTALLNING NO-UNDO.
DEFINE VARIABLE avtal LIKE ANSTFORMTAB.KOD NO-UNDO.  
DEFINE VARIABLE oversum AS INTEGER NO-UNDO. 
DEFINE VARIABLE total AS INTEGER NO-UNDO.
DEFINE VARIABLE oa1 AS INTEGER NO-UNDO.
DEFINE VARIABLE oa2 AS INTEGER NO-UNDO.
DEFINE VARIABLE oa3 AS INTEGER NO-UNDO.
DEFINE VARIABLE t2 AS INTEGER NO-UNDO.
DEFINE VARIABLE t3 AS INTEGER NO-UNDO.
DEFINE VARIABLE bryt AS LOGICAL NO-UNDO.
DEFINE VARIABLE nod AS LOGICAL NO-UNDO.
DEFINE VARIABLE flex AS LOGICAL NO-UNDO.
DEFINE VARIABLE fltid AS INTEGER NO-UNDO.
DEFINE VARIABLE seku AS INTEGER NO-UNDO.
DEFINE VARIABLE frvaro AS INTEGER NO-UNDO.
DEFINE VARIABLE bolag AS CHARACTER FORMAT "X(2)" NO-UNDO.
DEFINE VARIABLE flkod LIKE TIDREGITAB.LONTILLAGG NO-UNDO.
DEFINE VARIABLE arrflex AS INTEGER NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE pa90fil
   FIELD PPERSONNUMMER LIKE PERSONALTAB.PERSONNUMMER
   FIELD PLONTILLAGG LIKE TIDREGITAB.LONTILLAGG
   FIELD PLONTILLANTAL AS DECIMAL FORMAT "-999.99"
   FIELD POVERTIDTILL LIKE TIDREGITAB.OVERTIDTILL
   FIELD POVERANTAL LIKE TIDREGITAB.OVERANTAL
   FIELD PTRAKTKOD LIKE TIDREGITAB.TRAKTKOD
   FIELD PTRAKTANTAL LIKE TIDREGITAB.TRAKTANTAL
   FIELD PBEREDSKAP LIKE TIDREGITAB.BEREDSKAP
   FIELD PBERANTAL LIKE TIDREGITAB.BERANTAL
   FIELD PDATUM LIKE TIDREGITAB.DATUM
   FIELD PVECKONUMMER LIKE TIDREGITAB.VECKONUMMER
   FIELD PSORT AS CHARACTER FORMAT "XX"
   FIELD PPAR8 AS DECIMAL FORMAT "-99.99"
   FIELD PANSTNR LIKE PERSONALTAB.ANSTNR
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD PAVTAL AS CHARACTER
   FIELD BOLAG AS CHARACTER FORMAT "X(2)"
   INDEX PANSTNR IS PRIMARY PANSTNR ASCENDING.
DEFINE TEMP-TABLE flexen
   FIELD PLONTILLANTAL AS DECIMAL FORMAT "-9999999.99"
   FIELD PDATUM LIKE TIDREGITAB.DATUM
   FIELD PKOD LIKE PERSONALTAB.PERSONALKOD
   INDEX PKOD IS PRIMARY PKOD ASCENDING.      
DEFINE TEMP-TABLE flexsum
   FIELD PLONTILLANTAL AS DECIMAL FORMAT "-9999999.99"
   FIELD PLONTILLSEK AS INTEGER
   FIELD PKOD LIKE PERSONALTAB.PERSONALKOD
   INDEX PKOD IS PRIMARY PKOD ASCENDING.      

FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
globanv = "ELPA".
vkdatum = 06/07/2001.
tperiod = 922.
FOR EACH pa90fil:
  DELETE pa90fil.
END.

OPEN QUERY pq FOR EACH PERSONALTAB WHERE PERSONALTAB.PERSMASK = TRUE AND PERSONALTAB.ANSTALLNING = 
"kOLLEKTIV nORRKÖPING 7-16" USE-INDEX PERSONALKOD NO-LOCK.
GET FIRST pq NO-LOCK.
DO WHILE AVAILABLE(PERSONALTAB):
   personal = PERSONALTAB.PERSONALKOD.
   avtal1 = PERSONALTAB.ANSTALLNING.
   persrec = RECID(PERSONALTAB).
   FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = avtal1
   USE-INDEX ANSTF NO-LOCK NO-ERROR.
   avtal = ANSTFORMTAB.KOD.
   
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
   ASSIGN  bolag = STRING(OMRADETAB.TIMKOST).
   
   FIND FIRST UTRYCKNING WHERE UTRYCKNING.KOD = avtal USE-INDEX UT
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE UTRYCKNING THEN para = FALSE.
   ELSE para = UTRYCKNING.PARA8.
   FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD 
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.            
   OPEN QUERY tq FOR EACH TIDREGITAB WHERE
   TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.VECKOKORD = "W20010601" USE-INDEX PSTART NO-LOCK. 
   
   GET FIRST tq NO-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB):
      IF (TIDREGITAB.TRAKTKOD = " "  OR TIDREGITAB.TRAKTANTAL = 0) AND
      (TIDREGITAB.LONTILLAGG = " "  OR TIDREGITAB.LONTILLANTAL = 0) AND
      (TIDREGITAB.OKOD1 = " " OR TIDREGITAB.OANT1 = 0) AND
      (TIDREGITAB.OKOD2 = " " OR TIDREGITAB.OANT2 = 0) AND
      (TIDREGITAB.OKOD3 = " " OR TIDREGITAB.OANT3 = 0) AND
      (TIDREGITAB.BEREDSKAP = " " OR TIDREGITAB.BERANTAL = 0)
      THEN DO:         
         /* om flextid skall räknas ut*/ 
    	   flex = FALSE.
    	   IF AVAILABLE FLEXAVT AND FLEXAVT.FLEXTID = TRUE THEN DO:
    	      regdatum = TIDREGITAB.DATUM.
    	      regvnr = TIDREGITAB.VECKONUMMER.
    	      RUN SLUTARB.P.
    	      musz = FALSE.
    	      IF TIDREGITAB.AONR = "910927" OR TIDREGITAB.AONR = "27" OR
    	      TIDREGITAB.AONR BEGINS "1097" THEN DO:
    	         nytid = TIDREGITAB.TOTALT.
    	         RUN TIMSEK.P.
    	         CREATE flexen.
               ASSIGN flexen.PKOD = PERSONALTAB.PERSONALKOD
               flexen.PLONTILLANTAL = (-1) * sekunder
               flexen.PDATUM = TIDREGITAB.DATUM.
            END.   
    	      /*IF bolag = "45" OR bolag = "49" OR bolag = "51" THEN DO:*/
    	         IF avtal BEGINS "K" AND TIDREGITAB.OVERTIDUTTAG = "F" AND
    	         TIDREGITAB.LONTILLAGG = "" THEN musz = TRUE.
            /*END.*/
            IF musz = TRUE THEN DO:
               musz = FALSE.
               IF TIDREGITAB.START < regstart THEN DO:                                                                     
                  nytid = TIDREGITAB.TOTALT.
                  RUN TIMSEK.P.
                  fltid = fltid + sekunder.                       
               END.
               ELSE IF TIDREGITAB.START GE regslut THEN DO:                                                             
                  ASSIGN                    
                  nytid = TIDREGITAB.TOTALT.
                  RUN TIMSEK.P.
                  fltid = fltid + sekunder.              
               END.            
               IF fltid NE 0 THEN DO:
                  FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
                  FIND FIRST AONRTAB WHERE 
                  AONRTAB.AONR = "1097" + SUBSTRING(STRING(OMRADETAB.ORGIDNUM,"9999"),1,2) 
                  AND AONRTAB.DELNR = INTEGER(SUBSTRING(STRING(OMRADETAB.ORGIDNUM,"9999"),3,2))
                  NO-LOCK NO-ERROR.               
                  IF AVAILABLE AONRTAB THEN DO:
                     sekunder = (-1) * fltid.
                     RUN FSEKTIM.P.           
                     CREATE pa90fil.
                     ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
                     pa90fil.PLONTILLAGG = "100"
                     pa90fil.PLONTILLANTAL = fnytid
                     pa90fil.PSORT = "TI"
                     pa90fil.PDATUM = TIDREGITAB.DATUM
                     pa90fil.AONR = "1097" + SUBSTRING(STRING(OMRADETAB.ORGIDNUM,"9999"),1,2)
                     pa90fil.DELNR = INTEGER(SUBSTRING(STRING(OMRADETAB.ORGIDNUM,"9999"),3,2))
                     pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
                     pa90fil.PAVTAL = avtal
                     pa90fil.BOLAG = bolag.
                     CREATE flexen.
                     ASSIGN flexen.PKOD = PERSONALTAB.PERSONALKOD
                     flexen.PLONTILLANTAL = (-1) * sekunder
                     flexen.PDATUM = TIDREGITAB.DATUM.
                     fltid = 0.         
                  END.   
               END.                                   
            END.    
            ELSE IF TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.LONTILLAGG = "" THEN DO:
               IF TIDREGITAB.AONR = "910927" OR TIDREGITAB.AONR = "27" THEN fltid = fltid.
               ELSE IF regstart = regslut THEN DO:
                  nytid = TIDREGITAB.TOTALT.
                  RUN TIMSEK.P.
                  fltid = fltid + sekunder.      
               END.   
               ELSE IF TIDREGITAB.START < regstart THEN DO:                                                                     
                  nytid = TIDREGITAB.TOTALT.
                  RUN TIMSEK.P.
                  fltid = fltid + sekunder.                       
               END.
               ELSE IF TIDREGITAB.START GE regslut THEN DO:                                                             
                  ASSIGN                    
                  nytid = TIDREGITAB.TOTALT.
                  RUN TIMSEK.P.
                  fltid = fltid + sekunder.              
               END.
            END.  
            IF fltid NE 0 THEN DO:
               IF bolag = "41" THEN DO:
                  IF avtal = "T" THEN  ASSIGN flkod = "399". /*ENBART GAMLA NORDKRAFT*/
                  ELSE ASSIGN flkod = "119".
               END.
               ELSE ASSIGN flkod = "119".
               sekunder = fltid.
               RUN FSEKTIM.P.           
               CREATE pa90fil.
               ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
               pa90fil.PLONTILLAGG = flkod
               pa90fil.PLONTILLANTAL = fnytid
               pa90fil.PSORT = "TI"
               pa90fil.PDATUM = TIDREGITAB.DATUM
               pa90fil.AONR = TIDREGITAB.AONR
               pa90fil.DELNR = TIDREGITAB.DELNR
               pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
               pa90fil.PAVTAL = avtal
               pa90fil.BOLAG = bolag.
               CREATE flexen.
               ASSIGN flexen.PKOD = PERSONALTAB.PERSONALKOD
               flexen.PLONTILLANTAL = sekunder
               flexen.PDATUM = TIDREGITAB.DATUM.
               fltid = 0. 
               flex = TRUE.
            END.   
         END.   	     
         frvaro = 0.
         FIND FIRST FVARO WHERE FVARO.AONR = TIDREGITAB.AONR AND FVARO.DELNR = TIDREGITAB.DELNR
         NO-LOCK NO-ERROR.
         IF AVAILABLE FVARO THEN ASSIGN frvaro = 1.
         IF PERSONALTAB.BEFATTNING = "TIMANSTÄLLD" AND TIDREGITAB.TOTALT > 0 THEN DO:                                                                     
            IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
            ELSE DO:	     
               CREATE pa90fil.
               ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
               pa90fil.PLONTILLAGG = "010"
               pa90fil.PLONTILLANTAL = TIDREGITAB.TOTALT
               pa90fil.PDATUM = TIDREGITAB.DATUM
               pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
               pa90fil.AONR = TIDREGITAB.AONR
               pa90fil.DELNR = TIDREGITAB.DELNR
               pa90fil.PAVTAL = avtal
               pa90fil.BOLAG = bolag.
            END.   
         END.
         ELSE IF PERSONALTAB.BEFATTNING = "TIMANS.+SEM TJ" AND TIDREGITAB.TOTALT > 0 THEN DO:                                                                     
            IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
            ELSE DO:            
               CREATE pa90fil.  /* VILART = 011*/
               ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR.
               IF globforetag = "NORD" OR globforetag = "ESMA" 
               OR globforetag = "ELPA" OR globforetag = "ESAN" THEN DO:
                  ASSIGN pa90fil.PLONTILLAGG = "011". /*040*/
               END.
               ELSE IF globforetag = "ETA" THEN DO:
                      ASSIGN pa90fil.PLONTILLAGG = "011".
               END. 
               ASSIGN   
               pa90fil.PLONTILLANTAL = TIDREGITAB.TOTALT
               pa90fil.PDATUM = TIDREGITAB.DATUM
               pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
               pa90fil.AONR = TIDREGITAB.AONR
               pa90fil.DELNR = TIDREGITAB.DELNR
               pa90fil.PAVTAL = avtal
               pa90fil.BOLAG = bolag.
            END.   
         END.
         ELSE IF PERSONALTAB.BEFATTNING = "TIMANS.+SEM KOLL" AND TIDREGITAB.TOTALT > 0 THEN DO:                                                                     
            IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
            ELSE DO:
               CREATE pa90fil.  /* VILART = 011*/
               ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR.
               IF globforetag = "NORD" OR globforetag = "ESMA" 
               OR globforetag = "ELPA" OR globforetag = "ESAN" THEN DO:
                  ASSIGN pa90fil.PLONTILLAGG = "011". /*030*/
               END.
               ELSE IF globforetag = "ETA" THEN DO:
                      ASSIGN pa90fil.PLONTILLAGG = "011".
               END. 
               ASSIGN
               pa90fil.PLONTILLANTAL = TIDREGITAB.TOTALT
               pa90fil.PDATUM = TIDREGITAB.DATUM
               pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
               pa90fil.AONR = TIDREGITAB.AONR
               pa90fil.DELNR = TIDREGITAB.DELNR
               pa90fil.PAVTAL = avtal
               pa90fil.BOLAG = bolag.
            END.   
         END.
         ELSE IF flex = TRUE THEN nytid = nytid.
         ELSE IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
         ELSE IF TIDREGITAB.PRISTYP = "RESTID..."  THEN nytid = nytid.
         ELSE IF TIDREGITAB.LONTILLAGG NE "" THEN nytid = nytid.
         ELSE IF frvaro = 1 THEN ASSIGN frvaro = 0.  /*frånvaro*/
         /*ELSE IF TIDREGITAB.AONR BEGINS "910" THEN nytid = nytid.*/
         ELSE IF globforetag = "ETA" AND TIDREGITAB.AONR = "45" THEN nytid = nytid.
         ELSE IF ( TIDREGITAB.OKOD1 = " " AND TIDREGITAB.OKOD2 = " " AND
         TIDREGITAB.OKOD3 = " " ) AND TIDREGITAB.TOTALT > 0 THEN DO:
            regdatum = TIDREGITAB.DATUM.
            regvnr = TIDREGITAB.VECKONUMMER.
            RUN SLUTARB.P.
            IF TIDREGITAB.START < regstart OR  TIDREGITAB.START GE regslut
               THEN DO:
               CREATE pa90fil.  /* TIMMAR LÖNEART 100*/
               ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
               pa90fil.PLONTILLAGG = "100"
               pa90fil.PLONTILLANTAL = TIDREGITAB.TOTALT
               pa90fil.PDATUM = TIDREGITAB.DATUM
               pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
               pa90fil.AONR = TIDREGITAB.AONR
               pa90fil.DELNR = TIDREGITAB.DELNR
               pa90fil.PAVTAL = avtal
               pa90fil.BOLAG = bolag.
            END.
         END.            
     END.
     ELSE DO:
     
     
       flex = FALSE.  
       IF AVAILABLE FLEXAVT AND FLEXAVT.FLEXTID = TRUE THEN DO:
          regdatum = TIDREGITAB.DATUM.
          regvnr = TIDREGITAB.VECKONUMMER.
          RUN SLUTARB.P.
          musz = FALSE.
         /* IF bolag = "45" OR bolag = "49" OR bolag = "51" THEN DO:*/
             IF avtal BEGINS "K" AND TIDREGITAB.OVERTIDUTTAG = "F" AND
             TIDREGITAB.LONTILLAGG = "" THEN musz = TRUE.
          /*END.*/
          IF musz = TRUE THEN DO:
             musz = FALSE.
             IF TIDREGITAB.START < regstart THEN DO:                                                                     
               nytid = TIDREGITAB.TOTALT.
               RUN TIMSEK.P.
               fltid = fltid + sekunder.                       
             END.
             ELSE IF TIDREGITAB.START GE regslut THEN DO:                                                             
               ASSIGN                    
               nytid = TIDREGITAB.TOTALT.
               RUN TIMSEK.P.
               fltid = fltid + sekunder.              
             END.            
             IF fltid NE 0 THEN DO:
                FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
                FIND FIRST AONRTAB WHERE 
                AONRTAB.AONR = "1097" + SUBSTRING(STRING(OMRADETAB.ORGIDNUM,"9999"),1,2) 
                AND AONRTAB.DELNR = INTEGER(SUBSTRING(STRING(OMRADETAB.ORGIDNUM,"9999"),3,2))
                NO-LOCK NO-ERROR.               
                IF AVAILABLE AONRTAB THEN DO:
                   sekunder = (-1) * fltid.
                   RUN FSEKTIM.P.           
                   CREATE pa90fil.
                   ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
                   pa90fil.PLONTILLAGG = "100"
                   pa90fil.PLONTILLANTAL = fnytid
                   pa90fil.PSORT = "TI"
                   pa90fil.PDATUM = TIDREGITAB.DATUM
                   pa90fil.AONR = "1097" + SUBSTRING(STRING(OMRADETAB.ORGIDNUM,"9999"),1,2)
                   pa90fil.DELNR = INTEGER(SUBSTRING(STRING(OMRADETAB.ORGIDNUM,"9999"),3,2))
                   pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
                   pa90fil.PAVTAL = avtal
                   pa90fil.BOLAG = bolag.
                   CREATE flexen.
                   ASSIGN flexen.PKOD = PERSONALTAB.PERSONALKOD
                   flexen.PLONTILLANTAL = (-1) * sekunder
                   flexen.PDATUM = TIDREGITAB.DATUM.
                   fltid = 0.         
                END.   
             END.                                   
          END.    
          ELSE IF TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.LONTILLAGG = "" THEN DO:
             IF TIDREGITAB.AONR = "910927" OR TIDREGITAB.AONR = "27" THEN fltid = fltid.
             ELSE IF regstart = regslut THEN DO:
                nytid = TIDREGITAB.TOTALT.
                RUN TIMSEK.P.
                fltid = fltid + sekunder.      
             END.   
             ELSE IF TIDREGITAB.START < regstart THEN DO:                                                                     
                nytid = TIDREGITAB.TOTALT.
                RUN TIMSEK.P.
                fltid = fltid + sekunder.                       
             END.
             ELSE IF TIDREGITAB.START GE regslut THEN DO:                                                             
                ASSIGN                    
                nytid = TIDREGITAB.TOTALT.
                RUN TIMSEK.P.
                fltid = fltid + sekunder.              
             END.
          END.  
          IF fltid NE 0 THEN DO:
             IF bolag = "41" THEN DO:
                IF avtal = "T" THEN ASSIGN flkod = "399". /*ENBART GAMLA NORDKRAFT*/
                ELSE ASSIGN flkod = "119".
             END.
             ELSE ASSIGN flkod = "119".
             sekunder = fltid.
             RUN FSEKTIM.P.           
             CREATE pa90fil.
             ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
             pa90fil.PLONTILLAGG = flkod
             pa90fil.PLONTILLANTAL = fnytid
             pa90fil.PSORT = "TI"
             pa90fil.PDATUM = TIDREGITAB.DATUM
             pa90fil.AONR = TIDREGITAB.AONR
             pa90fil.DELNR = TIDREGITAB.DELNR
             pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
             pa90fil.PAVTAL = avtal
             pa90fil.BOLAG = bolag.
             CREATE flexen.
             ASSIGN flexen.PKOD = PERSONALTAB.PERSONALKOD
             flexen.PLONTILLANTAL = sekunder
             flexen.PDATUM = TIDREGITAB.DATUM.
             fltid =0. 
             flex = TRUE.
          END.   
       END.   	     
       /* om flextid skall räknas ut*/
       frvaro = 0.
       FIND FIRST FVARO WHERE FVARO.AONR = TIDREGITAB.AONR AND FVARO.DELNR = TIDREGITAB.DELNR
       NO-LOCK NO-ERROR.
       IF AVAILABLE FVARO THEN ASSIGN frvaro = 1. 
       IF PERSONALTAB.BEFATTNING = "TIMANSTÄLLD" AND TIDREGITAB.TOTALT > 0 THEN DO:
          IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
          ELSE DO:
             CREATE pa90fil.
             ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
             pa90fil.PLONTILLAGG = "010"
             pa90fil.PLONTILLANTAL = TIDREGITAB.TOTALT
             pa90fil.PDATUM = TIDREGITAB.DATUM
             pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
             pa90fil.AONR = TIDREGITAB.AONR
             pa90fil.DELNR = TIDREGITAB.DELNR
             pa90fil.PAVTAL = avtal
             pa90fil.BOLAG = bolag.
          END.   
       END.
       ELSE IF PERSONALTAB.BEFATTNING = "TIMANS.+SEM TJ" AND TIDREGITAB.TOTALT > 0 THEN DO:                                                                     
          IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
          ELSE DO:
             CREATE pa90fil. /*VILART 011*/
             ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR.
             IF globforetag = "NORD" OR globforetag = "ESMA" 
             OR globforetag = "ELPA" OR globforetag = "ESAN" THEN DO:
                ASSIGN pa90fil.PLONTILLAGG = "011". /* 040*/
             END.
             ELSE IF globforetag = "ETA" THEN DO:
                 ASSIGN pa90fil.PLONTILLAGG = "011".
             END. 
             ASSIGN
             pa90fil.PLONTILLANTAL = TIDREGITAB.TOTALT
             pa90fil.PDATUM = TIDREGITAB.DATUM
             pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
             pa90fil.AONR = TIDREGITAB.AONR
             pa90fil.DELNR = TIDREGITAB.DELNR
             pa90fil.PAVTAL = avtal
             pa90fil.BOLAG = bolag.
          END.   
       END.
       ELSE IF PERSONALTAB.BEFATTNING = "TIMANS.+SEM KOLL" AND TIDREGITAB.TOTALT > 0 THEN DO:                                                                     
          IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
          ELSE DO:
            CREATE pa90fil.  /* VILART 011*/
            ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR.
            IF globforetag = "NORD" OR globforetag = "ESMA" 
            OR globforetag = "ELPA" OR globforetag = "ESAN" THEN DO:
               ASSIGN pa90fil.PLONTILLAGG = "011". /*030*/
            END.
            ELSE IF globforetag = "ETA" THEN DO:
                  ASSIGN pa90fil.PLONTILLAGG = "011".
            END. 
            ASSIGN
            pa90fil.PLONTILLANTAL = TIDREGITAB.TOTALT
            pa90fil.PDATUM = TIDREGITAB.DATUM
            pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
            pa90fil.AONR = TIDREGITAB.AONR
            pa90fil.DELNR = TIDREGITAB.DELNR
            pa90fil.PAVTAL = avtal
            pa90fil.BOLAG = bolag.
         END.   
      END.       
      ELSE IF flex = TRUE THEN nytid = nytid. /*MÄLARDALEN koll skickar -flex och lart 100*/
      ELSE IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
      ELSE IF TIDREGITAB.PRISTYP = "RESTID..." THEN nytid = nytid.
      ELSE IF TIDREGITAB.LONTILLAGG NE "" THEN nytid = nytid.	  
      ELSE IF frvaro = 1 THEN ASSIGN frvaro = 0.  /*frånvaro*/
      /*ELSE IF TIDREGITAB.AONR BEGINS "910" THEN nytid = nytid.*/
      ELSE IF globforetag = "ETA" AND TIDREGITAB.AONR = "45" THEN nytid = nytid.
      ELSE IF ( TIDREGITAB.OKOD1 = " " AND TIDREGITAB.OKOD2 = " " AND
      TIDREGITAB.OKOD3 = " " ) AND TIDREGITAB.TOTALT > 0 THEN DO:
         regdatum = TIDREGITAB.DATUM.
         regvnr = TIDREGITAB.VECKONUMMER.
         RUN SLUTARB.P.
         IF TIDREGITAB.START < regstart OR  TIDREGITAB.START GE regslut
         THEN DO:
            CREATE pa90fil.  /* TIMMAR LÖNEART 100*/
            ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
            pa90fil.PLONTILLAGG = "100"
            pa90fil.PLONTILLANTAL = TIDREGITAB.TOTALT
            pa90fil.PDATUM = TIDREGITAB.DATUM
            pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
            pa90fil.AONR = TIDREGITAB.AONR
            pa90fil.DELNR = TIDREGITAB.DELNR
            pa90fil.PAVTAL = avtal
            pa90fil.BOLAG = bolag.
         END.
       END.          
    END.
    GET NEXT tq.
 END.
 GET NEXT pq.
END.
FOR EACH pa90fil WHERE pa90fil.PANSTNR = " ":
DELETE pa90fil.
END.
FOR EACH pa90fil where pa90fil.PLONTILLAGG NE "":
 FIND FIRST LONTILL WHERE LONTILL.LONTILLAGG = pa90fil.PLONTILLAGG
 AND LONTILL.KOD = pa90fil.PAVTAL USE-INDEX LON 
 NO-LOCK NO-ERROR.
 IF AVAILABLE LONTILL THEN ASSIGN pa90fil.PLONTILLAGG = LONTILL.VILART.
END.
FOR EACH pa90fil where pa90fil.POVERTIDTILL NE "":
 FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = pa90fil.POVERTIDTILL
 AND OVERKOD.KOD = pa90fil.PAVTAL USE-INDEX OVER 
 NO-LOCK NO-ERROR.
 IF AVAILABLE OVERKOD THEN ASSIGN pa90fil.POVERTIDTILL = OVERKOD.VILART.
END.
FOR EACH pa90fil where pa90fil.PBEREDSKAP NE "":
 FIND FIRST BERKOD WHERE BERKOD.BEREDSKAP = pa90fil.PBEREDSKAP
 AND BERKOD.BEREDSKAPSAVTAL = pa90fil.PAVTAL USE-INDEX BERE 
 NO-LOCK NO-ERROR.
 IF AVAILABLE BERKOD THEN ASSIGN pa90fil.PBEREDSKAP = BERKOD.VILART.
END.
FOR EACH pa90fil where pa90fil.PTRAKTKOD NE "":
 FIND FIRST TRAKTREGLER WHERE TRAKTREGLER.TRAKTKOD = pa90fil.PTRAKTKOD
 AND TRAKTREGLER.TRAAVTAL = pa90fil.PAVTAL USE-INDEX AVTAL NO-LOCK NO-ERROR.
 IF AVAILABLE TRAKTREGLER THEN DO:
     IF TRAKTREGLER.TRAKTANTAL = 0 THEN DO: /* TIMTRAKTAMENTE EVICOM*/      
        nytid = pa90fil.PTRAKTANTAL.
        RUN TIMSEK.P.
        pa90fil.PTRAKTANTAL =  sekunder / 3600.
     END.
 END.
END.
OUTPUT TO /u10/guru/export/esan/FIL0607.d.
FOR EACH PA90FIL:
   DISPLAY PA90FIL.

END.
RUN XFLFEL2.P.      /* summerar veckans skörd till tidigare */
