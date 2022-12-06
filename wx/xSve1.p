 /*ESVE1.P*/
DEFINE INPUT PARAMETER kordatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER koranv LIKE ANVANDARE.ANVANDARE NO-UNDO.
DEFINE INPUT PARAMETER perioden AS INTEGER FORMAT "999" NO-UNDO.

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
globanv = koranv.
vkdatum = kordatum.
tperiod = perioden.
FOR EACH pa90fil:
  DELETE pa90fil.
END.
OPEN QUERY pq FOR EACH PERSONALTAB WHERE PERSONALTAB.PERSMASK = TRUE
USE-INDEX PERSONALKOD NO-LOCK.
GET FIRST pq NO-LOCK.
DO WHILE AVAILABLE(PERSONALTAB):
   personal = PERSONALTAB.PERSONALKOD.
   avtal1 = PERSONALTAB.ANSTALLNING.
   persrec = RECID(PERSONALTAB).
   FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = avtal1
   USE-INDEX ANSTF NO-LOCK NO-ERROR.
   avtal = ANSTFORMTAB.KOD.
   /* LÄGG IN LÖNEFÖRETAG I OMRADETAB.TIMKOST */
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
   ASSIGN  bolag = STRING(OMRADETAB.TIMKOST).
   /*IF PERSONALTAB.OMRADE = "2501" THEN DO:
      ASSIGN bolag = "42".
      IF PERSONALTAB.ANSTNR = "68124" OR PERSONALTAB.ANSTNR = "68233" OR PERSONALTAB.ANSTNR = "68258"
      OR PERSONALTAB.ANSTNR = "68501" OR PERSONALTAB.ANSTNR = "68612" OR PERSONALTAB.ANSTNR = "68614"
      THEN ASSIGN bolag = "50".
   END.*/
   FIND FIRST UTRYCKNING WHERE UTRYCKNING.KOD = avtal USE-INDEX UT
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE UTRYCKNING THEN para = FALSE.
   ELSE para = UTRYCKNING.PARA8.
   FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD 
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.            
   OPEN QUERY tq FOR EACH TIDREGITAB WHERE
   TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.VECKOKORD = "w20010608" 
   USE-INDEX PSTART NO-LOCK. 
   
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
     ELSE DO :    
   	  IF TIDREGITAB.LONTILLAGG NE " " AND TIDREGITAB.LONTILLANTAL NE 0 THEN DO TRANSACTION:
   	     CREATE pa90fil.
   	     ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	     pa90fil.PLONTILLAGG = TIDREGITAB.LONTILLAGG
   	     pa90fil.PLONTILLANTAL = TIDREGITAB.LONTILLANTAL
   	     pa90fil.PDATUM = TIDREGITAB.DATUM
   	     pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	     pa90fil.AONR = TIDREGITAB.AONR
            pa90fil.DELNR = TIDREGITAB.DELNR
            pa90fil.PAVTAL = avtal
            pa90fil.BOLAG = bolag.
            /*IF pa90fil.PLONTILLAGG = "242" AND TIDREGITAB.AONR = "" THEN DO: /*motorvärmartillägg auto vid beredskap*/           
               IF PERSONALTAB.OMRADE = "2520" THEN ASSIGN pa90fil.AONR = "529915" pa90fil.DELNR = 000. /*2581*/
               IF PERSONALTAB.OMRADE = "2760" THEN ASSIGN pa90fil.AONR = "529916" pa90fil.DELNR = 000. /*2582*/        
               IF PERSONALTAB.OMRADE = "2840" THEN ASSIGN pa90fil.AONR = "529918" pa90fil.DELNR = 000. /*2585*/
               IF PERSONALTAB.OMRADE = "2750" THEN ASSIGN pa90fil.AONR = "529919" pa90fil.DELNR = 000. /*2587*/
               IF PERSONALTAB.OMRADE = "2801" THEN ASSIGN pa90fil.AONR = "529920" pa90fil.DELNR = 000. /*2800*/
               IF PERSONALTAB.OMRADE = "2810" THEN ASSIGN pa90fil.AONR = "531000" pa90fil.DELNR = 000. /*2801*/
               IF PERSONALTAB.OMRADE = "2820" THEN ASSIGN pa90fil.AONR = "532000" pa90fil.DELNR = 000. /*2802*/
               IF PERSONALTAB.OMRADE = "2830" THEN ASSIGN pa90fil.AONR = "533000" pa90fil.DELNR = 000. /*2803*/                
               IF PERSONALTAB.OMRADE = "402" THEN ASSIGN pa90fil.AONR = "529930" pa90fil.DELNR = 000. /*3802*/
               IF PERSONALTAB.OMRADE = "403" THEN ASSIGN pa90fil.AONR = "529934" pa90fil.DELNR = 000. /*3803*/
               IF PERSONALTAB.OMRADE = "501" THEN ASSIGN pa90fil.AONR = "529932" pa90fil.DELNR = 000. /*3805*/
            END.
            IF pa90fil.PLONTILLAGG = "413" AND TIDREGITAB.AONR = ""  THEN DO: 
               IF PERSONALTAB.OMRADE = "5901" THEN ASSIGN pa90fil.AONR = "890288" pa90fil.DELNR = 000. /*ETA ljus???*/
            END.*/               
        END.             
        IF TIDREGITAB.OANT1 NE 0 OR TIDREGITAB.OANT2 NE 0 OR TIDREGITAB.OANT3 NE 0
   	  THEN DO TRANSACTION:                                                      
   	     nytid = TIDREGITAB.OANT1.
   	     RUN TIMSEK.P.
   	     oa1 = sekunder.  
   	     nytid = TIDREGITAB.OANT2.
   	     RUN TIMSEK.P. 
   	     oa2 = sekunder.
   	     oversum = oa1 + sekunder.
   	     nytid = TIDREGITAB.OANT3.
   	     RUN TIMSEK.P.
   	     oa3 = sekunder.
   	     oversum = oversum + sekunder.
   	     nytid = TIDREGITAB.TOTALT.
   	     RUN TIMSEK.P.
   	     total = sekunder.  
   	     IF TIDREGITAB.OVERAUTO = FALSE OR TIDREGITAB.UTRYCKNING = FALSE OR
            oversum LE total THEN DO:                                               
   	        IF TIDREGITAB.OKOD1 NE "" THEN DO:
   	           CREATE pa90fil.
   	           ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	           pa90fil.POVERTIDTILL = TIDREGITAB.OKOD1
   	           pa90fil.POVERANTAL = TIDREGITAB.OANT1
   	           pa90fil.PDATUM = TIDREGITAB.DATUM
   	           pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	           pa90fil.AONR = TIDREGITAB.AONR
                 pa90fil.DELNR = TIDREGITAB.DELNR
                 pa90fil.PAVTAL = avtal
                 pa90fil.BOLAG = bolag.
   	        END.    
   	        IF TIDREGITAB.OKOD2 NE "" THEN DO:
   	           CREATE pa90fil.
   	           ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	           pa90fil.POVERTIDTILL = TIDREGITAB.OKOD2
   	           pa90fil.POVERANTAL = TIDREGITAB.OANT2
   	           pa90fil.PDATUM = TIDREGITAB.DATUM
   	           pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	           pa90fil.AONR = TIDREGITAB.AONR
   	           pa90fil.DELNR = TIDREGITAB.DELNR
   	           pa90fil.PAVTAL = avtal
   	           pa90fil.BOLAG = bolag.
   	        END.   
   	        IF TIDREGITAB.OKOD3 NE "" THEN DO:
   	           CREATE pa90fil.
   	           ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	           pa90fil.POVERTIDTILL = TIDREGITAB.OKOD3
   	           pa90fil.POVERANTAL = TIDREGITAB.OANT3
   	           pa90fil.PDATUM = TIDREGITAB.DATUM
   	           pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	           pa90fil.AONR = TIDREGITAB.AONR
   	           pa90fil.DELNR = TIDREGITAB.DELNR
   	           pa90fil.PAVTAL = avtal
   	           pa90fil.BOLAG = bolag.
   	        END.                
              /*utryckning 23-24 0-4  skall ej ge 2 tim luft som första ger*/
              IF TIDREGITAB.UTRYCKNING = TRUE AND oversum < TOTAL AND
              TIDREGITAB.START = 0 AND TIDREGITAB.OST1 > 0 AND TIDREGITAB.OANT1 >  0 THEN DO:        
                 FIND FIRST pa90fil WHERE pa90fil.PANSTNR = PERSONALTAB.ANSTNR AND
                 pa90fil.PDATUM = TIDREGITAB.DATUM - 1 AND pa90fil.POVERTIDTILL = "099" 
                 AND pa90fil.POVERANTAL = TIDREGITAB.OST1 NO-ERROR.
                 IF AVAILABLE pa90fil THEN DELETE pa90fil.                                      
              END.
   	        IF UTRYCKNING.LAGOVER = TRUE AND TIDREGITAB.NODF = TRUE THEN DO:  
   	        
   /*	VID NÖDFALL SKICKAS OCKSÅ 099 MED MINUSTIDEN
          UTRYCKNING.LAGOVER = TRUE FÖR DEM SOM SKICKER HELA ÖVERTIDEN + EXTRA LART 099 MED MINUSTID*/
          
     	           IF TIDREGITAB.OKOD1 NE "" THEN DO:
   	              CREATE pa90fil.
   	              ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	              pa90fil.POVERTIDTILL = "099"
   	              pa90fil.POVERANTAL = TIDREGITAB.OANT1
   	              pa90fil.PDATUM = TIDREGITAB.DATUM
   	              pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	              pa90fil.AONR = TIDREGITAB.AONR
                     pa90fil.DELNR = TIDREGITAB.DELNR
                     pa90fil.PAVTAL = avtal
                     pa90fil.BOLAG = bolag.
   	           END.    
   	           IF TIDREGITAB.OKOD2 NE "" THEN DO:
   	              CREATE pa90fil.
   	              ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	              pa90fil.POVERTIDTILL = "099"
   	              pa90fil.POVERANTAL = TIDREGITAB.OANT2
   	              pa90fil.PDATUM = TIDREGITAB.DATUM
   	              pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	              pa90fil.AONR = TIDREGITAB.AONR
   	              pa90fil.DELNR = TIDREGITAB.DELNR
   	              pa90fil.PAVTAL = avtal
   	              pa90fil.BOLAG = bolag.
   	           END.   
   	           IF TIDREGITAB.OKOD3 NE "" THEN DO:
   	              CREATE pa90fil.
   	              ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	              pa90fil.POVERTIDTILL = "099"
   	              pa90fil.POVERANTAL = TIDREGITAB.OANT3
   	              pa90fil.PDATUM = TIDREGITAB.DATUM
   	              pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	              pa90fil.AONR = TIDREGITAB.AONR
   	              pa90fil.DELNR = TIDREGITAB.DELNR
   	              pa90fil.PAVTAL = avtal
   	              pa90fil.BOLAG = bolag.
   	           END.
   	        END.   
   	     END.     
            ELSE IF oversum > total AND TIDREGITAB.OVERAUTO = TRUE
            AND TIDREGITAB.UTRYCKNING = TRUE THEN DO:                            
               IF UTRYCKNING.LAGOVER = FALSE THEN DO:  
                  /*GAMLA NORDKRAFT BÅKAB DELAR UPP ÖVERTID I VERKLIG TID OCH LUFTTID MHA NFALL*/
                  nod = FALSE.
   	           FIND FIRST NFALL WHERE NFALL.OBYT = TIDREGITAB.OKOD3 NO-LOCK NO-ERROR.
   	           IF AVAILABLE NFALL THEN nod = TRUE.  
   	           FIND FIRST NFALL WHERE NFALL.OBYT = TIDREGITAB.OKOD2 NO-LOCK NO-ERROR.
   	           IF AVAILABLE NFALL THEN nod = TRUE.
   	           FIND FIRST NFALL WHERE NFALL.OBYT = TIDREGITAB.OKOD1 NO-LOCK NO-ERROR.
   	           IF AVAILABLE NFALL THEN nod = TRUE.
   	           IF nod = TRUE THEN DO:
   	              IF OKOD1 NE "" THEN DO:
   	                 CREATE pa90fil.
   	                 ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	                 pa90fil.POVERTIDTILL = TIDREGITAB.OKOD1
   	                 pa90fil.POVERANTAL = TIDREGITAB.OANT1
                        pa90fil.PDATUM = TIDREGITAB.DATUM
                        pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
                        pa90fil.AONR = TIDREGITAB.AONR
   	                 pa90fil.DELNR = TIDREGITAB.DELNR
   	                 pa90fil.PAVTAL = avtal
   	                 pa90fil.BOLAG = bolag.
   	              END.    
   	              IF OKOD2 NE "" THEN DO:
   	                 CREATE pa90fil.
   	                 ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	                 pa90fil.POVERTIDTILL = TIDREGITAB.OKOD2
   	                 pa90fil.POVERANTAL = TIDREGITAB.OANT2
   	                 pa90fil.PDATUM = TIDREGITAB.DATUM
   	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                 pa90fil.AONR = TIDREGITAB.AONR
   	                 pa90fil.DELNR = TIDREGITAB.DELNR
   	                 pa90fil.PAVTAL = avtal
   	                 pa90fil.BOLAG = bolag.
   	              END.   
   	              IF OKOD3 NE "" THEN DO:
   	                 CREATE pa90fil.
   	                 ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	                 pa90fil.POVERTIDTILL = TIDREGITAB.OKOD3
   	                 pa90fil.POVERANTAL = TIDREGITAB.OANT3
   	                 pa90fil.PDATUM = TIDREGITAB.DATUM
   	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                 pa90fil.AONR = TIDREGITAB.AONR
    	                 pa90fil.DELNR = TIDREGITAB.DELNR
    	                 pa90fil.PAVTAL = avtal
    	                 pa90fil.BOLAG = bolag.
   	              END.             
   	           END.   
                 ELSE  DO:
   	              IF TIDREGITAB.OANT3 > 0 THEN DO:
   	                 oversum = oversum - oa3.
   	                 IF oversum > total THEN DO:
   	                    FIND FIRST NFALL WHERE NFALL.KOD = TIDREGITAB.OKOD3 NO-LOCK
   	                    NO-ERROR.  
   	                    IF NOT AVAILABLE NFALL THEN DO:
   	                       FIND FIRST NFALL WHERE NFALL.OBYT = TIDREGITAB.OKOD3 NO-LOCK
   	                       NO-ERROR.  
   	                    END.   
   	                    CREATE pa90fil.
   	                    ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	                    pa90fil.POVERTIDTILL = NFALL.OBYT
   	                    pa90fil.POVERANTAL = TIDREGITAB.OANT3
   	                    pa90fil.PDATUM = TIDREGITAB.DATUM
   	                    pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                    pa90fil.AONR = TIDREGITAB.AONR
                           pa90fil.DELNR = TIDREGITAB.DELNR
                           pa90fil.PAVTAL = avtal
                           pa90fil.BOLAG = bolag.
   	                 END.
   	                 ELSE DO:
   	                    t2 = total - oa1 - oa2. 
   	                    sekunder = t2.
   	                    RUN SEKTIM.P.	             
   	                    CREATE pa90fil.
   	                    ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	                    pa90fil.POVERTIDTILL = TIDREGITAB.OKOD3
   	                    pa90fil.POVERANTAL = nytid
   	                    pa90fil.PDATUM = TIDREGITAB.DATUM
   	                    pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                    pa90fil.AONR = TIDREGITAB.AONR
   	                    pa90fil.DELNR = TIDREGITAB.DELNR
   	                    pa90fil.PAVTAL = avtal
   	                    pa90fil.BOLAG = bolag.
   	                    t3 = oa3 - t2.
   	                    sekunder = t3.
   	                    RUN SEKTIM.P.
   	                    FIND FIRST NFALL WHERE NFALL.KOD = TIDREGITAB.OKOD3 NO-LOCK
   	                    NO-ERROR.
   	                    IF NOT AVAILABLE NFALL THEN DO:
   	                       FIND FIRST NFALL WHERE NFALL.OBYT = TIDREGITAB.OKOD3 NO-LOCK
   	                       NO-ERROR.  
   	                    END.      
   	                    CREATE pa90fil.
   	                    ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	                    pa90fil.POVERTIDTILL = NFALL.OBYT
   	                    pa90fil.POVERANTAL = nytid
   	                    pa90fil.PDATUM = TIDREGITAB.DATUM
   	                    pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                    pa90fil.AONR = TIDREGITAB.AONR
    	                    pa90fil.DELNR = TIDREGITAB.DELNR
    	                    pa90fil.PAVTAL = avtal
    	                    pa90fil.BOLAG = bolag.
   	                    bryt = TRUE.   
   	                 END.
   	              END.
   	              IF TIDREGITAB.OANT2 > 0 THEN DO:
   	                 oversum = oversum - oa2.
   	                 IF oversum > total THEN DO:
   	                    FIND FIRST NFALL WHERE NFALL.KOD = TIDREGITAB.OKOD2 NO-LOCK
   	                    NO-ERROR.   
   	                    IF NOT AVAILABLE NFALL THEN DO:
      	                       FIND FIRST NFALL WHERE NFALL.OBYT = TIDREGITAB.OKOD2 NO-LOCK
   	                       NO-ERROR.  
   	                    END.   
   	                    CREATE pa90fil.
   	                    ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	                    pa90fil.POVERTIDTILL = NFALL.OBYT
   	                    pa90fil.POVERANTAL = TIDREGITAB.OANT2
   	                    pa90fil.PDATUM = TIDREGITAB.DATUM
   	                    pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                    pa90fil.AONR = TIDREGITAB.AONR
   	                    pa90fil.DELNR = TIDREGITAB.DELNR
   	                    pa90fil.PAVTAL = avtal
   	                    pa90fil.BOLAG = bolag.
   	                 END.
   	                 ELSE IF bryt = TRUE THEN DO:
   	                    CREATE pa90fil.
   	                    ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	                    pa90fil.POVERTIDTILL = TIDREGITAB.OKOD2
   	                    pa90fil.POVERANTAL = TIDREGITAB.OANT2
   	                    pa90fil.PDATUM = TIDREGITAB.DATUM
   	                    pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                    pa90fil.AONR = TIDREGITAB.AONR
                          pa90fil.DELNR = TIDREGITAB.DELNR
                          pa90fil.PAVTAL = avtal
                          pa90fil.BOLAG = bolag.
   	                 END.                                      
   	                 ELSE DO:
   	                    t2 = total - oa1. 
   	                    sekunder = t2.
   	                    RUN SEKTIM.P.
   	                    CREATE pa90fil.
   	                    ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	                    pa90fil.POVERTIDTILL = TIDREGITAB.OKOD2
   	                    pa90fil.POVERANTAL = nytid
   	                    pa90fil.PDATUM = TIDREGITAB.DATUM
   	                    pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                    pa90fil.AONR = TIDREGITAB.AONR
   	                    pa90fil.DELNR = TIDREGITAB.DELNR
   	                    pa90fil.PAVTAL = avtal
   	                    pa90fil.BOLAG = bolag.
   	                    t3 = oa2 - t2.
   	                    sekunder = t3.
   	                    RUN SEKTIM.P.
   	                    FIND FIRST NFALL WHERE NFALL.KOD = TIDREGITAB.OKOD2 NO-LOCK
   	                    NO-ERROR. 
   	                    IF NOT AVAILABLE NFALL THEN DO:
   	                       FIND FIRST NFALL WHERE NFALL.OBYT = TIDREGITAB.OKOD2 NO-LOCK
   	                       NO-ERROR.  
   	                    END.     
   	                    CREATE pa90fil.
   	                    ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR   
        	                pa90fil.POVERTIDTILL = NFALL.OBYT
   	                    pa90fil.POVERANTAL = nytid
   	                    pa90fil.PDATUM = TIDREGITAB.DATUM
   	                    pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                    pa90fil.AONR = TIDREGITAB.AONR
   	                    pa90fil.DELNR = TIDREGITAB.DELNR
   	                    pa90fil.PAVTAL = avtal
   	                    pa90fil.BOLAG = bolag.
   	                    bryt = TRUE.
   	                 END.
   	              END.   
   	              IF TIDREGITAB.OANT1 > 0 THEN DO:
   	                 IF bryt = TRUE THEN DO:
   	                    CREATE pa90fil.
   	                    ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	                    pa90fil.POVERTIDTILL = TIDREGITAB.OKOD1
   	                    pa90fil.POVERANTAL = TIDREGITAB.OANT1
      	                 pa90fil.PDATUM = TIDREGITAB.DATUM
   	                    pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                    pa90fil.AONR = TIDREGITAB.AONR
   	                    pa90fil.DELNR = TIDREGITAB.DELNR
   	                    pa90fil.PAVTAL = avtal
   	                    pa90fil.BOLAG = bolag.
   	                 END .                                      
   	                 ELSE DO:
   	                    t3 = oa1 - total.
   	                    sekunder = t3.
   	                    RUN SEKTIM.P.
   	                    CREATE pa90fil.
   	                    ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	                    pa90fil.POVERTIDTILL = TIDREGITAB.OKOD1
   	                    pa90fil.POVERANTAL = TIDREGITAB.TOTALT
   	                    pa90fil.PDATUM = TIDREGITAB.DATUM  
   	                    pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                    pa90fil.AONR = TIDREGITAB.AONR
    	                    pa90fil.DELNR = TIDREGITAB.DELNR
    	                    pa90fil.PAVTAL = avtal
    	                    pa90fil.BOLAG = bolag.
   	                    FIND FIRST NFALL WHERE NFALL.KOD = TIDREGITAB.OKOD1 NO-LOCK
   	                    NO-ERROR.   
   	                    IF NOT AVAILABLE NFALL THEN DO:
   	                       FIND FIRST NFALL WHERE NFALL.OBYT = TIDREGITAB.OKOD1 NO-LOCK
   	                       NO-ERROR.  
   	                    END.   
   	                    CREATE pa90fil.
   	                    ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	                    pa90fil.POVERTIDTILL = NFALL.OBYT
   	                    pa90fil.POVERANTAL = nytid
   	                    pa90fil.PDATUM = TIDREGITAB.DATUM
   	                    pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                    pa90fil.AONR = TIDREGITAB.AONR
       	                pa90fil.DELNR = TIDREGITAB.DELNR
       	                pa90fil.PAVTAL = avtal
       	                pa90fil.BOLAG = bolag.
   	                 END.
   	              END.                                     
   	              bryt = FALSE.
   	           END.
   	        END.
              ELSE DO:   /* UTRYCKNING.LAGOVER = TRUE FÖR DEM SOM SKICKER HELA ÖVERTIDEN + EXTRA LART 099 MED MINUSTID*/
                 IF TIDREGITAB.NODF = TRUE THEN DO:                  
                     IF TIDREGITAB.OKOD1 NE "" THEN DO:
   	                  CREATE pa90fil.
   	                  ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	                  pa90fil.POVERTIDTILL = TIDREGITAB.OKOD1
                        pa90fil.POVERANTAL = TIDREGITAB.OANT1
   	                  pa90fil.PDATUM = TIDREGITAB.DATUM
   	                  pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                  pa90fil.AONR = TIDREGITAB.AONR
                        pa90fil.DELNR = TIDREGITAB.DELNR
                        pa90fil.PAVTAL = avtal
                        pa90fil.BOLAG = bolag.
                     END.
   	               IF TIDREGITAB.OKOD2 NE "" THEN DO:
   	                 CREATE pa90fil.
   	                 ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	                 pa90fil.POVERTIDTILL = TIDREGITAB.OKOD2
   	                 pa90fil.POVERANTAL = TIDREGITAB.OANT2
   	                 pa90fil.PDATUM = TIDREGITAB.DATUM
   	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                 pa90fil.AONR = TIDREGITAB.AONR
   	                 pa90fil.DELNR = TIDREGITAB.DELNR
   	                 pa90fil.PAVTAL = avtal
   	                 pa90fil.BOLAG = bolag.
   	              END.   
   	              IF TIDREGITAB.OKOD3 NE "" THEN DO:
   	                 CREATE pa90fil.
   	                 ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	                 pa90fil.POVERTIDTILL = TIDREGITAB.OKOD3
   	                 pa90fil.POVERANTAL = TIDREGITAB.OANT3
   	                 pa90fil.PDATUM = TIDREGITAB.DATUM
   	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                 pa90fil.AONR = TIDREGITAB.AONR
   	                 pa90fil.DELNR = TIDREGITAB.DELNR
   	                 pa90fil.PAVTAL = avtal
   	                 pa90fil.BOLAG = bolag.
   	              END.                                  
   	              	        
       /*	VID NÖDFALL SKICKAS OCKSÅ 099 MED MINUSTIDEN
          UTRYCKNING.LAGOVER = TRUE FÖR DEM SOM SKICKER HELA ÖVERTIDEN + EXTRA LART 099 MED MINUSTID*/        
     	         IF TIDREGITAB.OKOD1 NE "" THEN DO:
   	                 CREATE pa90fil.
   	                 ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	                 pa90fil.POVERTIDTILL = "099"
   	                 pa90fil.POVERANTAL = TIDREGITAB.OANT1
   	                 pa90fil.PDATUM = TIDREGITAB.DATUM
   	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                 pa90fil.AONR = TIDREGITAB.AONR
                        pa90fil.DELNR = TIDREGITAB.DELNR
                        pa90fil.PAVTAL = avtal
                        pa90fil.BOLAG = bolag.
   	              END.    
   	              IF TIDREGITAB.OKOD2 NE "" THEN DO:
   	                 CREATE pa90fil.
   	                 ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	                 pa90fil.POVERTIDTILL = "099"
   	                 pa90fil.POVERANTAL = TIDREGITAB.OANT2
   	                 pa90fil.PDATUM = TIDREGITAB.DATUM
   	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                 pa90fil.AONR = TIDREGITAB.AONR
   	                 pa90fil.DELNR = TIDREGITAB.DELNR
   	                 pa90fil.PAVTAL = avtal
   	                 pa90fil.BOLAG = bolag.
   	              END.   
   	              IF TIDREGITAB.OKOD3 NE "" THEN DO:
   	                 CREATE pa90fil.
   	                 ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	                 pa90fil.POVERTIDTILL = "099"
   	                 pa90fil.POVERANTAL = TIDREGITAB.OANT3
   	                 pa90fil.PDATUM = TIDREGITAB.DATUM
   	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                 pa90fil.AONR = TIDREGITAB.AONR
   	                 pa90fil.DELNR = TIDREGITAB.DELNR
   	                 pa90fil.PAVTAL = avtal
   	                 pa90fil.BOLAG = bolag.
   	              END.	                                               
                  END.
                  ELSE DO:
                      nod = FALSE.
       	           IF TIDREGITAB.OANT3 > 0 THEN DO:
       	              oversum = oversum - oa3.
       	              IF oversum > total THEN DO:	  
                            CREATE pa90fil.
       	                 ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
       	                 pa90fil.POVERTIDTILL = TIDREGITAB.OKOD3
       	                 pa90fil.POVERANTAL = TIDREGITAB.OANT3
       	                 pa90fil.PDATUM = TIDREGITAB.DATUM
       	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
       	                 pa90fil.AONR = TIDREGITAB.AONR
                            pa90fil.DELNR = TIDREGITAB.DELNR
                            pa90fil.PAVTAL = avtal
                            pa90fil.BOLAG = bolag.
       	                 CREATE pa90fil.
       	                 ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
       	                 pa90fil.POVERTIDTILL = "099"
       	                 pa90fil.POVERANTAL = TIDREGITAB.OANT3
       	                 pa90fil.PDATUM = TIDREGITAB.DATUM
       	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
       	                 pa90fil.AONR = TIDREGITAB.AONR
                            pa90fil.DELNR = TIDREGITAB.DELNR
                            pa90fil.PAVTAL = avtal
                            pa90fil.BOLAG = bolag.
       	              END.
       	              ELSE DO:
       	                 t2 = total - oa1 - oa2. 
       	                 sekunder = t2.
       	                 RUN SEKTIM.P.	             
       	                 CREATE pa90fil.
       	                 ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
       	                 pa90fil.POVERTIDTILL = TIDREGITAB.OKOD3
       	                 pa90fil.POVERANTAL = TIDREGITAB.OANT3
       	                 pa90fil.PDATUM = TIDREGITAB.DATUM
       	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
       	                 pa90fil.AONR = TIDREGITAB.AONR
       	                 pa90fil.DELNR = TIDREGITAB.DELNR
       	                 pa90fil.PAVTAL = avtal
       	                 pa90fil.BOLAG = bolag.
       	                 t3 = oa3 - t2.
       	                 sekunder = t3.
       	                 RUN SEKTIM.P.	                      
       	                 CREATE pa90fil.
       	                 ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
       	                 pa90fil.POVERTIDTILL = "099"
       	                 pa90fil.POVERANTAL = nytid
       	                 pa90fil.PDATUM = TIDREGITAB.DATUM
       	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
       	                 pa90fil.AONR = TIDREGITAB.AONR
        	                 pa90fil.DELNR = TIDREGITAB.DELNR
        	                 pa90fil.PAVTAL = avtal
        	                 pa90fil.BOLAG = bolag.
       	                 bryt = TRUE.   
       	              END.
       	           END.
       	           IF TIDREGITAB.OANT2 > 0 THEN DO:
       	              oversum = oversum - oa2.
       	              IF oversum > total THEN DO:
       	                 CREATE pa90fil.
       	                 ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
       	                 pa90fil.POVERTIDTILL = TIDREGITAB.OKOD2
       	                 pa90fil.POVERANTAL = TIDREGITAB.OANT2
       	                 pa90fil.PDATUM = TIDREGITAB.DATUM
       	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
       	                 pa90fil.AONR = TIDREGITAB.AONR
       	                 pa90fil.DELNR = TIDREGITAB.DELNR
       	                 pa90fil.PAVTAL = avtal
       	                 pa90fil.BOLAG = bolag.
       	                 CREATE pa90fil.
       	                 ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
       	                 pa90fil.POVERTIDTILL = "099"
       	                 pa90fil.POVERANTAL = TIDREGITAB.OANT2
       	                 pa90fil.PDATUM = TIDREGITAB.DATUM
       	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
       	                 pa90fil.AONR = TIDREGITAB.AONR
       	                 pa90fil.DELNR = TIDREGITAB.DELNR
       	                 pa90fil.PAVTAL = avtal
       	                 pa90fil.BOLAG = bolag.
       	              END.
       	              ELSE IF bryt = TRUE THEN DO:
       	                 CREATE pa90fil.
       	                 ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
       	                 pa90fil.POVERTIDTILL = TIDREGITAB.OKOD2
       	                 pa90fil.POVERANTAL = TIDREGITAB.OANT2
       	                 pa90fil.PDATUM = TIDREGITAB.DATUM
       	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
       	                 pa90fil.AONR = TIDREGITAB.AONR
                            pa90fil.DELNR = TIDREGITAB.DELNR
                            pa90fil.PAVTAL = avtal
                            pa90fil.BOLAG = bolag.
       	              END.                                      
       	              ELSE DO:
       	                 t2 = total - oa1. 
       	                 sekunder = t2.
       	                 RUN SEKTIM.P.
       	                 CREATE pa90fil.
       	                 ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
       	                 pa90fil.POVERTIDTILL = TIDREGITAB.OKOD2
       	                 pa90fil.POVERANTAL = TIDREGITAB.OANT2
       	                 pa90fil.PDATUM = TIDREGITAB.DATUM
       	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
       	                 pa90fil.AONR = TIDREGITAB.AONR
       	                 pa90fil.DELNR = TIDREGITAB.DELNR
       	                 pa90fil.PAVTAL = avtal
       	                 pa90fil.BOLAG = bolag.
       	                 t3 = oa2 - t2.
       	                 sekunder = t3.
       	                 RUN SEKTIM.P.	                    
       	                 CREATE pa90fil.
       	                 ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR   
                          pa90fil.POVERTIDTILL = "099"
       	                 pa90fil.POVERANTAL = nytid
       	                 pa90fil.PDATUM = TIDREGITAB.DATUM
       	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
       	                 pa90fil.AONR = TIDREGITAB.AONR
       	                 pa90fil.DELNR = TIDREGITAB.DELNR
       	                 pa90fil.PAVTAL = avtal
       	                 pa90fil.BOLAG = bolag.
       	                 bryt = TRUE.
       	              END.
       	           END.   
       	           IF TIDREGITAB.OANT1 > 0 THEN DO:
       	              IF bryt = TRUE THEN DO:
       	                 CREATE pa90fil.
       	                 ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
       	                 pa90fil.POVERTIDTILL = TIDREGITAB.OKOD1
       	                 pa90fil.POVERANTAL = TIDREGITAB.OANT1
          	              pa90fil.PDATUM = TIDREGITAB.DATUM
       	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
       	                 pa90fil.AONR = TIDREGITAB.AONR
       	                 pa90fil.DELNR = TIDREGITAB.DELNR
       	                 pa90fil.PAVTAL = avtal
       	                 pa90fil.BOLAG = bolag.
       	              END .                                      
       	              ELSE DO:
       	                 t3 = oa1 - total.
       	                 sekunder = t3.
       	                 RUN SEKTIM.P.
       	                 CREATE pa90fil.
       	                 ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
       	                 pa90fil.POVERTIDTILL = TIDREGITAB.OKOD1
       	                 pa90fil.POVERANTAL = TIDREGITAB.OANT1
       	                 pa90fil.PDATUM = TIDREGITAB.DATUM  
       	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
       	                 pa90fil.AONR = TIDREGITAB.AONR
        	                 pa90fil.DELNR = TIDREGITAB.DELNR
        	                 pa90fil.PAVTAL = avtal
        	                 pa90fil.BOLAG = bolag.
       	                 CREATE pa90fil.
       	                 ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
       	                 pa90fil.POVERTIDTILL = "099"
       	                 pa90fil.POVERANTAL = nytid
       	                 pa90fil.PDATUM = TIDREGITAB.DATUM
       	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
       	                 pa90fil.AONR = TIDREGITAB.AONR
           	             pa90fil.DELNR = TIDREGITAB.DELNR
           	             pa90fil.PAVTAL = avtal
           	             pa90fil.BOLAG = bolag.
       	              END.
                     END.                                     
   	              bryt = FALSE.
                  END.                  
   	         END.
   	      END.                                
       END.
       IF TIDREGITAB.OANT1 = 0 AND TIDREGITAB.OANT2 = 0 AND TIDREGITAB.OANT3 = 0
       AND TIDREGITAB.START = 0 AND TIDREGITAB.SLUT > 0 THEN DO TRANSACTION:                                                      
          FIND FIRST pa90fil WHERE pa90fil.PANSTNR = PERSONALTAB.ANSTNR AND
          pa90fil.PDATUM = TIDREGITAB.DATUM - 1 AND pa90fil.POVERTIDTILL = "099" AND
          pa90fil.POVERANTAL GE TIDREGITAB.TOTALT NO-ERROR.
          IF AVAILABLE pa90fil THEN DO:
             nytid = TIDREGITAB.TOTALT.
             RUN TIMSEK.P.
             seku = sekunder.
             nytid = pa90fil.POVERANTAL. 
             RUN TIMSEK.P.
             sekunder = sekunder - seku.
             RUN SEKTIM.P.
             pa90fil.POVERANTAL = nytid.
          END.             
       END.   
       IF TIDREGITAB.TRAKTKOD NE " " AND TIDREGITAB.TRAKTANTAL NE 0
   	   THEN DO TRANSACTION:
   	      frvaro = 0.
            FIND FIRST FVARO WHERE FVARO.AONR = TIDREGITAB.AONR AND FVARO.DELNR = TIDREGITAB.DELNR
            NO-LOCK NO-ERROR.
            IF AVAILABLE FVARO THEN ASSIGN frvaro = 1.
   	     IF globforetag = "ESAN" AND frvaro = 1 THEN frvaro = 0.
   	     ELSE IF globforetag = "ESMA"  AND frvaro = 1 THEN frvaro = 0.
   	     ELSE DO:
   	        CREATE pa90fil.
   	        ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	        pa90fil.PTRAKTKOD = TIDREGITAB.TRAKTKOD
   	        pa90fil.PTRAKTANTAL = TIDREGITAB.TRAKTANTAL
   	        pa90fil.PDATUM = TIDREGITAB.DATUM
   	        pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	        pa90fil.AONR = TIDREGITAB.AONR
               pa90fil.DELNR = TIDREGITAB.DELNR
               pa90fil.PAVTAL = PERSONALTAB.TRAAVTAL
               pa90fil.BOLAG = bolag.
            END.   
   	   END.
   	   IF TIDREGITAB.BEREDSKAP NE " " AND TIDREGITAB.BERANTAL NE 0
   	   THEN DO TRANSACTION:	     	      
   	     CREATE pa90fil.
   	     ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	     pa90fil.PBEREDSKAP = TIDREGITAB.BEREDSKAP
   	     pa90fil.PBERANTAL = TIDREGITAB.BERANTAL
   	     pa90fil.PDATUM = TIDREGITAB.DATUM
   	     pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	     pa90fil.AONR = TIDREGITAB.AONR
            pa90fil.DELNR = TIDREGITAB.DELNR	 
            pa90fil.PAVTAL = PERSONALTAB.BEREDSKAPSAVTAL
            pa90fil.BOLAG = bolag.
          /*IF TIDREGITAB.AONR = "" THEN DO:    
               IF PERSONALTAB.OMRADE = "2520" THEN ASSIGN pa90fil.AONR = "529915" pa90fil.DELNR = 000. /*2581*/
               IF PERSONALTAB.OMRADE = "2760" THEN ASSIGN pa90fil.AONR = "529916" pa90fil.DELNR = 000. /*2582*/         
               IF PERSONALTAB.OMRADE = "2840" THEN ASSIGN pa90fil.AONR = "529918" pa90fil.DELNR = 000. /*2585*/
               IF PERSONALTAB.OMRADE = "2750" THEN ASSIGN pa90fil.AONR = "529919" pa90fil.DELNR = 000. /*2587*/
               IF PERSONALTAB.OMRADE = "2801" THEN ASSIGN pa90fil.AONR = "529920" pa90fil.DELNR = 000. /*2800*/
               IF PERSONALTAB.OMRADE = "2810" THEN ASSIGN pa90fil.AONR = "531000" pa90fil.DELNR = 000. /*2801*/
               IF PERSONALTAB.OMRADE = "2820" THEN ASSIGN pa90fil.AONR = "532000" pa90fil.DELNR = 000. /*2802*/
               IF PERSONALTAB.OMRADE = "2830" THEN ASSIGN pa90fil.AONR = "533000" pa90fil.DELNR = 000. /*2803*/                     
               IF PERSONALTAB.OMRADE = "402" THEN ASSIGN pa90fil.AONR = "529930" pa90fil.DELNR = 000. /*3802*/
               IF PERSONALTAB.OMRADE = "403" THEN ASSIGN pa90fil.AONR = "529934" pa90fil.DELNR = 000. /*3803*/
               IF PERSONALTAB.OMRADE = "501" THEN ASSIGN pa90fil.AONR = "529932" pa90fil.DELNR = 000. /*3805*/
               IF PERSONALTAB.OMRADE = "5720" OR PERSONALTAB.OMRADE = "5750" THEN DO:
                  /* 50% 481002 00
                   20% 481003 00
                  15% 481006 00
                  10% 481004 00
                  5%  481005 00*/ 
                  ASSIGN pa90fil.AONR = "481002" pa90fil.DELNR = 000. /*ETA*/
                  nytid = TIDREGITAB.BERANTAL.
                  RUN TIMSEK.P.
                  ASSIGN
                  seku = sekunder
                  sekunder = seku * 0.50.
                  RUN SEKTIM.P.
                  ASSIGN pa90fil.PBERANTAL = nytid.
                  sekunder = seku * 0.20.
                  RUN SEKTIM.P.
                  CREATE pa90fil.
                  ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	            pa90fil.PBEREDSKAP = TIDREGITAB.BEREDSKAP
   	            pa90fil.PBERANTAL = nytid
   	            pa90fil.PDATUM = TIDREGITAB.DATUM
   	            pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER	 
                  pa90fil.PAVTAL = PERSONALTAB.BEREDSKAPSAVTAL
                  pa90fil.AONR = "481003" pa90fil.DELNR = 000
                  pa90fil.BOLAG = bolag.
                  sekunder = seku * 0.15.
                  RUN SEKTIM.P.
                  CREATE pa90fil.
                  ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	            pa90fil.PBEREDSKAP = TIDREGITAB.BEREDSKAP
   	            pa90fil.PBERANTAL = nytid
   	            pa90fil.PDATUM = TIDREGITAB.DATUM
   	            pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER	 
                  pa90fil.PAVTAL = PERSONALTAB.BEREDSKAPSAVTAL
                  pa90fil.AONR = "481006" pa90fil.DELNR = 000
                  pa90fil.BOLAG = bolag.
                  sekunder = seku * 0.10.
                  RUN SEKTIM.P.
                  CREATE pa90fil.
                  ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	            pa90fil.PBEREDSKAP = TIDREGITAB.BEREDSKAP
   	            pa90fil.PBERANTAL = nytid
   	            pa90fil.PDATUM = TIDREGITAB.DATUM
   	            pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER	 
                  pa90fil.PAVTAL = PERSONALTAB.BEREDSKAPSAVTAL
                  pa90fil.AONR = "481004" pa90fil.DELNR = 000
                  pa90fil.BOLAG = bolag.
                  sekunder = seku * 0.05.
                  RUN SEKTIM.P.
                  CREATE pa90fil.
                  ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	            p90fil.PBEREDSKAP = TIDREGITAB.BEREDSKAP
   	            pa90fil.PBERANTAL = nytid
   	            pa90fil.PDATUM = TIDREGITAB.DATUM
   	            pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER	 
                  pa90fil.PAVTAL = PERSONALTAB.BEREDSKAPSAVTAL
                  pa90fil.AONR = "481005" pa90fil.DELNR = 000
                  pa90fil.BOLAG = bolag.
               END.    
               IF PERSONALTAB.OMRADE = "5730" THEN DO:
                  /* 80% 485000 00
                     20% 485001 00*/ 
                  ASSIGN pa90fil.AONR = "485000" pa90fil.DELNR = 000. /*ETA*/
                  nytid = TIDREGITAB.BERANTAL.
                  RUN TIMSEK.P.
                  ASSIGN
                  seku = sekunder
                  sekunder = seku * 0.80.
                  RUN SEKTIM.P.
                  ASSIGN pa90fil.PBERANTAL = nytid.
                  sekunder = seku * 0.20.
                  RUN SEKTIM.P.
                  CREATE pa90fil.
                  ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
   	            pa90fil.PBEREDSKAP = TIDREGITAB.BEREDSKAP
   	            pa90fil.PBERANTAL = nytid
   	            pa90fil.PDATUM = TIDREGITAB.DATUM
   	            pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER	 
                  pa90fil.PAVTAL = PERSONALTAB.BEREDSKAPSAVTAL
                  pa90fil.AONR = "485001" pa90fil.DELNR = 000
                  pa90fil.BOLAG = bolag.
               END. 
               IF PERSONALTAB.OMRADE = "5740" THEN ASSIGN pa90fil.AONR = "489023" pa90fil.DELNR = 000. /*ETA*/
               IF PERSONALTAB.OMRADE = "5901" THEN ASSIGN pa90fil.AONR = "890288" pa90fil.DELNR = 000. /*ETA ljus???*/
               IF PERSONALTAB.OMRADE = "5725" THEN ASSIGN pa90fil.AONR = "481037" pa90fil.DELNR = 000. /*ETA HELSINGBORG*/
    /*         IF PERSONALTAB.OMRADE = "5760" THEN ASSIGN pa90fil.AONR = "529929" pa90fil.DELNR = 000. /*ETA OSLO?? */*/
            END.                 */
   	   END.                    
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
FOR EACH pa90fil where pa90fil.PTRAKTKOD NE "":
   FIND FIRST TRAKTATAB WHERE TRAKTATAB.TRAKTKOD = pa90fil.PTRAKTKOD
   AND TRAKTATAB.TRAAVTAL = pa90fil.PAVTAL USE-INDEX TRAAV 
   NO-LOCK NO-ERROR.
   IF AVAILABLE TRAKTATAB THEN ASSIGN pa90fil.PTRAKTKOD = TRAKTATAB.VILART.
END. 

if globforetag = "elpa" then DO:
   OUTPUT TO D:\delad\pro8\GURU\apptemp\pa90fil.d.
   for each pa90fil BY pa90fil.PANSTNR BY pa90fil.PLONTILLAGG:
     display pa90fil.
   end.  
   OUTPUT TO D:\delad\pro8\GURU\apptemp\flexen.d.
   for each flexen BY flexen.PKOD:
     display flexen.
   end.
END.
arrflex = 0.
FOR EACH flexen BREAK BY flexen.PKOD:    
   ACCUMULATE flexen.PLONTILLANTAL(TOTAL BY flexen.PKOD).       
   IF LAST-OF(flexen.PKOD) THEN DO:
      CREATE flexsum.                    
      ASSIGN  
      flexsum.PKOD =  flexen.PKOD
      sekunder = (ACCUM TOTAL flexen.PLONTILLANTAL) - arrflex.
      RUN FSEKTIM.P.
      ASSIGN
      flexsum.PLONTILLSEK = sekunder
      flexsum.PLONTILLANTAL = fnytid.
      arrflex = ACCUM TOTAL flexen.PLONTILLANTAL.             
   END.
END.


GET FIRST pq NO-LOCK.
DO WHILE AVAILABLE(PERSONALTAB):
   FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD 
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   IF AVAILABLE FLEXAVT AND FLEXAVT.FLEXTID = TRUE THEN DO:
      FIND FIRST flexsum WHERE flexsum.PKOD = PERSONALTAB.PERSONALKOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE flexsum THEN DO TRANSACTION:
         CREATE FSALDMAN.
         ASSIGN
         FSALDMAN.PERSONALKOD = PERSONALTAB.PERSONALKOD
         FSALDMAN.PERIODFLEX = 0
         FSALDMAN.DATUM = vkdatum.
         /* ACCFLEX = ingående saldo PERIODFLEX = sista körningens flex
            BACFLEX = FÖRRA ACCFLEX */            
         FIND FIRST FLEXSALDO WHERE FLEXSALDO.PERSONALKOD = PERSONALTAB.PERSONALKOD 
         EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE FLEXSALDO THEN DO:
            CREATE FLEXSALDO.
            ASSIGN FLEXSALDO.PERSONALKOD = PERSONALTAB.PERSONALKOD.
         END.   
         ASSIGN   
         FLEXSALDO.BACFLEX = FLEXSALDO.ACCFLEX 
         nytid = FLEXSALDO.ACCFLEX.
         RUN TIMSEK.P.
         seku = sekunder.
         nytid = FLEXSALDO.PERIODFLEX.
         RUN TIMSEK.P.
         sekunder = sekunder + seku.
         RUN FSEKTIM.P.
         ASSIGN 
         FLEXSALDO.ACCFLEX = fnytid
         FLEXSALDO.PERIODFLEX = 0.
      END.   
      ELSE DO TRANSACTION:
         CREATE FSALDMAN.
         ASSIGN
         FSALDMAN.PERSONALKOD = flexsum.PKOD
         FSALDMAN.PERIODFLEX = flexsum.PLONTILLANTAL
         FSALDMAN.DATUM = vkdatum.
         /* ACCFLEX = ingående saldo PERIODFLEX = sista körningens flex
            BACFLEX = FÖRRA ACCFLEX */            
         FIND FIRST FLEXSALDO WHERE FLEXSALDO.PERSONALKOD = flexsum.PKOD 
         EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE FLEXSALDO THEN DO:
            CREATE FLEXSALDO.
            ASSIGN FLEXSALDO.PERSONALKOD = flexsum.PKOD.
         END.   
         ASSIGN   
         FLEXSALDO.BACFLEX = FLEXSALDO.ACCFLEX 
         nytid = FLEXSALDO.ACCFLEX.
         RUN TIMSEK.P.
         seku = sekunder.
         nytid = FLEXSALDO.PERIODFLEX.
         RUN TIMSEK.P.
         sekunder = sekunder + seku.
         RUN FSEKTIM.P.
         ASSIGN 
         FLEXSALDO.ACCFLEX = fnytid
         FLEXSALDO.PERIODFLEX = flexsum.PLONTILLANTAL.
      END.   
   END.   
   GET NEXT pq NO-LOCK.
END.   
   
      

/*FOR EACH flexsum:
   DO TRANSACTION:
      CREATE FSALDMAN.
      ASSIGN
      FSALDMAN.PERSONALKOD = flexsum.PKOD
      FSALDMAN.PERIODFLEX = flexsum.PLONTILLANTAL
      FSALDMAN.DATUM = vkdatum.
      /* ACCFLEX = ingående saldo PERIODFLEX = sista körningens flex
         BACFLEX = FÖRRA ACCFLEX */            
      FIND FIRST FLEXSALDO WHERE FLEXSALDO.PERSONALKOD = flexsum.PKOD 
      EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE FLEXSALDO THEN DO:
         CREATE FLEXSALDO.
         ASSIGN FLEXSALDO.PERSONALKOD = flexsum.PKOD.
      END.   
      ASSIGN   
      FLEXSALDO.BACFLEX = FLEXSALDO.ACCFLEX 
      nytid = FLEXSALDO.ACCFLEX.
      RUN TIMSEK.P.
      seku = sekunder.
      nytid = FLEXSALDO.PERIODFLEX.
      RUN TIMSEK.P.
      sekunder = sekunder + seku.
      RUN FSEKTIM.P.
      ASSIGN 
      FLEXSALDO.ACCFLEX = fnytid
      FLEXSALDO.PERIODFLEX = flexsum.PLONTILLANTAL.
   END.   
END.  */
if globforetag = "elpa" then DO:
   OUTPUT TO D:\delad\pro8\GURU\apptemp\flexsum.d.
   for each flexsum BY flexsum.PKOD:
     display flexsum.
   end.
END.


RUN ESVE3.P.      /* summerar veckans skörd till tidigare */
