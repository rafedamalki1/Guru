 /*ESFE1.P*/
/*FELRATTNINGAR*/
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

DEFINE NEW SHARED TEMP-TABLE pa90fel
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
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
globanv = koranv.
vkdatum = kordatum.
tperiod = perioden.
FOR EACH pa90fel:
  DELETE pa90fel.
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
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
   ASSIGN  bolag = STRING(OMRADETAB.TIMKOST).
   
   FIND FIRST UTRYCKNING WHERE UTRYCKNING.KOD = avtal USE-INDEX UT
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE UTRYCKNING THEN para = FALSE.
   ELSE para = UTRYCKNING.PARA8.
   FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD 
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.            
   OPEN QUERY tq FOR EACH TIDFEL WHERE
   TIDFEL.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDFEL.SKICKA = TRUE AND TIDFEL.FELKORD = "w20010608" NO-LOCK. 
   
   GET FIRST tq NO-LOCK.
   DO WHILE AVAILABLE(TIDFEL):
      IF (TIDFEL.TRAKTKOD = " "  OR TIDFEL.TRAKTANTAL = 0) AND
      (TIDFEL.LONTILLAGG = " "  OR TIDFEL.LONTILLANTAL = 0) AND
      (TIDFEL.OKOD1 = " " OR TIDFEL.OANT1 = 0) AND
      (TIDFEL.OKOD2 = " " OR TIDFEL.OANT2 = 0) AND
      (TIDFEL.OKOD3 = " " OR TIDFEL.OANT3 = 0) AND
      (TIDFEL.BEREDSKAP = " " OR TIDFEL.BERANTAL = 0)
      THEN DO:         
        /* om flextid skall räknas ut*/ 
   	  flex = FALSE.
        IF AVAILABLE FLEXAVT AND FLEXAVT.FLEXTID = TRUE THEN DO:
   	     regdatum = TIDFEL.DATUM.
   	     regvnr = TIDFEL.VECKONUMMER.
   	     RUN SLUTARB.P.
   	     musz = FALSE.
   	     IF bolag = "45" OR bolag = "49" OR bolag = "51" THEN DO:
   	        IF avtal BEGINS "K" AND TIDFEL.OVERTIDUTTAG = "F" AND
   	        TIDFEL.LONTILLAGG = "" THEN musz = TRUE.
           END.
           IF musz = TRUE THEN DO:
               musz = FALSE.
               IF TIDFEL.START < regstart THEN DO:                                                                     
                  nytid = TIDFEL.TOTALT.
                  RUN TIMSEK.P.
                  fltid = fltid + sekunder.                       
               END.
               ELSE IF TIDFEL.START GE regslut THEN DO:                                                             
                  ASSIGN                    
                  nytid = TIDFEL.TOTALT.
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
                     /* *minus?*/
                     sekunder = (-1) * fltid.
                     RUN FSEKTIM.P.           
                     CREATE pa90fel.
                     ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
                     pa90fel.PLONTILLAGG = "100"
                     pa90fel.PLONTILLANTAL = fnytid
                     pa90fel.PSORT = "TI"
                     pa90fel.PDATUM = TIDFEL.DATUM
                     pa90fel.AONR = "1097" + SUBSTRING(STRING(OMRADETAB.ORGIDNUM,"9999"),1,2)
                     pa90fel.DELNR = INTEGER(SUBSTRING(STRING(OMRADETAB.ORGIDNUM,"9999"),3,2))
                     pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
                     pa90fel.PAVTAL = avtal
                     pa90fel.BOLAG = bolag.
                     IF TIDFEL.DEBET = FALSE THEN DO:
                        ASSIGN pa90fel.PLONTILLANTAL = (-1) * pa90fel.PLONTILLANTAL.
                     END.   
                     fltid =0.         
                  END.   
               END.                                   
           END.    
           ELSE IF TIDFEL.OVERTIDUTTAG = "F" AND TIDFEL.LONTILLAGG = "" THEN DO:
               IF TIDFEL.AONR = "910927" OR TIDFEL.AONR = "27" THEN fltid = fltid.
               ELSE IF regstart = regslut THEN DO:
                  nytid = TIDFEL.TOTALT.
                  RUN TIMSEK.P.
                  fltid = fltid + sekunder.      
               END.   
               ELSE IF TIDFEL.START < regstart THEN DO:                                                                     
                  nytid = TIDFEL.TOTALT.
                  RUN TIMSEK.P.
                  fltid = fltid + sekunder.                       
               END.
               ELSE IF TIDFEL.START GE regslut THEN DO:                                                             
                  ASSIGN                    
                  nytid = TIDFEL.TOTALT.
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
               CREATE pa90fel.
               ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
               pa90fel.PLONTILLAGG = flkod
               pa90fel.PLONTILLANTAL = fnytid
               pa90fel.PSORT = "TI"
               pa90fel.PDATUM = TIDFEL.DATUM
               pa90fel.AONR = TIDFEL.AONR
               pa90fel.DELNR = TIDFEL.DELNR
               pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
               pa90fel.PAVTAL = avtal
               pa90fel.BOLAG = bolag.
               IF TIDFEL.DEBET = FALSE THEN DO:
                  ASSIGN pa90fel.PLONTILLANTAL = (-1) * pa90fel.PLONTILLANTAL.
               END.
               fltid =0. 
               flex = TRUE.
           END.   
        END.   	     
        frvaro = 0.
        FIND FIRST FVARO WHERE FVARO.AONR = TIDFEL.AONR AND FVARO.DELNR = TIDFEL.DELNR
        NO-LOCK NO-ERROR.
        IF AVAILABLE FVARO THEN ASSIGN frvaro = 1.
        IF PERSONALTAB.BEFATTNING = "TIMANSTÄLLD" AND TIDFEL.TOTALT > 0 THEN DO:                                                                     
            IF TIDFEL.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
            ELSE DO:	     
               CREATE pa90fel.
               ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
               pa90fel.PLONTILLAGG = "010"
               pa90fel.PLONTILLANTAL = TIDFEL.TOTALT
               pa90fel.PDATUM = TIDFEL.DATUM
               pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
               pa90fel.AONR = TIDFEL.AONR
               pa90fel.DELNR = TIDFEL.DELNR
               pa90fel.PAVTAL = avtal
               pa90fel.BOLAG = bolag.
               IF TIDFEL.DEBET = FALSE THEN DO:
                  ASSIGN pa90fel.PLONTILLANTAL = (-1) * pa90fel.PLONTILLANTAL.
               END.
            END.   
        END.
        ELSE IF PERSONALTAB.BEFATTNING = "TIMANS.+SEM TJ" AND TIDFEL.TOTALT > 0 THEN DO:                                                                     
   	      IF TIDFEL.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
   	      ELSE DO:	        
   	        CREATE pa90fel.  /* VILART = 011*/
   	        ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR.
   	        IF globforetag = "NORD" OR globforetag = "ESMA" 
   	        OR globforetag = "ELPA" OR globforetag = "ESAN" THEN DO:
   	           ASSIGN pa90fel.PLONTILLAGG = "011". /*040*/
   	        END.
   	        ELSE IF globforetag = "ETA" THEN DO:
       	           ASSIGN pa90fel.PLONTILLAGG = "011".
   	        END. 
   	        ASSIGN   
   	        pa90fel.PLONTILLANTAL = TIDFEL.TOTALT
   	        pa90fel.PDATUM = TIDFEL.DATUM
   	        pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	        pa90fel.AONR = TIDFEL.AONR
   	        pa90fel.DELNR = TIDFEL.DELNR
   	        pa90fel.PAVTAL = avtal
   	        pa90fel.BOLAG = bolag.
   	        IF TIDFEL.DEBET = FALSE THEN DO:
                 ASSIGN pa90fel.PLONTILLANTAL = (-1) * pa90fel.PLONTILLANTAL.
              END.
   	      END.   
   	  END.
        ELSE IF PERSONALTAB.BEFATTNING = "TIMANS.+SEM KOLL" AND TIDFEL.TOTALT > 0 THEN DO:                                                                     
   	     IF TIDFEL.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
   	     ELSE DO:
   	        CREATE pa90fel.  /* VILART = 011*/
   	        ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR.
   	        IF globforetag = "NORD" OR globforetag = "ESMA" 
   	        OR globforetag = "ELPA" OR globforetag = "ESAN" THEN DO:
   	           ASSIGN pa90fel.PLONTILLAGG = "011". /*030*/
   	        END.
   	        ELSE IF globforetag = "ETA" THEN DO:
       	        ASSIGN pa90fel.PLONTILLAGG = "011".
   	        END. 
   	        ASSIGN
   	        pa90fel.PLONTILLANTAL = TIDFEL.TOTALT
   	        pa90fel.PDATUM = TIDFEL.DATUM
   	        pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	        pa90fel.AONR = TIDFEL.AONR
   	        pa90fel.DELNR = TIDFEL.DELNR
   	        pa90fel.PAVTAL = avtal
   	        pa90fel.BOLAG = bolag.
   	        IF TIDFEL.DEBET = FALSE THEN DO:
                 ASSIGN pa90fel.PLONTILLANTAL = (-1) * pa90fel.PLONTILLANTAL.
              END.
   	     END.   
   	  END.
   	  ELSE IF flex = TRUE THEN nytid = nytid.
   	  ELSE IF TIDFEL.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
   	  ELSE IF TIDFEL.PRISTYP = "RESTID..."  THEN nytid = nytid.
   	  ELSE IF TIDFEL.LONTILLAGG NE "" THEN nytid = nytid.
   	  ELSE IF frvaro = 1 THEN ASSIGN frvaro = 0.  /*frånvaro*/
   	  /*ELSE IF TIDFEL.AONR BEGINS "910" THEN nytid = nytid.*/
   	  ELSE IF globforetag = "ETA" AND TIDFEL.AONR = "45" THEN nytid = nytid.
   	  ELSE IF ( TIDFEL.OKOD1 = " " AND TIDFEL.OKOD2 = " " AND
        TIDFEL.OKOD3 = " " ) AND TIDFEL.TOTALT > 0 THEN DO:
           CREATE pa90fel.  /* TIMMAR LÖNEART 100*/
   	     ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	     pa90fel.PLONTILLAGG = "100"
   	     pa90fel.PLONTILLANTAL = TIDFEL.TOTALT
   	     pa90fel.PDATUM = TIDFEL.DATUM
   	     pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	     pa90fel.AONR = TIDFEL.AONR
   	     pa90fel.DELNR = TIDFEL.DELNR
   	     pa90fel.PAVTAL = avtal
   	     pa90fel.BOLAG = bolag.
   	     IF TIDFEL.DEBET = FALSE THEN DO:
              ASSIGN pa90fel.PLONTILLANTAL = (-1) * pa90fel.PLONTILLANTAL.
           END.
   	  END.            
      END.
      ELSE DO :    
   	  IF TIDFEL.LONTILLAGG NE " " AND TIDFEL.LONTILLANTAL NE 0 THEN DO TRANSACTION:
   	     CREATE pa90fel.
   	     ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	     pa90fel.PLONTILLAGG = TIDFEL.LONTILLAGG
   	     pa90fel.PLONTILLANTAL = TIDFEL.LONTILLANTAL
   	     pa90fel.PDATUM = TIDFEL.DATUM
   	     pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	     pa90fel.AONR = TIDFEL.AONR
           pa90fel.DELNR = TIDFEL.DELNR
           pa90fel.PAVTAL = avtal
           pa90fel.BOLAG = bolag.
           IF TIDFEL.DEBET = FALSE THEN DO:
              ASSIGN pa90fel.PLONTILLANTAL = (-1) * pa90fel.PLONTILLANTAL.
           END.        
       /*    IF pa90fel.PLONTILLAGG = "413" AND TIDFEL.AONR = ""  THEN DO: 
              IF PERSONALTAB.OMRADE = "5901" THEN ASSIGN pa90fel.AONR = "890288" pa90fel.DELNR = 000. /*ETA ljus???*/
           END.               */
        END.             
        IF TIDFEL.OANT1 NE 0 OR TIDFEL.OANT2 NE 0 OR TIDFEL.OANT3 NE 0
   	  THEN DO TRANSACTION:                                                      
   	     nytid = TIDFEL.OANT1.
   	     RUN TIMSEK.P.
   	     oa1 = sekunder.  
   	     nytid = TIDFEL.OANT2.
   	     RUN TIMSEK.P. 
   	     oa2 = sekunder.
   	     oversum = oa1 + sekunder.
   	     nytid = TIDFEL.OANT3.
   	     RUN TIMSEK.P.
   	     oa3 = sekunder.
   	     oversum = oversum + sekunder.
   	     nytid = TIDFEL.TOTALT.
   	     RUN TIMSEK.P.
   	     total = sekunder.  
   	     IF TIDFEL.OVERAUTO = FALSE OR TIDFEL.UTRYCKNING = FALSE OR
           oversum LE total THEN DO:                                               
   	        IF OKOD1 NE "" THEN DO:
   	           CREATE pa90fel.
   	           ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	           pa90fel.POVERTIDTILL = TIDFEL.OKOD1
   	           pa90fel.POVERANTAL = TIDFEL.OANT1
   	           pa90fel.PDATUM = TIDFEL.DATUM
   	           pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	           pa90fel.AONR = TIDFEL.AONR
                 pa90fel.DELNR = TIDFEL.DELNR
                 pa90fel.PAVTAL = avtal
                 pa90fel.BOLAG = bolag.
                 IF TIDFEL.DEBET = FALSE THEN DO:
                    ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                 END.
   	        END.    
   	        IF OKOD2 NE "" THEN DO:
   	           CREATE pa90fel.
   	           ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	           pa90fel.POVERTIDTILL = TIDFEL.OKOD2
   	           pa90fel.POVERANTAL = TIDFEL.OANT2
   	           pa90fel.PDATUM = TIDFEL.DATUM
   	           pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	           pa90fel.AONR = TIDFEL.AONR
   	           pa90fel.DELNR = TIDFEL.DELNR
   	           pa90fel.PAVTAL = avtal
   	           pa90fel.BOLAG = bolag.
   	           IF TIDFEL.DEBET = FALSE THEN DO:
                    ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                 END.
   	        END.   
   	        IF OKOD3 NE "" THEN DO:
   	           CREATE pa90fel.
   	           ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	           pa90fel.POVERTIDTILL = TIDFEL.OKOD3
   	           pa90fel.POVERANTAL = TIDFEL.OANT3
   	           pa90fel.PDATUM = TIDFEL.DATUM
   	           pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	           pa90fel.AONR = TIDFEL.AONR
   	           pa90fel.DELNR = TIDFEL.DELNR
   	           pa90fel.PAVTAL = avtal
   	           pa90fel.BOLAG = bolag.
   	           IF TIDFEL.DEBET = FALSE THEN DO:
                    ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                 END.
   	        END.                                    
   	     END.     
           ELSE IF oversum > total AND TIDFEL.OVERAUTO = TRUE
           AND TIDFEL.UTRYCKNING = TRUE THEN DO:                            
              IF UTRYCKNING.LAGOVER = FALSE THEN DO:  
                 /*GAMLA NORDKRAFT BÅKAB DELAR UPP ÖVERTID I VERKLIG TID OCH LUFTTID MHA NFALL*/
                 nod = FALSE.
   	           FIND FIRST NFALL WHERE NFALL.OBYT = TIDFEL.OKOD3 NO-LOCK NO-ERROR.
   	           IF AVAILABLE NFALL THEN nod = TRUE.  
   	           FIND FIRST NFALL WHERE NFALL.OBYT = TIDFEL.OKOD2 NO-LOCK NO-ERROR.
   	           IF AVAILABLE NFALL THEN nod = TRUE.
   	           FIND FIRST NFALL WHERE NFALL.OBYT = TIDFEL.OKOD1 NO-LOCK NO-ERROR.
   	           IF AVAILABLE NFALL THEN nod = TRUE.
   	           IF nod = TRUE THEN DO:
   	              IF OKOD1 NE "" THEN DO:
   	                 CREATE pa90fel.
   	                 ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                 pa90fel.POVERTIDTILL = TIDFEL.OKOD1
   	                 pa90fel.POVERANTAL = TIDFEL.OANT1
                       pa90fel.PDATUM = TIDFEL.DATUM
                       pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
                       pa90fel.AONR = TIDFEL.AONR
   	                 pa90fel.DELNR = TIDFEL.DELNR
   	                 pa90fel.PAVTAL = avtal
   	                 pa90fel.BOLAG = bolag.
   	                 IF TIDFEL.DEBET = FALSE THEN DO:
                          ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                       END.
   	              END.    
   	              IF OKOD2 NE "" THEN DO:
   	                 CREATE pa90fel.
   	                 ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                 pa90fel.POVERTIDTILL = TIDFEL.OKOD2
   	                 pa90fel.POVERANTAL = TIDFEL.OANT2
   	                 pa90fel.PDATUM = TIDFEL.DATUM
   	                 pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                 pa90fel.AONR = TIDFEL.AONR
   	                 pa90fel.DELNR = TIDFEL.DELNR
   	                 pa90fel.PAVTAL = avtal
   	                 pa90fel.BOLAG = bolag.
   	                 IF TIDFEL.DEBET = FALSE THEN DO:
                          ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                       END.
   	              END.   
   	              IF OKOD3 NE "" THEN DO:
   	                 CREATE pa90fel.
   	                 ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                 pa90fel.POVERTIDTILL = TIDFEL.OKOD3
   	                 pa90fel.POVERANTAL = TIDFEL.OANT3
   	                 pa90fel.PDATUM = TIDFEL.DATUM
   	                 pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                 pa90fel.AONR = TIDFEL.AONR
    	                 pa90fel.DELNR = TIDFEL.DELNR
    	                 pa90fel.PAVTAL = avtal
    	                 pa90fel.BOLAG = bolag.
    	                 IF TIDFEL.DEBET = FALSE THEN DO:
                          ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                       END.
   	              END.             
   	           END.   
                 ELSE  DO:
   	              IF TIDFEL.OANT3 > 0 THEN DO:
   	                 oversum = oversum - oa3.
   	                 IF oversum > total THEN DO:
   	                    FIND FIRST NFALL WHERE NFALL.KOD = TIDFEL.OKOD3 NO-LOCK
   	                    NO-ERROR.  
   	                    IF NOT AVAILABLE NFALL THEN DO:
   	                       FIND FIRST NFALL WHERE NFALL.OBYT = TIDFEL.OKOD3 NO-LOCK
   	                       NO-ERROR.  
   	                    END.   
   	                    CREATE pa90fel.
   	                    ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                    pa90fel.POVERTIDTILL = NFALL.OBYT
   	                    pa90fel.POVERANTAL = TIDFEL.OANT3
   	                    pa90fel.PDATUM = TIDFEL.DATUM
   	                    pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                    pa90fel.AONR = TIDFEL.AONR
                          pa90fel.DELNR = TIDFEL.DELNR
                          pa90fel.PAVTAL = avtal
                          pa90fel.BOLAG = bolag.
                          IF TIDFEL.DEBET = FALSE THEN DO:
                             ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                          END.
   	                 END.
   	                 ELSE DO:
   	                    t2 = total - oa1 - oa2. 
   	                    sekunder = t2.
   	                    RUN SEKTIM.P.	             
   	                    CREATE pa90fel.
   	                    ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                    pa90fel.POVERTIDTILL = TIDFEL.OKOD3
   	                    pa90fel.POVERANTAL = nytid
   	                    pa90fel.PDATUM = TIDFEL.DATUM
   	                    pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                    pa90fel.AONR = TIDFEL.AONR
   	                    pa90fel.DELNR = TIDFEL.DELNR
   	                    pa90fel.PAVTAL = avtal
   	                    pa90fel.BOLAG = bolag.
   	                    IF TIDFEL.DEBET = FALSE THEN DO:
                             ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                          END.
   	                    t3 = oa3 - t2.
   	                    sekunder = t3.
   	                    RUN SEKTIM.P.
   	                    FIND FIRST NFALL WHERE NFALL.KOD = TIDFEL.OKOD3 NO-LOCK
   	                    NO-ERROR.
   	                    IF NOT AVAILABLE NFALL THEN DO:
   	                       FIND FIRST NFALL WHERE NFALL.OBYT = TIDFEL.OKOD3 NO-LOCK
   	                       NO-ERROR.  
   	                    END.      
   	                    CREATE pa90fel.
   	                    ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                    pa90fel.POVERTIDTILL = NFALL.OBYT
   	                    pa90fel.POVERANTAL = nytid
   	                    pa90fel.PDATUM = TIDFEL.DATUM
   	                    pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                    pa90fel.AONR = TIDFEL.AONR
    	                    pa90fel.DELNR = TIDFEL.DELNR
    	                    pa90fel.PAVTAL = avtal
    	                    pa90fel.BOLAG = bolag.
    	                    IF TIDFEL.DEBET = FALSE THEN DO:
                             ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                          END.
   	                    bryt = TRUE.  	                     
   	                 END.
   	              END.
   	              IF TIDFEL.OANT2 > 0 THEN DO:
   	                 oversum = oversum - oa2.
   	                 IF oversum > total THEN DO:
   	                    FIND FIRST NFALL WHERE NFALL.KOD = TIDFEL.OKOD2 NO-LOCK
   	                    NO-ERROR.   
   	                    IF NOT AVAILABLE NFALL THEN DO:
      	                    FIND FIRST NFALL WHERE NFALL.OBYT = TIDFEL.OKOD2 NO-LOCK
   	                       NO-ERROR.  
   	                    END.   
   	                    CREATE pa90fel.
   	                    ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                    pa90fel.POVERTIDTILL = NFALL.OBYT
   	                    pa90fel.POVERANTAL = TIDFEL.OANT2
   	                    pa90fel.PDATUM = TIDFEL.DATUM
   	                    pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                    pa90fel.AONR = TIDFEL.AONR
   	                    pa90fel.DELNR = TIDFEL.DELNR
   	                    pa90fel.PAVTAL = avtal
   	                    pa90fel.BOLAG = bolag.
   	                    IF TIDFEL.DEBET = FALSE THEN DO:
                             ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                          END.
   	                 END.
   	                 ELSE IF bryt = TRUE THEN DO:
   	                    CREATE pa90fel.
   	                    ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                    pa90fel.POVERTIDTILL = TIDFEL.OKOD2
   	                    pa90fel.POVERANTAL = TIDFEL.OANT2
   	                    pa90fel.PDATUM = TIDFEL.DATUM
   	                    pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                    pa90fel.AONR = TIDFEL.AONR
                          pa90fel.DELNR = TIDFEL.DELNR
                          pa90fel.PAVTAL = avtal
                          pa90fel.BOLAG = bolag.
                          IF TIDFEL.DEBET = FALSE THEN DO:
                             ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                          END.
   	                 END.                                      
   	                 ELSE DO:
   	                    t2 = total - oa1. 
   	                    sekunder = t2.
   	                    RUN SEKTIM.P.
   	                    CREATE pa90fel.
   	                    ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                    pa90fel.POVERTIDTILL = TIDFEL.OKOD2
   	                    pa90fel.POVERANTAL = nytid
   	                    pa90fel.PDATUM = TIDFEL.DATUM
   	                    pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                    pa90fel.AONR = TIDFEL.AONR
   	                    pa90fel.DELNR = TIDFEL.DELNR
   	                    pa90fel.PAVTAL = avtal
   	                    pa90fel.BOLAG = bolag.
   	                    IF TIDFEL.DEBET = FALSE THEN DO:
                             ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                          END.
   	                    t3 = oa2 - t2.
   	                    sekunder = t3.
   	                    RUN SEKTIM.P.
   	                    FIND FIRST NFALL WHERE NFALL.KOD = TIDFEL.OKOD2 NO-LOCK
   	                    NO-ERROR. 
   	                    IF NOT AVAILABLE NFALL THEN DO:
   	                       FIND FIRST NFALL WHERE NFALL.OBYT = TIDFEL.OKOD2 NO-LOCK
   	                       NO-ERROR.  
   	                    END.     
   	                    CREATE pa90fel.
   	                    ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR   
        	                 pa90fel.POVERTIDTILL = NFALL.OBYT
   	                    pa90fel.POVERANTAL = nytid
   	                    pa90fel.PDATUM = TIDFEL.DATUM
   	                    pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                    pa90fel.AONR = TIDFEL.AONR
   	                    pa90fel.DELNR = TIDFEL.DELNR
   	                    pa90fel.PAVTAL = avtal
   	                    pa90fel.BOLAG = bolag.
   	                    IF TIDFEL.DEBET = FALSE THEN DO:
                             ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                          END.
   	                    bryt = TRUE.
   	                 END.
   	              END.   
   	              IF TIDFEL.OANT1 > 0 THEN DO:
   	                 IF bryt = TRUE THEN DO:
   	                    CREATE pa90fel.
   	                    ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                    pa90fel.POVERTIDTILL = TIDFEL.OKOD1
   	                    pa90fel.POVERANTAL = TIDFEL.OANT1
      	                 pa90fel.PDATUM = TIDFEL.DATUM
   	                    pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                    pa90fel.AONR = TIDFEL.AONR
   	                    pa90fel.DELNR = TIDFEL.DELNR
   	                    pa90fel.PAVTAL = avtal
   	                    pa90fel.BOLAG = bolag.
   	                    IF TIDFEL.DEBET = FALSE THEN DO:
                             ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                          END.
   	                 END .                                      
   	                 ELSE DO:
   	                    t3 = oa1 - total.
   	                    sekunder = t3.
   	                    RUN SEKTIM.P.
   	                    CREATE pa90fel.
   	                    ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                    pa90fel.POVERTIDTILL = TIDFEL.OKOD1
   	                    pa90fel.POVERANTAL = TIDFEL.TOTALT
   	                    pa90fel.PDATUM = TIDFEL.DATUM  
   	                    pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                    pa90fel.AONR = TIDFEL.AONR
    	                    pa90fel.DELNR = TIDFEL.DELNR
    	                    pa90fel.PAVTAL = avtal
    	                    pa90fel.BOLAG = bolag.
    	                    IF TIDFEL.DEBET = FALSE THEN DO:
                             ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                          END.
   	                    FIND FIRST NFALL WHERE NFALL.KOD = TIDFEL.OKOD1 NO-LOCK
   	                    NO-ERROR.   
   	                    IF NOT AVAILABLE NFALL THEN DO:
   	                       FIND FIRST NFALL WHERE NFALL.OBYT = TIDFEL.OKOD1 NO-LOCK
   	                       NO-ERROR.  
   	                    END.   
   	                    CREATE pa90fel.
   	                    ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                    pa90fel.POVERTIDTILL = NFALL.OBYT
   	                    pa90fel.POVERANTAL = nytid
   	                    pa90fel.PDATUM = TIDFEL.DATUM
   	                    pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                    pa90fel.AONR = TIDFEL.AONR
       	                 pa90fel.DELNR = TIDFEL.DELNR
       	                 pa90fel.PAVTAL = avtal
       	                 pa90fel.BOLAG = bolag.
       	                 IF TIDFEL.DEBET = FALSE THEN DO:
                              ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                          END.
   	                 END.
   	              END.                                     
   	              bryt = FALSE.
                 END.
   	        END.
              ELSE DO:   /* UTRYCKNING.LAGOVER = TRUE FÖR DEM SOM SKICKER HELA ÖVERTIDEN + EXTRA LART 099 MED MINUSTID*/                 
                 nod = FALSE.
   	           IF TIDFEL.OANT3 > 0 THEN DO:
   	              oversum = oversum - oa3.
   	              IF oversum > total THEN DO:	  
                       CREATE pa90fel.
   	                 ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                 pa90fel.POVERTIDTILL = TIDFEL.OKOD3
   	                 pa90fel.POVERANTAL = TIDFEL.OANT3
   	                 pa90fel.PDATUM = TIDFEL.DATUM
   	                 pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                 pa90fel.AONR = TIDFEL.AONR
                       pa90fel.DELNR = TIDFEL.DELNR
                       pa90fel.PAVTAL = avtal
                       pa90fel.BOLAG = bolag.
                       IF TIDFEL.DEBET = FALSE THEN DO:
                          ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                       END.
   	                 CREATE pa90fel.
   	                 ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                 pa90fel.POVERTIDTILL = "099"
   	                 pa90fel.POVERANTAL = TIDFEL.OANT3
   	                 pa90fel.PDATUM = TIDFEL.DATUM
   	                 pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                 pa90fel.AONR = TIDFEL.AONR
                       pa90fel.DELNR = TIDFEL.DELNR
                       pa90fel.PAVTAL = avtal
                       pa90fel.BOLAG = bolag.
                       IF TIDFEL.DEBET = FALSE THEN DO:
                          ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                       END.
   	              END.
   	              ELSE DO:
   	                 t2 = total - oa1 - oa2. 
   	                 sekunder = t2.
   	                 RUN SEKTIM.P.	             
   	                 CREATE pa90fel.
   	                 ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                 pa90fel.POVERTIDTILL = TIDFEL.OKOD3
   	                 pa90fel.POVERANTAL = TIDFEL.OANT3
   	                 pa90fel.PDATUM = TIDFEL.DATUM
   	                 pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                 pa90fel.AONR = TIDFEL.AONR
   	                 pa90fel.DELNR = TIDFEL.DELNR
   	                 pa90fel.PAVTAL = avtal
   	                 pa90fel.BOLAG = bolag.
   	                 IF TIDFEL.DEBET = FALSE THEN DO:
                          ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                       END.
   	                 t3 = oa3 - t2.
   	                 sekunder = t3.
   	                 RUN SEKTIM.P.	                      
   	                 CREATE pa90fel.
   	                 ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                 pa90fel.POVERTIDTILL = "099"
   	                 pa90fel.POVERANTAL = nytid
   	                 pa90fel.PDATUM = TIDFEL.DATUM
   	                 pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                 pa90fel.AONR = TIDFEL.AONR
    	                 pa90fel.DELNR = TIDFEL.DELNR
    	                 pa90fel.PAVTAL = avtal
    	                 pa90fel.BOLAG = bolag.
    	                 IF TIDFEL.DEBET = FALSE THEN DO:
                          ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                       END.
   	                 bryt = TRUE.   
   	              END.
   	           END.
   	           IF TIDFEL.OANT2 > 0 THEN DO:
   	              oversum = oversum - oa2.
   	              IF oversum > total THEN DO:
   	                 CREATE pa90fel.
   	                 ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                 pa90fel.POVERTIDTILL = TIDFEL.OKOD2
   	                 pa90fel.POVERANTAL = TIDFEL.OANT2
   	                 pa90fel.PDATUM = TIDFEL.DATUM
   	                 pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                 pa90fel.AONR = TIDFEL.AONR
   	                 pa90fel.DELNR = TIDFEL.DELNR
   	                 pa90fel.PAVTAL = avtal
   	                 pa90fel.BOLAG = bolag.
   	                 IF TIDFEL.DEBET = FALSE THEN DO:
                          ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                       END.
   	                 CREATE pa90fel.
   	                 ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                 pa90fel.POVERTIDTILL = "099"
   	                 pa90fel.POVERANTAL = TIDFEL.OANT2
   	                 pa90fel.PDATUM = TIDFEL.DATUM
   	                 pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                 pa90fel.AONR = TIDFEL.AONR
   	                 pa90fel.DELNR = TIDFEL.DELNR
   	                 pa90fel.PAVTAL = avtal
   	                 pa90fel.BOLAG = bolag.
   	                 IF TIDFEL.DEBET = FALSE THEN DO:
                          ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                       END.
   	              END.
   	              ELSE IF bryt = TRUE THEN DO:
   	                 CREATE pa90fel.
   	                 ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                 pa90fel.POVERTIDTILL = TIDFEL.OKOD2
   	                 pa90fel.POVERANTAL = TIDFEL.OANT2
   	                 pa90fel.PDATUM = TIDFEL.DATUM
   	                 pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                 pa90fel.AONR = TIDFEL.AONR
                       pa90fel.DELNR = TIDFEL.DELNR
                       pa90fel.PAVTAL = avtal
                       pa90fel.BOLAG = bolag.
                       IF TIDFEL.DEBET = FALSE THEN DO:
                          ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                       END.
   	              END.                                      
   	              ELSE DO:
   	                 t2 = total - oa1. 
   	                 sekunder = t2.
   	                 RUN SEKTIM.P.
   	                 CREATE pa90fel.
   	                 ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                 pa90fel.POVERTIDTILL = TIDFEL.OKOD2
   	                 pa90fel.POVERANTAL = TIDFEL.OANT2
   	                 pa90fel.PDATUM = TIDFEL.DATUM
   	                 pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                 pa90fel.AONR = TIDFEL.AONR
   	                 pa90fel.DELNR = TIDFEL.DELNR
   	                 pa90fel.PAVTAL = avtal
   	                 pa90fel.BOLAG = bolag.
   	                 IF TIDFEL.DEBET = FALSE THEN DO:
                          ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                       END.
   	                 t3 = oa2 - t2.
   	                 sekunder = t3.
   	                 RUN SEKTIM.P.	                    
   	                 CREATE pa90fel.
   	                 ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR   
        	              pa90fel.POVERTIDTILL = "099"
   	                 pa90fel.POVERANTAL = nytid
   	                 pa90fel.PDATUM = TIDFEL.DATUM
   	                 pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                 pa90fel.AONR = TIDFEL.AONR
   	                 pa90fel.DELNR = TIDFEL.DELNR
   	                 pa90fel.PAVTAL = avtal
   	                 pa90fel.BOLAG = bolag.
   	                 IF TIDFEL.DEBET = FALSE THEN DO:
                          ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                       END.
   	                 bryt = TRUE.
   	              END.
   	           END.   
   	           IF TIDFEL.OANT1 > 0 THEN DO:
   	              IF bryt = TRUE THEN DO:
   	                 CREATE pa90fel.
   	                 ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                 pa90fel.POVERTIDTILL = TIDFEL.OKOD1
   	                 pa90fel.POVERANTAL = TIDFEL.OANT1
      	              pa90fel.PDATUM = TIDFEL.DATUM
   	                 pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                 pa90fel.AONR = TIDFEL.AONR
   	                 pa90fel.DELNR = TIDFEL.DELNR
   	                 pa90fel.PAVTAL = avtal
   	                 pa90fel.BOLAG = bolag.
   	                 IF TIDFEL.DEBET = FALSE THEN DO:
                          ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                       END.
   	              END .                                      
   	              ELSE DO:
   	                 t3 = oa1 - total.
   	                 sekunder = t3.
   	                 RUN SEKTIM.P.
   	                 CREATE pa90fel.
   	                 ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                 pa90fel.POVERTIDTILL = TIDFEL.OKOD1
   	                 pa90fel.POVERANTAL = TIDFEL.OANT1
   	                 pa90fel.PDATUM = TIDFEL.DATUM  
   	                 pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                 pa90fel.AONR = TIDFEL.AONR
    	                 pa90fel.DELNR = TIDFEL.DELNR
    	                 pa90fel.PAVTAL = avtal
    	                 pa90fel.BOLAG = bolag.
    	                 IF TIDFEL.DEBET = FALSE THEN DO:
                          ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                       END.
   	                 CREATE pa90fel.
   	                 ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	                 pa90fel.POVERTIDTILL = "099"
   	                 pa90fel.POVERANTAL = nytid
   	                 pa90fel.PDATUM = TIDFEL.DATUM
   	                 pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	                 pa90fel.AONR = TIDFEL.AONR
       	              pa90fel.DELNR = TIDFEL.DELNR
       	              pa90fel.PAVTAL = avtal
       	              pa90fel.BOLAG = bolag.
       	              IF TIDFEL.DEBET = FALSE THEN DO:
                          ASSIGN pa90fel.POVERANTAL = (-1) * pa90fel.POVERANTAL.
                       END.
   	              END.
   	           END.                                     
   	           bryt = FALSE.                  
   	        END.
   	     END.                                            
        END.
        IF TIDFEL.TRAKTKOD NE " " AND TIDFEL.TRAKTANTAL NE 0
   	  THEN DO TRANSACTION:
   	     frvaro = 0.
           FIND FIRST FVARO WHERE FVARO.AONR = TIDFEL.AONR AND FVARO.DELNR = TIDFEL.DELNR
           NO-LOCK NO-ERROR.
           IF AVAILABLE FVARO THEN ASSIGN frvaro = 1.
   	     IF globforetag = "ESAN" AND frvaro = 1 THEN frvaro = 0.
   	     ELSE IF globforetag = "ESMA"  AND frvaro = 1 THEN frvaro = 0.
   	     ELSE DO:
   	        CREATE pa90fel.
   	        ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	        pa90fel.PTRAKTKOD = TIDFEL.TRAKTKOD
   	        pa90fel.PTRAKTANTAL = TIDFEL.TRAKTANTAL
   	        pa90fel.PDATUM = TIDFEL.DATUM
   	        pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	        pa90fel.AONR = TIDFEL.AONR
              pa90fel.DELNR = TIDFEL.DELNR
              pa90fel.PAVTAL = PERSONALTAB.TRAAVTAL
              pa90fel.BOLAG = bolag.
              IF TIDFEL.DEBET = FALSE THEN DO:
                 ASSIGN pa90fel.PTRAKTANTAL = (-1) * pa90fel.PTRAKTANTAL.
              END.
           END.   
   	  END.
        IF TIDFEL.BEREDSKAP NE " " AND TIDFEL.BERANTAL NE 0
   	  THEN DO TRANSACTION:	     	      
   	     CREATE pa90fel.
   	     ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	     pa90fel.PBEREDSKAP = TIDFEL.BEREDSKAP
   	     pa90fel.PBERANTAL = TIDFEL.BERANTAL
   	     pa90fel.PDATUM = TIDFEL.DATUM
   	     pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	     pa90fel.AONR = TIDFEL.AONR
           pa90fel.DELNR = TIDFEL.DELNR	 
           pa90fel.PAVTAL = PERSONALTAB.BEREDSKAPSAVTAL
           pa90fel.BOLAG = bolag.
           IF TIDFEL.DEBET = FALSE THEN DO:
              ASSIGN pa90fel.PBERANTAL = (-1) * pa90fel.PBERANTAL.
           END.        
   	  END.                    
   	  flex = FALSE.  
        IF AVAILABLE FLEXAVT AND FLEXAVT.FLEXTID = TRUE THEN DO:
   	     regdatum = TIDFEL.DATUM.
   	     regvnr = TIDFEL.VECKONUMMER.
   	     RUN SLUTARB.P.
   	     musz = FALSE.
   	     IF bolag = "45" OR bolag = "49" OR bolag = "51" THEN DO:
   	        IF avtal BEGINS "K" AND TIDFEL.OVERTIDUTTAG = "F" AND
   	        TIDFEL.LONTILLAGG = "" THEN musz = TRUE.
           END.
           IF musz = TRUE THEN DO:
              musz = FALSE.
              IF TIDFEL.START < regstart THEN DO:                                                                     
                 nytid = TIDFEL.TOTALT.
                  RUN TIMSEK.P.
                  fltid = fltid + sekunder.                       
              END.
              ELSE IF TIDFEL.START GE regslut THEN DO:                                                             
                  ASSIGN                    
                  nytid = TIDFEL.TOTALT.
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
                     CREATE pa90fel.
                     ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
                     pa90fel.PLONTILLAGG = "100"
                     pa90fel.PLONTILLANTAL = fnytid
                     pa90fel.PSORT = "TI"
                     pa90fel.PDATUM = TIDFEL.DATUM
                     pa90fel.AONR = "1097" + SUBSTRING(STRING(OMRADETAB.ORGIDNUM,"9999"),1,2)
                     pa90fel.DELNR = INTEGER(SUBSTRING(STRING(OMRADETAB.ORGIDNUM,"9999"),3,2))
                     pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
                     pa90fel.PAVTAL = avtal
                     pa90fel.BOLAG = bolag.
                     IF TIDFEL.DEBET = FALSE THEN DO:
                        ASSIGN pa90fel.PLONTILLANTAL = (-1) * pa90fel.PLONTILLANTAL.
                     END.
                     fltid =0.         
                  END.   
               END.                                   
           END.    
           ELSE IF TIDFEL.OVERTIDUTTAG = "F" AND TIDFEL.LONTILLAGG = "" THEN DO:
               IF TIDFEL.AONR = "910927" OR TIDFEL.AONR = "27" THEN fltid = fltid.
               ELSE IF regstart = regslut THEN DO:
                  nytid = TIDFEL.TOTALT.
                  RUN TIMSEK.P.
                  fltid = fltid + sekunder.      
               END.   
               ELSE IF TIDFEL.START < regstart THEN DO:                                                                     
                  nytid = TIDFEL.TOTALT.
                  RUN TIMSEK.P.
                  fltid = fltid + sekunder.                       
               END.
               ELSE IF TIDFEL.START GE regslut THEN DO:                                                             
                  ASSIGN                    
                  nytid = TIDFEL.TOTALT.
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
               CREATE pa90fel.
               ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
               pa90fel.PLONTILLAGG = flkod
               pa90fel.PLONTILLANTAL = fnytid
               pa90fel.PSORT = "TI"
               pa90fel.PDATUM = TIDFEL.DATUM
               pa90fel.AONR = TIDFEL.AONR
               pa90fel.DELNR = TIDFEL.DELNR
               pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
               pa90fel.PAVTAL = avtal
               pa90fel.BOLAG = bolag.
               IF TIDFEL.DEBET = FALSE THEN DO:
                  ASSIGN pa90fel.PLONTILLANTAL = (-1) * pa90fel.PLONTILLANTAL.
               END.
               fltid =0. 
               flex = TRUE.
           END.   
        END.   	     
        /* om flextid skall räknas ut*/
        frvaro = 0.
        FIND FIRST FVARO WHERE FVARO.AONR = TIDFEL.AONR AND FVARO.DELNR = TIDFEL.DELNR
        NO-LOCK NO-ERROR.
        IF AVAILABLE FVARO THEN ASSIGN frvaro = 1. 
   	  IF PERSONALTAB.BEFATTNING = "TIMANSTÄLLD" AND TIDFEL.TOTALT > 0 THEN DO:
   	     IF TIDFEL.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
   	     ELSE DO:
   	        CREATE pa90fel.
   	        ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	        pa90fel.PLONTILLAGG = "010"
   	        pa90fel.PLONTILLANTAL = TIDFEL.TOTALT
   	        pa90fel.PDATUM = TIDFEL.DATUM
   	        pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	        pa90fel.AONR = TIDFEL.AONR
   	        pa90fel.DELNR = TIDFEL.DELNR
   	        pa90fel.PAVTAL = avtal
   	        pa90fel.BOLAG = bolag.
   	        IF TIDFEL.DEBET = FALSE THEN DO:
                 ASSIGN pa90fel.PLONTILLANTAL = (-1) * pa90fel.PLONTILLANTAL.
              END.
   	     END.   
   	  END.
   	  ELSE IF PERSONALTAB.BEFATTNING = "TIMANS.+SEM TJ" AND TIDFEL.TOTALT > 0 THEN DO:                                                                     
   	     IF TIDFEL.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
   	     ELSE DO:
   	        CREATE pa90fel. /*VILART 011*/
   	        ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR.
   	        IF globforetag = "NORD" OR globforetag = "ESMA" 
   	        OR globforetag = "ELPA" OR globforetag = "ESAN" THEN DO:
   	           ASSIGN pa90fel.PLONTILLAGG = "011". /* 040*/
   	        END.
   	        ELSE IF globforetag = "ETA" THEN DO:
       	           ASSIGN pa90fel.PLONTILLAGG = "011".
   	        END. 
   	        ASSIGN
   	        pa90fel.PLONTILLANTAL = TIDFEL.TOTALT
   	        pa90fel.PDATUM = TIDFEL.DATUM
   	        pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	        pa90fel.AONR = TIDFEL.AONR
   	        pa90fel.DELNR = TIDFEL.DELNR
   	        pa90fel.PAVTAL = avtal
   	        pa90fel.BOLAG = bolag.
   	        IF TIDFEL.DEBET = FALSE THEN DO:
                  ASSIGN pa90fel.PLONTILLANTAL = (-1) * pa90fel.PLONTILLANTAL.
              END.
   	     END.   
   	  END.
        ELSE IF PERSONALTAB.BEFATTNING = "TIMANS.+SEM KOLL" AND TIDFEL.TOTALT > 0 THEN DO:                                                                     
   	     IF TIDFEL.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
   	     ELSE DO:
   	        CREATE pa90fel.  /* VILART 011*/
   	        ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR.
       	     IF globforetag = "NORD" OR globforetag = "ESMA" 
   	        OR globforetag = "ELPA" OR globforetag = "ESAN" THEN DO:
   	           ASSIGN pa90fel.PLONTILLAGG = "011". /*030*/
   	        END.
   	        ELSE IF globforetag = "ETA" THEN DO:
       	           ASSIGN pa90fel.PLONTILLAGG = "011".
   	        END. 
   	        ASSIGN
   	        pa90fel.PLONTILLANTAL = TIDFEL.TOTALT
   	        pa90fel.PDATUM = TIDFEL.DATUM
   	        pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	        pa90fel.AONR = TIDFEL.AONR
   	        pa90fel.DELNR = TIDFEL.DELNR
   	        pa90fel.PAVTAL = avtal
   	        pa90fel.BOLAG = bolag.
   	        IF TIDFEL.DEBET = FALSE THEN DO:
                 ASSIGN pa90fel.PLONTILLANTAL = (-1) * pa90fel.PLONTILLANTAL.
              END.
   	     END.   
        END.       
        ELSE IF flex = TRUE THEN nytid = nytid. /*MÄLARDALEN koll skickar -flex och lart 100*/
        ELSE IF TIDFEL.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
   	  ELSE IF TIDFEL.PRISTYP = "RESTID..." THEN nytid = nytid.
   	  ELSE IF TIDFEL.LONTILLAGG NE "" THEN nytid = nytid.	  
   	  ELSE IF frvaro = 1 THEN ASSIGN frvaro = 0.  /*frånvaro*/
   	  /*ELSE IF TIDFEL.AONR BEGINS "910" THEN nytid = nytid.*/
   	  ELSE IF globforetag = "ETA" AND TIDFEL.AONR = "45" THEN nytid = nytid.
   	  ELSE IF ( TIDFEL.OKOD1 = " " AND TIDFEL.OKOD2 = " " AND
        TIDFEL.OKOD3 = " " ) AND TIDFEL.TOTALT > 0 THEN DO:
           CREATE pa90fel.  /* TIMMAR LÖNEART 100*/
   	     ASSIGN pa90fel.PANSTNR = PERSONALTAB.ANSTNR
   	     pa90fel.PLONTILLAGG = "100"
   	     pa90fel.PLONTILLANTAL = TIDFEL.TOTALT
   	     pa90fel.PDATUM = TIDFEL.DATUM
   	     pa90fel.PVECKONUMMER = TIDFEL.VECKONUMMER
   	     pa90fel.AONR = TIDFEL.AONR
   	     pa90fel.DELNR = TIDFEL.DELNR
   	     pa90fel.PAVTAL = avtal
   	     pa90fel.BOLAG = bolag.
   	     IF TIDFEL.DEBET = FALSE THEN DO:
              ASSIGN pa90fel.PLONTILLANTAL = (-1) * pa90fel.PLONTILLANTAL.
           END.
   	  END.          
     END.
     GET NEXT tq.
   END.
   GET NEXT pq.
END.
FOR EACH pa90fel WHERE pa90fel.PANSTNR = " ":
  DELETE pa90fel.
END.
FOR EACH pa90fel where pa90fel.PLONTILLAGG NE "":
   FIND FIRST LONTILL WHERE LONTILL.LONTILLAGG = pa90fel.PLONTILLAGG
   AND LONTILL.KOD = pa90fel.PAVTAL USE-INDEX LON 
   NO-LOCK NO-ERROR.
   IF AVAILABLE LONTILL THEN ASSIGN pa90fel.PLONTILLAGG = LONTILL.VILART.
END.
FOR EACH pa90fel where pa90fel.POVERTIDTILL NE "":
   FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = pa90fel.POVERTIDTILL
   AND OVERKOD.KOD = pa90fel.PAVTAL USE-INDEX OVER 
   NO-LOCK NO-ERROR.
   IF AVAILABLE OVERKOD THEN ASSIGN pa90fel.POVERTIDTILL = OVERKOD.VILART.
END.
FOR EACH pa90fel where pa90fel.PBEREDSKAP NE "":
   FIND FIRST BERKOD WHERE BERKOD.BEREDSKAP = pa90fel.PBEREDSKAP
   AND BERKOD.BEREDSKAPSAVTAL = pa90fel.PAVTAL USE-INDEX BERE 
   NO-LOCK NO-ERROR.
   IF AVAILABLE BERKOD THEN ASSIGN pa90fel.PBEREDSKAP = BERKOD.VILART.
END.
FOR EACH pa90fel where pa90fel.PTRAKTKOD NE "":
   FIND FIRST TRAKTATAB WHERE TRAKTATAB.TRAKTKOD = pa90fel.PTRAKTKOD
   AND TRAKTATAB.TRAAVTAL = pa90fel.PAVTAL USE-INDEX TRAAV 
   NO-LOCK NO-ERROR.
   IF AVAILABLE TRAKTATAB THEN ASSIGN pa90fel.PTRAKTKOD = TRAKTATAB.VILART.
END.  
if globforetag = "elpa" then DO:
   OUTPUT TO D:\delad\pro8\GURU\apptemp\pa90fel.d.
   for each pa90fel BY pa90fel.PANSTNR BY pa90fel.PLONTILLAGG:
     display pa90fel.
   end.  
END.

RUN ESFE3.P.      /* summerar veckans skörd till tidigare */
