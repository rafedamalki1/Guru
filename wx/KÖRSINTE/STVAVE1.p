 /*STVAVE1.P*/
DEFINE INPUT PARAMETER kordatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER koranv LIKE ANVANDARE.ANVANDARE NO-UNDO.
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.     
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE NEW SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
/*DEFINE NEW SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE NEW SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE NEW SHARED VARIABLE regtotalt LIKE TIDREGITAB.TOTALT NO-UNDO.
DEFINE NEW SHARED VARIABLE frustarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE fruslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffestart AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffeslut AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchstarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchslutet AS DECIMAL NO-UNDO.
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
DEFINE VARIABLE seku AS INTEGER NO-UNDO.
DEFINE VARIABLE anstperson LIKE PERSONALTAB.ANSTNR NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE TEMP-TABLE pa90fil
   FIELD PANSTNR LIKE PERSONALTAB.ANSTNR
   FIELD START LIKE TIDREGITAB.START
   FIELD SLUT LIKE TIDREGITAB.SLUT
   FIELD PLONTILLAGG LIKE TIDREGITAB.LONTILLAGG
   FIELD PLONTILLANTAL LIKE TIDREGITAB.LONTILLANTAL
   FIELD POVERTIDTILL LIKE TIDREGITAB.OVERTIDTILL
   FIELD POVERANTAL LIKE TIDREGITAB.OVERANTAL
   FIELD PTRAKTKOD LIKE TIDREGITAB.TRAKTKOD
   FIELD PTRAKTANTAL LIKE TIDREGITAB.TRAKTANTAL
   FIELD PBEREDSKAP LIKE TIDREGITAB.BEREDSKAP
   FIELD PBERANTAL LIKE TIDREGITAB.BERANTAL
   FIELD PDATUM LIKE TIDREGITAB.DATUM
   FIELD SLUTDATUM LIKE TIDREGITAB.DATUM INITIAL ?
   FIELD PVECKONUMMER LIKE TIDREGITAB.VECKONUMMER
   FIELD MANAD AS INTEGER
   FIELD PSORT AS CHARACTER FORMAT "XX"
   FIELD PPAR8 AS DECIMAL FORMAT "-99.99"
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD PAVTAL AS CHARACTER
   FIELD TID AS LOGICAL
   FIELD BERKOLL AS LOGICAL INITIAL FALSE
   INDEX PANSTNR IS PRIMARY PANSTNR ASCENDING.
DEFINE TEMP-TABLE berfil
   FIELD PANSTNR LIKE PERSONALTAB.ANSTNR
   FIELD START LIKE TIDREGITAB.START
   FIELD SLUT LIKE TIDREGITAB.SLUT
   FIELD PLONTILLAGG LIKE TIDREGITAB.LONTILLAGG
   FIELD PLONTILLANTAL LIKE TIDREGITAB.LONTILLANTAL
   FIELD POVERTIDTILL LIKE TIDREGITAB.OVERTIDTILL
   FIELD POVERANTAL LIKE TIDREGITAB.OVERANTAL
   FIELD PTRAKTKOD LIKE TIDREGITAB.TRAKTKOD
   FIELD PTRAKTANTAL LIKE TIDREGITAB.TRAKTANTAL
   FIELD PBEREDSKAP LIKE TIDREGITAB.BEREDSKAP
   FIELD PBERANTAL LIKE TIDREGITAB.BERANTAL
   FIELD PDATUM LIKE TIDREGITAB.DATUM
   FIELD SLUTDATUM LIKE TIDREGITAB.DATUM
   FIELD PVECKONUMMER LIKE TIDREGITAB.VECKONUMMER
   FIELD MANAD AS INTEGER
   FIELD PSORT AS CHARACTER FORMAT "XX"
   FIELD PPAR8 AS DECIMAL FORMAT "-99.99"
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD PAVTAL AS CHARACTER
   FIELD TID AS LOGICAL
   FIELD BERKOLL AS LOGICAL INITIAL FALSE
   INDEX PANSTNR IS PRIMARY PANSTNR ASCENDING PDATUM ASCENDING START ASCENDING.

DEFINE OUTPUT PARAMETER TABLE FOR pa90fil.   
DEFINE BUFFER r3buff FOR pa90fil.
FOR EACH pa90fil:
  DELETE pa90fil.
END.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.

vkdatum = kordatum.
OPEN QUERY pq FOR EACH PERSONALTAB WHERE PERSONALTAB.PERSMASK = TRUE
USE-INDEX PERSONALKOD NO-LOCK.
GET FIRST pq NO-LOCK.
DO WHILE AVAILABLE(PERSONALTAB):
   personal = PERSONALTAB.PERSONALKOD.
   avtal1 = PERSONALTAB.ANSTALLNING.
   FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = avtal1
   USE-INDEX ANSTF NO-LOCK NO-ERROR.
   avtal = ANSTFORMTAB.KOD.
   FIND FIRST UTRYCKNING WHERE UTRYCKNING.KOD = avtal USE-INDEX UT
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE UTRYCKNING THEN para = FALSE.
   ELSE para = UTRYCKNING.PARA8.
   OPEN QUERY tq FOR EACH TIDREGITAB WHERE
   TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.GODKAND BEGINS "G" AND TIDREGITAB.VECKOKORD = "" AND
   TIDREGITAB.DATUM <= vkdatum
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
         IF TIDREGITAB.START = 0 AND TIDREGITAB.SLUT > 0 AND TIDREGITAB.OKOD1 NE "" THEN DO:
            FIND FIRST r3buff where r3buff.PANSTNR = PERSONALTAB.ANSTNR AND r3buff.PDATUM = TIDREGITAB.DATUM
            AND r3buff.START = 0 AND r3buff.SLUT GE TIDREGITAB.SLUT NO-LOCK NO-ERROR.
            IF AVAILABLE r3buff THEN DO:
               CREATE pa90fil.
	           ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
	           pa90fil.POVERTIDTILL = TIDREGITAB.OKOD1
	           pa90fil.POVERANTAL = TIDREGITAB.TOTALT
	           pa90fil.PDATUM = TIDREGITAB.DATUM
	           pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
	           pa90fil.AONR = TIDREGITAB.AONR
    	       pa90fil.DELNR = TIDREGITAB.DELNR
    	       pa90fil.MANAD = MONTH(TIDREGITAB.DATUM)
    	       pa90fil.PAVTAL = avtal
    	       pa90fil.START = TIDREGITAB.START
               pa90fil.SLUT = TIDREGITAB.SLUT
               pa90fil.TID = TRUE.
               ASSIGN 
               r3buff.START = pa90fil.SLUT.
            END.
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
            pa90fil.MANAD = MONTH(TIDREGITAB.DATUM)
            pa90fil.PAVTAL = avtal.
            FIND FIRST LONTILL WHERE LONTILL.LONTILLAGG = pa90fil.PLONTILLAGG
            AND LONTILL.KOD = pa90fil.PAVTAL USE-INDEX LON NO-LOCK NO-ERROR.
            IF AVAILABLE LONTILL THEN DO:
               IF LONTILL.LONTILLAGG = "1150" OR LONTILL.LONTILLAGG = "1160" THEN pa90fil.TID = FALSE.
               ELSE IF LONTILL.ENHET = "TI" THEN DO:
                  ASSIGN pa90fil.TID = TRUE
                  pa90fil.START = TIDREGITAB.START
                  pa90fil.SLUT = TIDREGITAB.SLUT.
               END.
               ELSE pa90fil.TID = FALSE.                              
            END.                     
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
	        IF OKOD1 NE "" THEN DO:
	           CREATE pa90fil.
	           ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
	           pa90fil.POVERTIDTILL = TIDREGITAB.OKOD1
	           pa90fil.POVERANTAL = TIDREGITAB.OANT1
	           pa90fil.PDATUM = TIDREGITAB.DATUM
	           pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
	           pa90fil.AONR = TIDREGITAB.AONR
                  pa90fil.DELNR = TIDREGITAB.DELNR
                  pa90fil.MANAD = MONTH(TIDREGITAB.DATUM)
                  pa90fil.PAVTAL = avtal
                  pa90fil.TID = TRUE
                  pa90fil.START = TIDREGITAB.OST1
                  pa90fil.SLUT = TIDREGITAB.OSL1.
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
	           pa90fil.MANAD = MONTH(TIDREGITAB.DATUM)
	           pa90fil.PAVTAL = avtal
                  pa90fil.TID = TRUE
                  pa90fil.START = TIDREGITAB.OST2
                  pa90fil.SLUT = TIDREGITAB.OSL2.	           
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
	           pa90fil.MANAD = MONTH(TIDREGITAB.DATUM)
	           pa90fil.PAVTAL = avtal
                  pa90fil.TID = TRUE                 
                  pa90fil.START = TIDREGITAB.OST3
                  pa90fil.SLUT = TIDREGITAB.OSL3.
	        END.                                    
	     END.     
            ELSE IF oversum > total AND TIDREGITAB.OVERAUTO = TRUE
            AND TIDREGITAB.UTRYCKNING = TRUE THEN DO:                            
	        nod = FALSE.
	        IF TIDREGITAB.OANT3 > 0 THEN DO:
	           oversum = oversum - oa3.
	           IF oversum > total THEN DO:
	              FIND FIRST NFALL WHERE NFALL.KOD = TIDREGITAB.OKOD3 NO-LOCK
                     NO-ERROR.  	  
                     CREATE pa90fil.
                     ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
                     pa90fil.POVERTIDTILL = NFALL.OBYT
                     pa90fil.POVERANTAL = TIDREGITAB.OANT3
                     pa90fil.PDATUM = TIDREGITAB.DATUM
                     pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
                     pa90fil.AONR = TIDREGITAB.AONR
                     pa90fil.DELNR = TIDREGITAB.DELNR
                     pa90fil.PAVTAL = avtal
                     pa90fil.START = TIDREGITAB.OST3
                     pa90fil.SLUT = TIDREGITAB.OSL3
                     pa90fil.TID = TRUE.
                    
	           END.
	           ELSE DO:
	              t2 = total - oa1 - oa2. 
	              sekunder = t2.
	              RUN SEKTIM.P.	             
	              CREATE r3buff.
	              ASSIGN r3buff.PANSTNR = PERSONALTAB.ANSTNR
	              r3buff.POVERTIDTILL = TIDREGITAB.OKOD3
	              r3buff.POVERANTAL = nytid
	              r3buff.PDATUM = TIDREGITAB.DATUM
	              r3buff.PVECKONUMMER = TIDREGITAB.VECKONUMMER
	              r3buff.AONR = TIDREGITAB.AONR
	              r3buff.DELNR = TIDREGITAB.DELNR
	              r3buff.MANAD = MONTH(TIDREGITAB.DATUM)
	              r3buff.PAVTAL = avtal
	              r3buff.START = TIDREGITAB.OST3
	              r3buff.TID = TRUE.
	              nytid = TIDREGITAB.OST3.
	              RUN TIMSEK.P.
	              sekunder = sekunder + t2.
	              RUN SEKTIM.P.
                     r3buff.SLUT = nytid.
	              t3 = oa3 - t2.
	              sekunder = t3.
	              RUN SEKTIM.P.	   
	              FIND FIRST NFALL WHERE NFALL.KOD = TIDREGITAB.OKOD3 NO-LOCK
                     NO-ERROR.                   
	              CREATE pa90fil.
	              ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
	              pa90fil.POVERTIDTILL = NFALL.OBYT
	              pa90fil.POVERANTAL = nytid
	              pa90fil.PDATUM = TIDREGITAB.DATUM
	              pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
	              pa90fil.AONR = TIDREGITAB.AONR
 	              pa90fil.DELNR = TIDREGITAB.DELNR
 	              pa90fil.MANAD = MONTH(TIDREGITAB.DATUM)
 	              pa90fil.PAVTAL = avtal
 	              pa90fil.START = r3buff.SLUT
 	              pa90fil.SLUT = TIDREGITAB.OST3
                     pa90fil.TID = TRUE.
	              bryt = TRUE.   
	           END.
	        END.
	        IF TIDREGITAB.OANT2 > 0 THEN DO:
	           oversum = oversum - oa2.
	           IF oversum > total THEN DO:
	              FIND FIRST NFALL WHERE NFALL.KOD = TIDREGITAB.OKOD2 NO-LOCK
                     NO-ERROR.	              
	              CREATE pa90fil.
	              ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
	              pa90fil.POVERTIDTILL = NFALL.OBYT
	              pa90fil.POVERANTAL = TIDREGITAB.OANT2.
	              IF TIDREGITAB.OST2 = 0 AND TIDREGITAB.OSL2 < TIDREGITAB.SLUT THEN ASSIGN pa90fil.PDATUM = TIDREGITAB.DATUM + 1.
                     ELSE ASSIGN pa90fil.PDATUM = TIDREGITAB.DATUM.
                     ASSIGN
	              pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
	              pa90fil.AONR = TIDREGITAB.AONR
	              pa90fil.DELNR = TIDREGITAB.DELNR
	              pa90fil.MANAD = MONTH(TIDREGITAB.DATUM)
	              pa90fil.PAVTAL = avtal
	              pa90fil.START = TIDREGITAB.OST2
                     pa90fil.SLUT = TIDREGITAB.OSL2
                     pa90fil.TID = TRUE.
	           END.
	           ELSE IF bryt = TRUE THEN DO:
	              CREATE pa90fil.
	              ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
	              pa90fil.POVERTIDTILL = TIDREGITAB.OKOD2
	              pa90fil.POVERANTAL = TIDREGITAB.OANT2.
	              IF TIDREGITAB.OST2 = 0 AND TIDREGITAB.OSL2 < TIDREGITAB.SLUT THEN ASSIGN pa90fil.PDATUM = TIDREGITAB.DATUM + 1.
                     ELSE ASSIGN pa90fil.PDATUM = TIDREGITAB.DATUM.
                     ASSIGN
	              pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
	              pa90fil.AONR = TIDREGITAB.AONR
                     pa90fil.DELNR = TIDREGITAB.DELNR
                     pa90fil.MANAD = MONTH(TIDREGITAB.DATUM)
                     pa90fil.PAVTAL = avtal
                     pa90fil.START = TIDREGITAB.OST2
                     pa90fil.SLUT = TIDREGITAB.OSL2
                     pa90fil.TID = TRUE.
	           END.                                      
	           ELSE DO:
	              t2 = total - oa1. 
	              sekunder = t2.
	              RUN SEKTIM.P.
	              IF t2 > 0 THEN DO:
  	                 CREATE r3buff.
	                 ASSIGN r3buff.PANSTNR = PERSONALTAB.ANSTNR
	                 r3buff.POVERTIDTILL = TIDREGITAB.OKOD2
	                 r3buff.POVERANTAL = nytid.
	                 IF TIDREGITAB.OST2 = 0 AND TIDREGITAB.OSL2 < TIDREGITAB.SLUT THEN ASSIGN r3buff.PDATUM = TIDREGITAB.DATUM + 1.
	                 ELSE ASSIGN r3buff.PDATUM = TIDREGITAB.DATUM.
	                 ASSIGN
	                 r3buff.PVECKONUMMER = TIDREGITAB.VECKONUMMER
	                 r3buff.AONR = TIDREGITAB.AONR
	                 r3buff.DELNR = TIDREGITAB.DELNR
	                 r3buff.MANAD = MONTH(TIDREGITAB.DATUM)
	                 r3buff.PAVTAL = avtal
	                 r3buff.START = TIDREGITAB.OST2
                        r3buff.TID = TRUE
	                 nytid = TIDREGITAB.OST2.
	                 RUN TIMSEK.P.
	                 sekunder = sekunder + t2.
	                 RUN SEKTIM.P.
                        r3buff.SLUT = nytid.
                     END.   
                     FIND FIRST NFALL WHERE NFALL.KOD = TIDREGITAB.OKOD2 NO-LOCK
                     NO-ERROR.
	             /* t3 = oa2 - t2.
	              sekunder = t3.
	              RUN SEKTIM.P.	                    */	              
 	              CREATE pa90fil.
	              ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR   
     	              pa90fil.POVERTIDTILL = NFALL.OBYT
	              pa90fil.POVERANTAL = nytid.
	              IF TIDREGITAB.OST2 = 0 AND TIDREGITAB.OSL2 < TIDREGITAB.SLUT THEN ASSIGN pa90fil.PDATUM = TIDREGITAB.DATUM + 1.
                     ELSE ASSIGN pa90fil.PDATUM = TIDREGITAB.DATUM.
	              ASSIGN
	              pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
	              pa90fil.AONR = TIDREGITAB.AONR
	              pa90fil.DELNR = TIDREGITAB.DELNR
	              pa90fil.MANAD = MONTH(TIDREGITAB.DATUM)
	              pa90fil.PAVTAL = avtal.
	              IF t2 > 0 THEN ASSIGN pa90fil.START = r3buff.SLUT.
	              ELSE ASSIGN pa90fil.START = TIDREGITAB.OST2.
	              ASSIGN
                     pa90fil.SLUT = TIDREGITAB.OSL2
                     pa90fil.TID = TRUE.    
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
	              pa90fil.MANAD = MONTH(TIDREGITAB.DATUM)
	              pa90fil.PAVTAL = avtal
	              pa90fil.START = TIDREGITAB.OST1
                     pa90fil.SLUT = TIDREGITAB.OSL1
                     pa90fil.TID = TRUE.
	           END .                                      
	           ELSE DO:
	             /* t3 = oa1 - total.
	              sekunder = t3.
	              RUN SEKTIM.P.*/
	              CREATE r3buff.
	              ASSIGN r3buff.PANSTNR = PERSONALTAB.ANSTNR
	              r3buff.POVERTIDTILL = TIDREGITAB.OKOD1
	              r3buff.POVERANTAL = TIDREGITAB.TOTALT
	              r3buff.PDATUM = TIDREGITAB.DATUM  
	              r3buff.PVECKONUMMER = TIDREGITAB.VECKONUMMER
	              r3buff.AONR = TIDREGITAB.AONR
 	              r3buff.DELNR = TIDREGITAB.DELNR
 	              r3buff.MANAD = MONTH(TIDREGITAB.DATUM)
 	              r3buff.PAVTAL = avtal
 	              r3buff.START = TIDREGITAB.OST1
 	              r3buff.TID = TRUE.
	              nytid = TIDREGITAB.OST1.
	              RUN TIMSEK.P.
	              sekunder = sekunder + total.
	              RUN SEKTIM.P.
                     r3buff.SLUT = nytid. 	     
 	              FIND FIRST NFALL WHERE NFALL.KOD = TIDREGITAB.OKOD1 NO-LOCK
                     NO-ERROR.              
	              CREATE pa90fil.
	              ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
	              pa90fil.POVERTIDTILL = NFALL.OBYT
	              pa90fil.POVERANTAL = nytid
	              pa90fil.PDATUM = TIDREGITAB.DATUM
	              pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
	              pa90fil.AONR = TIDREGITAB.AONR
    	              pa90fil.DELNR = TIDREGITAB.DELNR
    	              pa90fil.MANAD = MONTH(TIDREGITAB.DATUM)
    	              pa90fil.PAVTAL = avtal
    	              pa90fil.START = r3buff.SLUT
                     pa90fil.SLUT = TIDREGITAB.OSL1
                     pa90fil.TID = TRUE.    
	           END.
	        END.                                     
	        bryt = FALSE.
	     END.	                                                 	     
      END.      
      IF TIDREGITAB.TRAKTKOD NE " " AND TIDREGITAB.TRAKTANTAL NE 0
	  THEN DO TRANSACTION:
	     CREATE pa90fil.
	     ASSIGN pa90fil.PANSTNR = PERSONALTAB.ANSTNR
	     pa90fil.PTRAKTKOD = TIDREGITAB.TRAKTKOD
	     pa90fil.PTRAKTANTAL = TIDREGITAB.TRAKTANTAL
	     pa90fil.PDATUM = TIDREGITAB.DATUM
	     pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
	     pa90fil.AONR = TIDREGITAB.AONR
            pa90fil.DELNR = TIDREGITAB.DELNR
            pa90fil.MANAD = MONTH(TIDREGITAB.DATUM)
            pa90fil.PAVTAL = PERSONALTAB.TRAAVTAL
            pa90fil.TID = FALSE.
	  END.
	  IF TIDREGITAB.BEREDSKAP NE " " AND TIDREGITAB.BERANTAL NE 0
	  THEN DO TRANSACTION:
	     CREATE berfil.
	     ASSIGN berfil.PANSTNR = PERSONALTAB.ANSTNR
	     berfil.PBEREDSKAP = TIDREGITAB.BEREDSKAP
	     berfil.PBERANTAL = TIDREGITAB.BERANTAL
	     berfil.PDATUM = TIDREGITAB.DATUM
	     berfil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
	     berfil.AONR = TIDREGITAB.AONR
            berfil.DELNR = TIDREGITAB.DELNR
            berfil.MANAD = MONTH(TIDREGITAB.DATUM)
            berfil.PAVTAL = PERSONALTAB.BEREDSKAPSAVTAL
            berfil.START = TIDREGITAB.BEREDSKAPSTART
            berfil.SLUT = TIDREGITAB.BEREDSKAPSLUT
            berfil.TID = TRUE.
	  END.                    
      END.
      GET NEXT tq.
   END.
   GET NEXT pq.
END.
FOR EACH pa90fil WHERE pa90fil.PANSTNR = " ":
  DELETE pa90fil.
END.
FOR EACH pa90fil where pa90fil.START = 00 AND pa90fil.POVERTIDTILL = "1118":
   FIND FIRST r3buff where r3buff.PANSTNR = pa90fil.PANSTNR AND r3buff.PDATUM = pa90fil.PDATUM
   AND r3buff.START = pa90fil.SLUT AND r3buff.POVERTIDTILL = "1110" NO-LOCK NO-ERROR.
   IF AVAILABLE r3buff THEN DO:
      ASSIGN pa90fil.POVERTIDTILL = "1110".
   END.
END.   
FOR EACH pa90fil where pa90fil.START = 00 AND pa90fil.POVERTIDTILL = "1119":
   FIND FIRST r3buff where r3buff.PANSTNR = pa90fil.PANSTNR AND r3buff.PDATUM = pa90fil.PDATUM
   AND r3buff.START = pa90fil.SLUT AND r3buff.POVERTIDTILL = "1111" NO-LOCK NO-ERROR.
   IF AVAILABLE r3buff THEN DO:
      ASSIGN pa90fil.POVERTIDTILL = "1111".
   END.
END.       
    
FOR EACH pa90fil where pa90fil.PLONTILLAGG NE "":
   FIND FIRST LONTILL WHERE LONTILL.LONTILLAGG = pa90fil.PLONTILLAGG
   AND LONTILL.KOD = pa90fil.PAVTAL USE-INDEX LON NO-LOCK NO-ERROR.
   IF AVAILABLE LONTILL THEN ASSIGN pa90fil.PLONTILLAGG = LONTILL.VILART.
END.
FOR EACH pa90fil where pa90fil.POVERTIDTILL NE "":
   FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = pa90fil.POVERTIDTILL
   AND OVERKOD.KOD = pa90fil.PAVTAL USE-INDEX OVER NO-LOCK NO-ERROR.
   IF AVAILABLE OVERKOD THEN ASSIGN pa90fil.POVERTIDTILL = OVERKOD.VILART.
END.
FOR EACH berfil where berfil.PBEREDSKAP NE "":
   FIND FIRST BERKOD WHERE BERKOD.BEREDSKAP = berfil.PBEREDSKAP 
   AND BERKOD.BEREDSKAPSAVTAL = berfil.PAVTAL USE-INDEX BERE NO-LOCK NO-ERROR.
   IF AVAILABLE BERKOD THEN ASSIGN berfil.PBEREDSKAP = BERKOD.VILART.
END.
FOR EACH pa90fil where pa90fil.PTRAKTKOD NE "":
   FIND FIRST TRAKTATAB WHERE TRAKTATAB.TRAKTKOD = pa90fil.PTRAKTKOD
   AND TRAKTATAB.TRAAVTAL = pa90fil.PAVTAL USE-INDEX TRAAV NO-LOCK NO-ERROR.
   IF AVAILABLE TRAKTATAB THEN ASSIGN pa90fil.PTRAKTKOD = TRAKTATAB.VILART.
END.
GET FIRST pq NO-LOCK.
DO WHILE AVAILABLE(PERSONALTAB):
   FIND FIRST berfil WHERE berfil.PANSTNR = PERSONALTAB.ANSTNR USE-INDEX PANSTNR NO-LOCK NO-ERROR.
   IF AVAILABLE berfil THEN DO:
      CREATE pa90fil.
      ASSIGN pa90fil.PANSTNR = berfil.PANSTNR
      pa90fil.PBEREDSKAP = berfil.PBEREDSKAP
      pa90fil.PBERANTAL = berfil.PBERANTAL
      pa90fil.PDATUM = berfil.PDATUM
      pa90fil.SLUTDATUM = berfil.PDATUM
      pa90fil.PVECKONUMMER = berfil.PVECKONUMMER
      pa90fil.AONR = berfil.AONR
      pa90fil.DELNR = berfil.DELNR
      pa90fil.MANAD = berfil.MANAD
      pa90fil.PAVTAL = berfil.PAVTAL
      pa90fil.START = berfil.START
      pa90fil.SLUT = berfil.SLUT
      pa90fil.TID = TRUE. 
   END.   
   berkoll:
   REPEAT:
      FIND NEXT berfil WHERE berfil.PANSTNR = PERSONALTAB.ANSTNR USE-INDEX PANSTNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE berfil THEN LEAVE berkoll.
      IF berfil.PDATUM = pa90fil.SLUTDATUM THEN DO:
         IF pa90fil.SLUT = berfil.START AND pa90fil.PBEREDSKAP = berfil.PBEREDSKAP THEN DO:
            ASSIGN pa90fil.SLUT = berfil.SLUT.
         END.
         ELSE IF pa90fil.SLUT GE 7 AND berfil.START LE 16.40 AND pa90fil.PBEREDSKAP = berfil.PBEREDSKAP THEN DO:
            ASSIGN pa90fil.SLUT = berfil.SLUT.
         END.
         ELSE DO:
            CREATE pa90fil.
            ASSIGN pa90fil.PANSTNR = berfil.PANSTNR
            pa90fil.PBEREDSKAP = berfil.PBEREDSKAP
            pa90fil.PBERANTAL = berfil.PBERANTAL
            pa90fil.PDATUM = berfil.PDATUM
            pa90fil.SLUTDATUM = berfil.PDATUM
            pa90fil.PVECKONUMMER = berfil.PVECKONUMMER
            pa90fil.AONR = berfil.AONR
            pa90fil.DELNR = berfil.DELNR
            pa90fil.MANAD = berfil.MANAD
            pa90fil.PAVTAL = berfil.PAVTAL
            pa90fil.START = berfil.START
            pa90fil.SLUT = berfil.SLUT
            pa90fil.TID = TRUE.
         END.   
      END.
      ELSE DO:   
         musz = FALSE.
         IF pa90fil.SLUT NE 24 THEN musz = TRUE.
         IF berfil.START NE 00 THEN musz = TRUE.
         IF pa90fil.PBEREDSKAP NE berfil.PBEREDSKAP THEN musz = TRUE.
/*         IF pa90fil.SLUT NE 24 OR berfil.START NE 00 OR pa90fil.PBEREDSKAP = berfil.PBEREDSKAP THEN DO:*/
         IF musz = TRUE THEN DO:
            musz = FALSE.           
            CREATE pa90fil.
            ASSIGN pa90fil.PANSTNR = berfil.PANSTNR
            pa90fil.PBEREDSKAP = berfil.PBEREDSKAP
            pa90fil.PBERANTAL = berfil.PBERANTAL
            pa90fil.PDATUM = berfil.PDATUM
            pa90fil.SLUTDATUM = berfil.PDATUM
            pa90fil.PVECKONUMMER = berfil.PVECKONUMMER
            pa90fil.AONR = berfil.AONR
            pa90fil.DELNR = berfil.DELNR
            pa90fil.MANAD = berfil.MANAD
            pa90fil.PAVTAL = berfil.PAVTAL
            pa90fil.START = berfil.START
            pa90fil.SLUT = berfil.SLUT
            pa90fil.TID = TRUE.
         END.   
         ELSE DO:
            ASSIGN 
            pa90fil.SLUTDATUM = berfil.PDATUM
            pa90fil.SLUT = berfil.SLUT.
         END.    
      END.     
   END.             
   GET NEXT pq NO-LOCK.
END.   
{AMERICANEUROPEAN.I}
/*IF globforetag = "VATT" THEN DO: 
   OUTPUT TO /guru/export/lon/stanstid.d NO-ECHO.
END.
IF globforetag = "elpa" THEN DO:
   OUTPUT TO D:\delad\pro8\GURU\apptemp\stanstid.d.
END.  
anstperson = "".
FOR EACH pa90fil WHERE pa90fil.TID = TRUE BY pa90fil.PANSTNR BY pa90fil.PDATUM BY pa90fil.START
BY pa90fil.PLONTILLAGG BY pa90fil.POVERTIDTILL BY pa90fil.PBEREDSKAP :
   IF anstperson NE pa90fil.PANSTNR THEN PUT SKIP (1).
   IF pa90fil.START NE pa90fil.SLUT THEN DO: /* TA EJ MED 7-7 00-00 OSV*/
      ASSIGN overrapp1 = "".
      ASSIGN
      SUBSTRING(overrapp1,2,8) = STRING(pa90fil.PANSTNR).
      ASSIGN
      SUBSTRING(overrapp1,11,10) = STRING(pa90fil.PDATUM,"9999/99/99").
      IF pa90fil.SLUTDATUM NE ? THEN DO:
         SUBSTRING(overrapp1,22,10) = STRING(pa90fil.SLUTDATUM,"9999/99/99").
      END.
      ASSIGN
      SUBSTRING(overrapp1,33,5) = STRING(pa90fil.START,"99.99")
      SUBSTRING(overrapp1,39,5) = STRING(pa90fil.SLUT,"99.99").
      IF pa90fil.PLONTILLAGG NE "" THEN SUBSTRING(overrapp1,45,4) = SUBSTRING(pa90fil.PLONTILLAGG,1,4).
      ELSE IF pa90fil.POVERTIDTILL NE "" THEN SUBSTRING(overrapp1,45,4) = SUBSTRING(pa90fil.POVERTIDTILL,1,4).
      ELSE IF pa90fil.PBEREDSKAP NE "" THEN SUBSTRING(overrapp1,45,4) = SUBSTRING(pa90fil.PBEREDSKAP,1,4). 
      PUT overrapp1.   /*  pa90filen.P    */
      PUT SKIP. 
   END.   
   anstperson = pa90fil.PANSTNR.    
END.
IF globforetag = "VATT" THEN DO: 
   OUTPUT TO /guru/export/lon/stanstill.d NO-ECHO.
END.
IF globforetag = "elpa" THEN DO:
   OUTPUT TO D:\delad\pro8\GURU\apptemp\stanstill.d.
END.   
anstperson = "".
FOR EACH pa90fil WHERE pa90fil.TID = FALSE BY pa90fil.PANSTNR BY pa90fil.PDATUM BY pa90fil.PLONTILLAGG
BY pa90fil.PTRAKTKOD:
   IF anstperson NE pa90fil.PANSTNR THEN PUT SKIP (1).
   IF pa90fil.PLONTILLANTAL = 0 AND pa90fil.PTRAKTANTAL = 0 THEN overrapp1 = overrapp1.
   ELSE DO:
      ASSIGN overrapp1 = "".   
      ASSIGN
      SUBSTRING(overrapp1,2,8) = STRING(pa90fil.PANSTNR).
      IF pa90fil.PLONTILLAGG NE "" THEN DO:
          IF pa90fil.PLONTILLAGG = "1150" OR pa90fil.PLONTILLAGG = "1160" THEN DO:
             SUBSTRING(overrapp1,45,4) = SUBSTRING(pa90fil.PLONTILLAGG,1,4).
             nytid = pa90fil.PLONTILLANTAL.
             RUN TIMSEK.P.
             nytid = sekunder / 3600.
             SUBSTRING(overrapp1,50,4) = STRING(nytid,">>>>>9.99").
          END.
          ELSE DO:
             SUBSTRING(overrapp1,45,4) = SUBSTRING(pa90fil.PLONTILLAGG,1,4).
             SUBSTRING(overrapp1,50,4) = STRING(pa90fil.PLONTILLANTAL,">>>>>9.99").
          END.   
      END.    
      ELSE IF pa90fil.PTRAKTKOD NE "" THEN DO:
         SUBSTRING(overrapp1,45,4) = SUBSTRING(pa90fil.PTRAKTKOD,1,4).
         SUBSTRING(overrapp1,50,4) = STRING(pa90fil.PTRAKTANTAL,">>>>>9.99").
      END.   
      ASSIGN
      SUBSTRING(overrapp1,11,10) = STRING(pa90fil.PDATUM,"9999/99/99"). 
   END.
   
   anstperson = pa90fil.PANSTNR.   
   PUT overrapp1.   /*  pa90filen.P    */
   PUT SKIP. 
END.*/

   
   
if globforetag = "elpa" then DO:
   OUTPUT TO D:\delad\pro8\GURU\apptemp\pa90fil.d.
   for each pa90fil BY pa90fil.PANSTNR BY pa90fil.PLONTILLAGG:
     display pa90fil.
   end.  
END.  
{EUROPEANAMERICAN.I}