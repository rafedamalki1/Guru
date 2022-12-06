 /*NORDVE1.P*/
/*UNIX VERSION KOMMUN*/
/* skapar filen pa90hj som är veckans körning .Denna lägges till pa90veck.d*/
/*SKAPAR FILEN PA90VECK.P */
/*ALLA TILLÄGG SOM VECKOKÖRS FÅR EN EGEN RAD I DENNA RAPPORT*/
/*RAPPORTEN LIGGER TILL GRUND FÖR MÅNADSKÖRNIING */
 
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
DEFINE new SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE new SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE seku AS INTEGER NO-UNDO.
/*DEFINE new SHARED VARIABLE vdatum AS DATE NO-UNDO.
DEFINE new SHARED VARIABLE xdatum AS DATE NO-UNDO.*/
DEFINE NEW SHARED TEMP-TABLE pa90fil
   FIELD PPERSONNUMMER LIKE PERSONALTAB.PERSONNUMMER
   FIELD PLONTILLAGG LIKE TIDREGITAB.LONTILLAGG
   FIELD PLONTILLANTAL LIKE TIDREGITAB.LONTILLANTAL
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
   INDEX PPERSONNUMMER IS PRIMARY PPERSONNUMMER ASCENDING.
/*HUR GÖRA MED BELOPP?????????*/
/*NOLL UTFYLLNAD, SUMMERING */
/*UPDATE xdatum LABEL "ANGE START DATUM" WITH FRAME AAAA1.
UPDATE vdatum LABEL "ANGE SLUT DATUM" WITH FRAME AAAA1.*/
FOR EACH pa90fil:
  DELETE pa90fil.
END.
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
    OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = personal
    AND TIDREGITAB.VECKOKORD = "v833"
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
	   IF PERSONALTAB.BEFATTNING = "TIMANSTÄLLD" AND TIDREGITAB.TOTALT > 0 THEN DO:                                                                     
	      IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
	      ELSE DO:
	         CREATE pa90fil.
	         ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	         pa90fil.PLONTILLAGG = "010"
	         pa90fil.PLONTILLANTAL = TIDREGITAB.TOTALT
	         pa90fil.PDATUM = TIDREGITAB.DATUM
	         pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.
	      END.   
	   END.
	   ELSE IF PERSONALTAB.BEFATTNING = "TIMANS.+SEM TJ" AND TIDREGITAB.TOTALT > 0 THEN DO:                                                                     
	      IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
	      ELSE DO:
	         CREATE pa90fil.
	         ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	         pa90fil.PLONTILLAGG = "040"
	         pa90fil.PLONTILLANTAL = TIDREGITAB.TOTALT
	         pa90fil.PDATUM = TIDREGITAB.DATUM
	         pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.
	      END.   
	   END.
          ELSE IF PERSONALTAB.BEFATTNING = "TIMANS.+SEM KOLL" AND TIDREGITAB.TOTALT > 0 THEN DO:                                                                     
 	      IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
	      ELSE DO:
	         CREATE pa90fil.
	         ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	         pa90fil.PLONTILLAGG = "030"
	         pa90fil.PLONTILLANTAL = TIDREGITAB.TOTALT
	         pa90fil.PDATUM = TIDREGITAB.DATUM
	         pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.
	      END.   
	   END.
       END.
       ELSE DO :    
	  IF TIDREGITAB.LONTILLAGG NE " " AND TIDREGITAB.LONTILLANTAL NE 0
	  THEN DO TRANSACTION:
	     CREATE pa90fil.
	     ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	     pa90fil.PLONTILLAGG = TIDREGITAB.LONTILLAGG
	     pa90fil.PLONTILLANTAL = TIDREGITAB.LONTILLANTAL
	     pa90fil.PDATUM = TIDREGITAB.DATUM
	     pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.
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
	           ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	           pa90fil.POVERTIDTILL = TIDREGITAB.OKOD1
	           pa90fil.POVERANTAL = TIDREGITAB.OANT1
	           pa90fil.PDATUM = TIDREGITAB.DATUM
	           pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.
	        END.    
	        IF OKOD2 NE "" THEN DO:
	           CREATE pa90fil.
	           ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	           pa90fil.POVERTIDTILL = TIDREGITAB.OKOD2
	           pa90fil.POVERANTAL = TIDREGITAB.OANT2
	           pa90fil.PDATUM = TIDREGITAB.DATUM
	           pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.
	        END.   
	        IF OKOD3 NE "" THEN DO:
	           CREATE pa90fil.
	           ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	           pa90fil.POVERTIDTILL = TIDREGITAB.OKOD3
	           pa90fil.POVERANTAL = TIDREGITAB.OANT3
	           pa90fil.PDATUM = TIDREGITAB.DATUM
	           pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.
	        END.                                    
	     END.     
            ELSE IF oversum > total AND TIDREGITAB.OVERAUTO = TRUE
            AND TIDREGITAB.UTRYCKNING = TRUE THEN DO:                            
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
	              ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	              pa90fil.POVERTIDTILL = TIDREGITAB.OKOD1
	              pa90fil.POVERANTAL = TIDREGITAB.OANT1
	              pa90fil.PDATUM = TIDREGITAB.DATUM
	              pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.
	           END.    
	           IF OKOD2 NE "" THEN DO:
	              CREATE pa90fil.
	              ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	              pa90fil.POVERTIDTILL = TIDREGITAB.OKOD2
	              pa90fil.POVERANTAL = TIDREGITAB.OANT2
                     pa90fil.PDATUM = TIDREGITAB.DATUM
	              pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.	           
	           END.   
	           IF OKOD3 NE "" THEN DO:
	              CREATE pa90fil.
	              ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	              pa90fil.POVERTIDTILL = TIDREGITAB.OKOD3
	              pa90fil.POVERANTAL = TIDREGITAB.OANT3
	              pa90fil.PDATUM = TIDREGITAB.DATUM
	              pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.
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
	                 ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	                 pa90fil.POVERTIDTILL = NFALL.OBYT
	                 pa90fil.POVERANTAL = TIDREGITAB.OANT3
	                 pa90fil.PDATUM = TIDREGITAB.DATUM
	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.
	              END.
	              ELSE DO:
	                 t2 = total - oa1 - oa2. 
	                 sekunder = t2.
 	                 RUN SEKTIM.P.	             
	                 CREATE pa90fil.
	                 ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	                 pa90fil.POVERTIDTILL = TIDREGITAB.OKOD3
	                 pa90fil.POVERANTAL = nytid
	                 pa90fil.PDATUM = TIDREGITAB.DATUM
	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.
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
	                 ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	                 pa90fil.POVERTIDTILL = NFALL.OBYT
	                 pa90fil.POVERANTAL = nytid
	                 pa90fil.PDATUM = TIDREGITAB.DATUM
	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER. 
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
	                 ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	                 pa90fil.POVERTIDTILL = NFALL.OBYT
	                 pa90fil.POVERANTAL = TIDREGITAB.OANT2
	                 pa90fil.PDATUM = TIDREGITAB.DATUM
	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.
	              END.
	              ELSE IF bryt = TRUE THEN DO:
	                 CREATE pa90fil.
	                 ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	                 pa90fil.POVERTIDTILL = TIDREGITAB.OKOD2
	                 pa90fil.POVERANTAL = TIDREGITAB.OANT2
	                 pa90fil.PDATUM = TIDREGITAB.DATUM
	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.
	              END.                                      
	              ELSE DO:
	                 t2 = total - oa1. 
	                 sekunder = t2.
	                 RUN SEKTIM.P.
	                 CREATE pa90fil.
	                 ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	                 pa90fil.POVERTIDTILL = TIDREGITAB.OKOD2
	                 pa90fil.POVERANTAL = nytid
	                 pa90fil.PDATUM = TIDREGITAB.DATUM
	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.
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
	                 ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER   
     	                 pa90fil.POVERTIDTILL = NFALL.OBYT
	                 pa90fil.POVERANTAL = nytid
	                 pa90fil.PDATUM = TIDREGITAB.DATUM
	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.    
	                 bryt = TRUE.
	              END.
	           END.   
	           IF TIDREGITAB.OANT1 > 0 THEN DO:
	              IF bryt = TRUE THEN DO:
	                 CREATE pa90fil.
	                 ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	                 pa90fil.POVERTIDTILL = TIDREGITAB.OKOD1
	                 pa90fil.POVERANTAL = TIDREGITAB.OANT1
   	                 pa90fil.PDATUM = TIDREGITAB.DATUM
	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.
	              END.                                      
	              ELSE DO:
	                 t3 = oa1 - total.
	                 sekunder = t3.
	                 RUN SEKTIM.P.
	                 CREATE pa90fil.
	                 ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	                 pa90fil.POVERTIDTILL = TIDREGITAB.OKOD1
	                 pa90fil.POVERANTAL = TIDREGITAB.TOTALT
	                 pa90fil.PDATUM = TIDREGITAB.DATUM  
	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER. 
	                 FIND FIRST NFALL WHERE NFALL.KOD = TIDREGITAB.OKOD1 NO-LOCK
	                 NO-ERROR.   
	                 IF NOT AVAILABLE NFALL THEN DO:
	                    FIND FIRST NFALL WHERE NFALL.OBYT = TIDREGITAB.OKOD1 NO-LOCK
	                    NO-ERROR.  
	                 END.   
	                 CREATE pa90fil.
	                 ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	                 pa90fil.POVERTIDTILL = NFALL.OBYT
	                 pa90fil.POVERANTAL = nytid
	                 pa90fil.PDATUM = TIDREGITAB.DATUM
	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.    
	              END.
	           END.                                     
	           bryt = FALSE.
	        END.
	     END.                                            
   	  END.
         IF TIDREGITAB.TRAKTKOD NE " " AND TIDREGITAB.TRAKTANTAL NE 0
	  THEN DO TRANSACTION:
	     CREATE pa90fil.
	     ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	     pa90fil.PTRAKTKOD = TIDREGITAB.TRAKTKOD
	     pa90fil.PTRAKTANTAL = TIDREGITAB.TRAKTANTAL
	     pa90fil.PDATUM = TIDREGITAB.DATUM
	     pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.
	  END.
	  IF TIDREGITAB.BEREDSKAP NE " " AND TIDREGITAB.BERANTAL NE 0
	  THEN DO TRANSACTION:
	     CREATE pa90fil.
	     ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	     pa90fil.PBEREDSKAP = TIDREGITAB.BEREDSKAP
	     pa90fil.PBERANTAL = TIDREGITAB.BERANTAL
	     pa90fil.PDATUM = TIDREGITAB.DATUM
	     pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.
	  END.                    
	  IF PERSONALTAB.BEFATTNING = "TIMANSTÄLLD" AND TIDREGITAB.TOTALT > 0 THEN DO:
	     IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
	     ELSE DO:
	        CREATE pa90fil.
	        ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	        pa90fil.PLONTILLAGG = "010"
	        pa90fil.PLONTILLANTAL = TIDREGITAB.TOTALT
	        pa90fil.PDATUM = TIDREGITAB.DATUM
	        pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.
	     END.   
	  END.
	  ELSE IF PERSONALTAB.BEFATTNING = "TIMANS.+SEM TJ" AND TIDREGITAB.TOTALT > 0 THEN DO:                                                                     
	     IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
	     ELSE DO:
	        CREATE pa90fil.
	        ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	        pa90fil.PLONTILLAGG = "040"
	        pa90fil.PLONTILLANTAL = TIDREGITAB.TOTALT
	        pa90fil.PDATUM = TIDREGITAB.DATUM
	        pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.
	     END.   
	  END.
         ELSE IF PERSONALTAB.BEFATTNING = "TIMANS.+SEM KOLL" AND TIDREGITAB.TOTALT > 0 THEN DO:                                                                     
	     IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
	     ELSE DO:
	        CREATE pa90fil.
	        ASSIGN pa90fil.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
	        pa90fil.PLONTILLAGG = "030"
	        pa90fil.PLONTILLANTAL = TIDREGITAB.TOTALT
	        pa90fil.PDATUM = TIDREGITAB.DATUM
	        pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER.
	     END.   
	  END.   
      END.
      GET NEXT tq NO-LOCK.       
   END.
   GET NEXT pQ NO-LOCK.
END.
/*FIND FIRST pa90fil WHERE pa90fil.PLONTILLAGG = "9008" NO-LOCK NO-ERROR.
IF NOT AVAILABLE pa90fil THEN DO:
  personal = personal.
END.
ELSE DO:
  rec = RECID(pa90fil).
  pnr = pa90fil.PPERSONNUMMER.
  vecknr = pa90fil.PVECKONUMMER.
  DO TRANSACTION:
     FIND FIRST pa90fil WHERE pa90fil.PPERSONNUMMER = pnr AND
     pa90fil.PVECKONUMMER = vecknr AND pa90fil.PPAR8 NE 0
     EXCLUSIVE-LOCK NO-ERROR.
     IF NOT AVAILABLE pa90fil THEN DO:
	personal = personal.
     END.
     ELSE DO:
	ASSIGN pa90fil.PPAR8 = 0.
	ett:
	REPEAT:
	   FIND NEXT pa90fil WHERE pa90fil.PPERSONNUMMER = pnr AND
	   pa90fil.PVECKONUMMER = vecknr AND pa90fil.PPAR8 NE 0
	   EXCLUSIVE-LOCK NO-ERROR.
	   IF NOT AVAILABLE pa90fil THEN LEAVE ett.
	   ASSIGN pa90fil.PPAR8 = 0.
	END.
     END.
  END.
  tva:
  REPEAT:
    FIND FIRST pa90fil WHERE RECID(pa90fil) = rec NO-LOCK NO-ERROR.
    FIND NEXT pa90fil WHERE pa90fil.PLONTILLAGG = "9008"
    NO-LOCK NO-ERROR.
    IF NOT AVAILABLE pa90fil THEN LEAVE tva.
    ELSE DO:
      rec = RECID(pa90fil).
      pnr = pa90fil.PPERSONNUMMER.
      vecknr = pa90fil.PVECKONUMMER.
      DO TRANSACTION:
	 FIND FIRST pa90fil WHERE pa90fil.PPERSONNUMMER = pnr AND
	 pa90fil.PVECKONUMMER = vecknr AND pa90fil.PPAR8 NE 0
	 EXCLUSIVE-LOCK NO-ERROR.
	 IF NOT AVAILABLE pa90fil THEN DO:
	    personal = personal.
	 END.
	 ELSE DO:
	    ASSIGN pa90fil.PPAR8 = 0.
	 END.
      END.
      IF NOT AVAILABLE pa90fil THEN DO:
	 personal = personal.
      END.
      ELSE DO:
	 tre:
	 REPEAT:
	    DO TRANSACTION:
	       FIND NEXT pa90fil WHERE pa90fil.PPERSONNUMMER = pnr AND
	       pa90fil.PVECKONUMMER = vecknr AND pa90fil.PPAR8 NE 0
	       EXCLUSIVE-LOCK NO-ERROR.
	       IF NOT AVAILABLE pa90fil THEN LEAVE tre.
	       ASSIGN pa90fil.PPAR8 = 0.
	    END.
	 END. /*repeat tre*/
      END.
    END.
  END.
END.
DO TRANSACTION:
   FIND FIRST pa90fil WHERE pa90fil.PPAR8 NE 0
   EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE pa90fil THEN DO:
      personal = personal.
   END.
   ELSE DO:
      pnr = pa90fil.PPERSONNUMMER.
      antalet = pa90fil.PPAR8.
      datrapphj1 = pa90fil.PDATUM.
      vecknr = pa90fil.PVECKONUMMER.
      ASSIGN pa90fil.PPAR8 = 0.
      CREATE pa90fil.
      ASSIGN pa90fil.PPERSONNUMMER = pnr
      pa90fil.PLONTILLAGG = "9008"
      pa90fil.PLONTILLANTAL = antalet
      pa90fil.PDATUM = datrapphj1
      pa90fil.PVECKONUMMER = vecknr.
   END.
END.
IF NOT AVAILABLE pa90fil THEN DO:
   personal = personal.
END.
ELSE DO:
   block1:
   REPEAT:
      DO TRANSACTION:
	 FIND NEXT pa90fil WHERE pa90fil.PPAR8 NE 0
	 EXCLUSIVE-LOCK NO-ERROR.
	 IF NOT AVAILABLE pa90fil THEN LEAVE block1.
	 ELSE DO:
	    pnr = pa90fil.PPERSONNUMMER.
	    antalet = pa90fil.PPAR8.
	    datrapphj1 = pa90fil.PDATUM.
	    vecknr = pa90fil.PVECKONUMMER.
	    ASSIGN pa90fil.PPAR8 = 0.
	    CREATE pa90fil.
	    ASSIGN pa90fil.PPERSONNUMMER = pnr
	    pa90fil.PLONTILLAGG = "9008"
	    pa90fil.PLONTILLANTAL = antalet
	    pa90fil.PDATUM = datrapphj1
	    pa90fil.PVECKONUMMER = vecknr.
	 END.
      END.
   END.
END. */
FOR EACH pa90fil WHERE pa90fil.PPERSONNUMMER = " ":
  DELETE pa90fil.
END.
RUN XNODU2.P.
