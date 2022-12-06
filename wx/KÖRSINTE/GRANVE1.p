 /*GRANVE1.P*/
/*UNIX VERSION KOMMUN*/
/* skapar filen pa90hj som är veckans körning .Denna lägges till pa90veck.d*/
/*SKAPAR FILEN PA90VECK.P */
/*ALLA TILLÄGG SOM EKO-LÖNE-SAMMANST. FÅR EN EGEN RAD I DENNA RAPPORT*/
/*RAPPORTEN LIGGER TILL GRUND FÖR MÅNADSKÖRNIING */
{LESAMMAN.I}  
DEFINE INPUT PARAMETER invkdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ingvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER inglobforetag LIKE FORETAG.FORETAG NO-UNDO.
RUN sammut_UI (INPUT 1).
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.

DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
ASSIGN
vkdatum     = invkdatum  
gvisatidpermanad   = ingvisatidpermanad 
Guru.Konstanter:globforetag = inglobforetag.

DEFINE VARIABLE antal LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.   /*LÖN*/
DEFINE VARIABLE antalet LIKE PHJALP.PPAR8 NO-UNDO.   /*LÖN*/
DEFINE VARIABLE personal LIKE PERSONALTAB.PERSONALKOD.      /*LÖN*/
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
DEFINE NEW SHARED TEMP-TABLE pa90fil
   FIELD PPERSKOD LIKE PERSONALTAB.PERSONALKOD
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
   FIELD PKOD LIKE ANSTFORMTAB.KOD
   FIELD POMR LIKE PERSONALTAB.OMRADE
   INDEX PPERSKOD IS PRIMARY PPERSKOD ASCENDING.
FOR EACH pa90fil:
  DELETE pa90fil.
END.
OPEN QUERY persq FOR EACH PERSONALTAB WHERE PERSONALTAB.PERSMASK = TRUE 
USE-INDEX PERSONALKOD NO-LOCK.
GET FIRST persq NO-LOCK.
DO WHILE AVAILABLE(PERSONALTAB):
   ASSIGN 
   personal = PERSONALTAB.PERSONALKOD
   avtal1 = PERSONALTAB.ANSTALLNING.
   IF Guru.Konstanter:globforetag = "GRIT"  OR Guru.Konstanter:globforetag = "celpa" THEN DO:
      personal = PERSONALTAB.ANSTNR.
   END.   
   

   FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = avtal1
   USE-INDEX ANSTF NO-LOCK NO-ERROR.
   avtal = ANSTFORMTAB.KOD.
   FIND FIRST UTRYCKNING WHERE UTRYCKNING.KOD = avtal USE-INDEX UT
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE UTRYCKNING THEN para = FALSE.
   ELSE para = UTRYCKNING.PARA8.
   IF gvisatidpermanad = TRUE THEN DO:
      OPEN QUERY tidq FOR EACH TIDREGITAB WHERE
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.GODKAND BEGINS "G" AND TIDREGITAB.VECKOKORD = "" AND
      TIDREGITAB.DATUM <= vkdatum
      USE-INDEX PSTART NO-LOCK. 
   END.   
   ELSE DO:
      OPEN QUERY tidq FOR EACH TIDREGITAB WHERE
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.GODKAND BEGINS "G" AND TIDREGITAB.VECKOKORD = "" 
      USE-INDEX PSTART NO-LOCK. 
   END.   
   GET FIRST tidq NO-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB): 
      IF (TIDREGITAB.TRAKTKOD = " "  OR TIDREGITAB.TRAKTANTAL = 0) AND
         (TIDREGITAB.LONTILLAGG = " "  OR TIDREGITAB.LONTILLANTAL = 0) AND
         (TIDREGITAB.OKOD1 = " " OR TIDREGITAB.OANT1 = 0) AND
         (TIDREGITAB.OKOD2 = " " OR TIDREGITAB.OANT2 = 0) AND
         (TIDREGITAB.OKOD3 = " " OR TIDREGITAB.OANT3 = 0) AND
         (TIDREGITAB.BEREDSKAP = " " OR TIDREGITAB.BERANTAL = 0)
      THEN DO:
         IF (PERSONALTAB.BEFATTNING = "TIMANSTÄLLD" OR 
         PERSONALTAB.BEFATTNING = "LOKALVÅRDARE" OR 
         PERSONALTAB.BEFATTNING = "PRAKTIKANT") AND TIDREGITAB.TOTALT > 0 
         THEN DO:  
            IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN avtal = avtal.
	         ELSE DO:
	            CREATE pa90fil.
	            ASSIGN pa90fil.PPERSKOD = personal
	            pa90fil.PLONTILLAGG = "020"
	            pa90fil.PLONTILLANTAL = TIDREGITAB.TOTALT
    	         pa90fil.PDATUM = TIDREGITAB.DATUM
	            pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
	            pa90fil.PKOD = avtal.
	         END.   
	      END.   
      END.
      ELSE DO :
         IF TIDREGITAB.LONTILLAGG NE " " AND TIDREGITAB.LONTILLANTAL NE 0
	      THEN DO TRANSACTION:
	         CREATE pa90fil.
	         ASSIGN pa90fil.PPERSKOD = personal
	         pa90fil.PLONTILLAGG = TIDREGITAB.LONTILLAGG
	         pa90fil.PLONTILLANTAL = TIDREGITAB.LONTILLANTAL
	         pa90fil.PDATUM = TIDREGITAB.DATUM
	         pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
	         pa90fil.PKOD = avtal.
	      END.          
         IF TIDREGITAB.OANT1 NE 0 OR TIDREGITAB.OANT2 NE 0 OR TIDREGITAB.OANT3 NE 0
	      THEN DO TRANSACTION:                                                      
	        nytid = TIDREGITAB.OANT1.
	        RUN TIMSEK.P.
	        ASSIGN
	        oa1 = sekunder
	        nytid = TIDREGITAB.OANT2.
	        RUN TIMSEK.P. 
	        ASSIGN
	        oa2 = sekunder
	        oversum = oa1 + sekunder
	        nytid = TIDREGITAB.OANT3.
	        RUN TIMSEK.P.
	        ASSIGN
	        oa3 = sekunder
	        oversum = oversum + sekunder
	        nytid = TIDREGITAB.TOTALT.
	        RUN TIMSEK.P.
	        total = sekunder.  
	        IF TIDREGITAB.OVERAUTO = FALSE OR TIDREGITAB.UTRYCKNING = FALSE OR
           oversum LE total THEN DO:                                               
   	        IF OKOD1 NE "" THEN DO:
   	           CREATE pa90fil.
   	           ASSIGN pa90fil.PPERSKOD = personal
   	           pa90fil.POVERTIDTILL = TIDREGITAB.OKOD1
   	           pa90fil.POVERANTAL = TIDREGITAB.OANT1
   	           pa90fil.PDATUM = TIDREGITAB.DATUM
   	           pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	           pa90fil.PKOD = avtal.
   	        END.    
   	        IF OKOD2 NE "" THEN DO:
   	           CREATE pa90fil.
   	           ASSIGN pa90fil.PPERSKOD = personal
   	           pa90fil.POVERTIDTILL = TIDREGITAB.OKOD2
   	           pa90fil.POVERANTAL = TIDREGITAB.OANT2
   	           pa90fil.PDATUM = TIDREGITAB.DATUM
   	           pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	           pa90fil.PKOD = avtal.
   	        END.   
   	        IF OKOD3 NE "" THEN DO:
   	           CREATE pa90fil.
   	           ASSIGN pa90fil.PPERSKOD = personal
   	           pa90fil.POVERTIDTILL = TIDREGITAB.OKOD3
   	           pa90fil.POVERANTAL = TIDREGITAB.OANT3
   	           pa90fil.PDATUM = TIDREGITAB.DATUM
   	           pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	           pa90fil.PKOD = avtal.
   	        END.                                    
   	     END.     
           ELSE IF oversum > total AND TIDREGITAB.OVERAUTO = TRUE AND 
           TIDREGITAB.UTRYCKNING = TRUE THEN DO:                            
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
   	              ASSIGN pa90fil.PPERSKOD = personal
   	              pa90fil.POVERTIDTILL = TIDREGITAB.OKOD1
   	              pa90fil.POVERANTAL = TIDREGITAB.OANT1
   	              pa90fil.PDATUM = TIDREGITAB.DATUM
   	              pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	              pa90fil.PKOD = avtal.
   	           END.    
   	           IF OKOD2 NE "" THEN DO:
   	              CREATE pa90fil.
   	              ASSIGN pa90fil.PPERSKOD = personal
   	              pa90fil.POVERTIDTILL = TIDREGITAB.OKOD2
   	              pa90fil.POVERANTAL = TIDREGITAB.OANT2
   	              pa90fil.PDATUM = TIDREGITAB.DATUM
   	              pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	              pa90fil.PKOD = avtal.
   	           END.    
   	           IF OKOD3 NE "" THEN DO:
   	              CREATE pa90fil.
   	              ASSIGN pa90fil.PPERSKOD = personal
   	              pa90fil.POVERTIDTILL = TIDREGITAB.OKOD3
   	              pa90fil.POVERANTAL = TIDREGITAB.OANT3
   	              pa90fil.PDATUM = TIDREGITAB.DATUM
   	              pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	              pa90fil.PKOD = avtal.
   	           END.             
   	        END.   
              ELSE  DO:
   	           IF TIDREGITAB.OKOD3 NE "" AND TIDREGITAB.OANT3 > 0 THEN DO:
   	              oversum = oversum - oa3.
   	              IF oversum > total THEN DO:
   	                 FIND FIRST NFALL WHERE NFALL.KOD = TIDREGITAB.OKOD3 
   	                 NO-LOCK NO-ERROR.  
   	                 IF NOT AVAILABLE NFALL THEN DO:
   	                    FIND FIRST NFALL WHERE NFALL.OBYT = TIDREGITAB.OKOD3 
   	                    NO-LOCK NO-ERROR.  
   	                 END.   
   	                 CREATE pa90fil.
   	                 ASSIGN pa90fil.PPERSKOD = personal
   	                 pa90fil.POVERTIDTILL = NFALL.OBYT
   	                 pa90fil.POVERANTAL = TIDREGITAB.OANT3
   	                 pa90fil.PDATUM = TIDREGITAB.DATUM
   	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                 pa90fil.PKOD = avtal.
   	              END.
   	              ELSE DO:
   	                 t2 = total - oa1 - oa2. 
   	                 sekunder = t2.
   	                 RUN SEKTIM.P.	             
       	              CREATE pa90fil.
   	                 ASSIGN pa90fil.PPERSKOD = personal
   	                 pa90fil.POVERTIDTILL = TIDREGITAB.OKOD3
   	                 pa90fil.POVERANTAL = nytid
   	                 pa90fil.PDATUM = TIDREGITAB.DATUM
   	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                 pa90fil.PKOD = avtal.
   	                 t3 = oa3 - t2.
   	                 sekunder = t3.
   	                 RUN SEKTIM.P.
   	                 FIND FIRST NFALL WHERE NFALL.KOD = TIDREGITAB.OKOD3 
   	                 NO-LOCK NO-ERROR.
   	                 IF NOT AVAILABLE NFALL THEN DO:
   	                    FIND FIRST NFALL WHERE NFALL.OBYT = TIDREGITAB.OKOD3 
   	                    NO-LOCK NO-ERROR.  
   	                 END.      
   	                 CREATE pa90fil.
   	                 ASSIGN pa90fil.PPERSKOD = personal
   	                 pa90fil.POVERTIDTILL = NFALL.OBYT
   	                 pa90fil.POVERANTAL = nytid
   	                 pa90fil.PDATUM = TIDREGITAB.DATUM
   	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                 pa90fil.PKOD = avtal. 
   	                 bryt = TRUE.   
   	              END.
   	           END.
   	           IF TIDREGITAB.OKOD2 NE "" AND TIDREGITAB.OANT2 > 0 THEN DO:
   	              oversum = oversum - oa2.
   	              IF oversum > total THEN DO:
   	                 FIND FIRST NFALL WHERE NFALL.KOD = TIDREGITAB.OKOD2 
   	                 NO-LOCK NO-ERROR.   
   	                 IF NOT AVAILABLE NFALL THEN DO:
      	                    FIND FIRST NFALL WHERE NFALL.OBYT = TIDREGITAB.OKOD2 
      	                    NO-LOCK NO-ERROR.  
   	                 END.   
   	                 CREATE pa90fil.
   	                 ASSIGN pa90fil.PPERSKOD = personal
   	                 pa90fil.POVERTIDTILL = NFALL.OBYT
   	                 pa90fil.POVERANTAL = TIDREGITAB.OANT2
   	                 pa90fil.PDATUM = TIDREGITAB.DATUM
   	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                 pa90fil.PKOD = avtal.
   	              END.
   	              ELSE IF bryt = TRUE THEN DO:
   	                 CREATE pa90fil.
   	                 ASSIGN pa90fil.PPERSKOD = personal
   	                 pa90fil.POVERTIDTILL = TIDREGITAB.OKOD2
   	                 pa90fil.POVERANTAL = TIDREGITAB.OANT2
   	                 pa90fil.PDATUM = TIDREGITAB.DATUM
   	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                 pa90fil.PKOD = avtal.
   	              END.                                      
   	              ELSE DO:
   	                 t2 = total - oa1. 
   	                 sekunder = t2.
   	                 RUN SEKTIM.P.
   	                 CREATE pa90fil.
   	                 ASSIGN pa90fil.PPERSKOD = personal
   	                 pa90fil.POVERTIDTILL = TIDREGITAB.OKOD2
   	                 pa90fil.POVERANTAL = nytid
   	                 pa90fil.PDATUM = TIDREGITAB.DATUM
   	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                 pa90fil.PKOD = avtal.
   	                 t3 = oa2 - t2.
   	                 sekunder = t3.
   	                 RUN SEKTIM.P.
   	                 FIND FIRST NFALL WHERE NFALL.KOD = TIDREGITAB.OKOD2 
   	                 NO-LOCK NO-ERROR. 
   	                 IF NOT AVAILABLE NFALL THEN DO:
   	                    FIND FIRST NFALL WHERE NFALL.OBYT = TIDREGITAB.OKOD2 
   	                    NO-LOCK NO-ERROR.  
   	                 END.     
   	                 CREATE pa90fil.
   	                 ASSIGN pa90fil.PPERSKOD = personal
        	              pa90fil.POVERTIDTILL = NFALL.OBYT
   	                 pa90fil.POVERANTAL = nytid
   	                 pa90fil.PDATUM = TIDREGITAB.DATUM
   	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                 pa90fil.PKOD = avtal.    
   	                 bryt = TRUE.
   	              END.
   	           END.   
   	           IF TIDREGITAB.OKOD1 NE "" AND TIDREGITAB.OANT1 > 0 THEN DO:
   	              IF bryt = TRUE THEN DO:
   	                 CREATE pa90fil.
   	                 ASSIGN pa90fil.PPERSKOD = personal
   	                 pa90fil.POVERTIDTILL = TIDREGITAB.OKOD1
   	                 pa90fil.POVERANTAL = TIDREGITAB.OANT1
      	                 pa90fil.PDATUM = TIDREGITAB.DATUM
   	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                 pa90fil.PKOD = avtal.
   	              END.                                      
   	              ELSE DO:
   	                 t3 = oa1 - total.
   	                 sekunder = t3.
   	                 RUN SEKTIM.P.
   	                 CREATE pa90fil.
   	                 ASSIGN pa90fil.PPERSKOD = personal
   	                 pa90fil.POVERTIDTILL = TIDREGITAB.OKOD1
   	                 pa90fil.POVERANTAL = TIDREGITAB.TOTALT
   	                 pa90fil.PDATUM = TIDREGITAB.DATUM  
   	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                 pa90fil.PKOD = avtal. 
   	                 FIND FIRST NFALL WHERE NFALL.KOD = TIDREGITAB.OKOD1 
   	                 NO-LOCK NO-ERROR.   
   	                 IF NOT AVAILABLE NFALL THEN DO:
   	                    FIND FIRST NFALL WHERE NFALL.OBYT = TIDREGITAB.OKOD1 
   	                    NO-LOCK NO-ERROR.  
   	                 END.   
   	                 CREATE pa90fil.
   	                 ASSIGN pa90fil.PPERSKOD = personal
   	                 pa90fil.POVERTIDTILL = NFALL.OBYT
   	                 pa90fil.POVERANTAL = nytid
   	                 pa90fil.PDATUM = TIDREGITAB.DATUM
   	                 pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	                 pa90fil.PKOD = avtal.    
   	              END.
   	           END.                                     
   	           bryt = FALSE.
   	        END.
   	     END.                                       	             
        END.
        IF TIDREGITAB.TRAKTKOD NE " " AND TIDREGITAB.TRAKTANTAL NE 0
	     THEN DO TRANSACTION:
   	     CREATE pa90fil.
   	     ASSIGN pa90fil.PPERSKOD = personal
   	     pa90fil.PTRAKTKOD = TIDREGITAB.TRAKTKOD
   	     pa90fil.PTRAKTANTAL = TIDREGITAB.TRAKTANTAL
   	     pa90fil.PDATUM = TIDREGITAB.DATUM
   	     pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	     pa90fil.PKOD = avtal.
   	  END.
   	  IF TIDREGITAB.BEREDSKAP NE " " AND TIDREGITAB.BERANTAL NE 0
   	  THEN DO TRANSACTION:
   	     CREATE pa90fil.
   	     ASSIGN pa90fil.PPERSKOD = personal
   	     pa90fil.PBEREDSKAP = TIDREGITAB.BEREDSKAP
   	     pa90fil.PBERANTAL = TIDREGITAB.BERANTAL
   	     pa90fil.PDATUM = TIDREGITAB.DATUM
   	     pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
   	     pa90fil.PKOD = avtal
   	     pa90fil.POMR = PERSONALTAB.OMRADE.
   	  END.
        /*Efter samtal med Marie 2004-03-30. Enbart normaltimmar skall över
        Övertid och restid går över med lönearter*/
        IF (TIDREGITAB.LONTILLAGG = " "  OR TIDREGITAB.LONTILLANTAL = 0) AND
         (TIDREGITAB.OKOD1 = " " OR TIDREGITAB.OANT1 = 0) AND
         (TIDREGITAB.OKOD2 = " " OR TIDREGITAB.OANT2 = 0) AND
         (TIDREGITAB.OKOD3 = " " OR TIDREGITAB.OANT3 = 0)          
        THEN DO:
           IF (PERSONALTAB.BEFATTNING = "TIMANSTÄLLD" OR PERSONALTAB.BEFATTNING = "LOKALVÅRDARE"
      	  OR PERSONALTAB.BEFATTNING = "PRAKTIKANT") AND 
      	  TIDREGITAB.TOTALT > 0 THEN DO: 
      	     IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN avtal = avtal.
      	     ELSE DO:
      	        CREATE pa90fil.
      	        ASSIGN pa90fil.PPERSKOD = personal
      	        pa90fil.PLONTILLAGG = "020"
      	        pa90fil.PLONTILLANTAL = TIDREGITAB.TOTALT
      	        pa90fil.PDATUM = TIDREGITAB.DATUM
      	        pa90fil.PVECKONUMMER = TIDREGITAB.VECKONUMMER
      	        pa90fil.PKOD = avtal.
      	     END.   
      	  END.                      
        END.
     END.
     GET NEXT tidq NO-LOCK.
  END.
  GET NEXT persq NO-LOCK.
END.
FOR EACH pa90fil WHERE pa90fil.PPERSKOD = " ":
  DELETE pa90fil.
END.

IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
   FOR EACH pa90fil WHERE pa90fil.PLONTILLAGG NE "":
      FIND FIRST LONTILL WHERE LONTILL.LONTILLAGG = pa90fil.PLONTILLAGG
      USE-INDEX LON NO-LOCK NO-ERROR.
      IF AVAILABLE LONTILL AND LONTILL.TYPKOD = "BIL" THEN DELETE pa90fil.    
   END.
END.

RUN GRANVE2.P.      /* summerar veckans skörd till tidigare */
RUN sammut_UI (INPUT 2).
