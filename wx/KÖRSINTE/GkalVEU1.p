 /*GKALVEU1.P*/
/*UNIX VERSION KOMMUN*/
/* skapar filen pa90hj som �r veckans k�rning .Denna l�gges till pa90veck.d*/
/*SKAPAR FILEN PA90VECK.P */
/*ALLA TILL�GG SOM EKO-L�NE-SAMMANST. F�R EN EGEN RAD I DENNA RAPPORT*/
/*RAPPORTEN LIGGER TILL GRUND F�R M�NADSK�RNIING */
{LESAMMAN.I}  
DEFINE INPUT PARAMETER invkdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ingvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER inglobforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE INPUT PARAMETER gkorvar AS CHARACTER NO-UNDO.
RUN sammut_UI (INPUT 1).
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.

DEFINE NEW SHARED VARIABLE korvar LIKE FORETAG.FORETAG NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
ASSIGN
vkdatum     = invkdatum  
gvisatidpermanad   = ingvisatidpermanad 
Guru.Konstanter:globforetag = inglobforetag
korvar = gkorvar.

DEFINE VARIABLE antal LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.   /*L�N*/
DEFINE VARIABLE antalet LIKE PHJALP.PPAR8 NO-UNDO.   /*L�N*/
DEFINE VARIABLE personal LIKE PERSONALTAB.PERSONALKOD.      /*L�N*/
DEFINE VARIABLE vecknr LIKE TIDREGITAB.VECKONUMMER.      /*L�N*/
DEFINE VARIABLE overrapp1 AS CHARACTER FORMAT "X(70)".      /*L�N*/
DEFINE VARIABLE overrapp2 AS CHARACTER FORMAT "X(70)".      /*�VERTID*/
DEFINE VARIABLE overrapp3 AS CHARACTER FORMAT "X(70)".      /*BEREEDSKAP*/
DEFINE VARIABLE overrapp4 AS CHARACTER FORMAT "X(70)".      /*TRAKTAMENTE*/
DEFINE VARIABLE rapphj1 AS CHARACTER FORMAT "X(9)".      /*HJ�LP ANTAL*/
DEFINE VARIABLE rapphj2 AS CHARACTER FORMAT "X(11)".      /*HJ�LP belopp*/
DEFINE VARIABLE datrapphj1 AS DATE FORMAT "999999".      /*HJ�LP datum*/
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
   FIELD MANAD AS INTEGER
   FIELD PVECKONUMMER LIKE TIDREGITAB.VECKONUMMER
   FIELD PSORT AS CHARACTER FORMAT "XX"
   FIELD PKOD LIKE ANSTFORMTAB.KOD
   FIELD POMR LIKE PERSONALTAB.OMRADE
   FIELD AONR AS CHARACTER FORMAT "X(6)"
   FIELD DELNR AS INTEGER
   FIELD AVKTO AS CHARACTER
   INDEX PPERSKOD IS PRIMARY PPERSKOD ASCENDING.
DEFINE BUFFER pabuff FOR pa90fil.
EMPTY TEMP-TABLE pa90fil NO-ERROR. 
{AMERICANEUROPEAN.I}
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

   OPEN QUERY tidq FOR EACH TIDREGITAB WHERE
   TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.GODKAND BEGINS "G" AND TIDREGITAB.VECKOKORD = korvar AND
   TIDREGITAB.DATUM <= vkdatum
   USE-INDEX PSTART NO-LOCK. 
   
   
   GET FIRST tidq NO-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB): 
      IF (TIDREGITAB.TRAKTKOD = " "  OR TIDREGITAB.TRAKTANTAL = 0) AND
         (TIDREGITAB.LONTILLAGG = " "  OR TIDREGITAB.LONTILLANTAL = 0) AND
         (TIDREGITAB.OKOD1 = " " OR TIDREGITAB.OANT1 = 0) AND
         (TIDREGITAB.OKOD2 = " " OR TIDREGITAB.OANT2 = 0) AND
         (TIDREGITAB.OKOD3 = " " OR TIDREGITAB.OANT3 = 0) AND
         (TIDREGITAB.BEREDSKAP = " " OR TIDREGITAB.BERANTAL = 0)
      THEN DO:
         IF (PERSONALTAB.BEFATTNING = "TIMANST�LLD" OR 
         PERSONALTAB.BEFATTNING = "LOKALV�RDARE" OR 
         PERSONALTAB.BEFATTNING = "PRAKTIKANT") AND TIDREGITAB.TOTALT > 0 
         THEN DO:  
            IF TIDREGITAB.PRISTYP = "FR�NVARO." THEN avtal = avtal.
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
        /*Efter samtal med Marie 2004-03-30. Enbart normaltimmar skall �ver
        �vertid och restid g�r �ver med l�nearter*/
        IF (TIDREGITAB.LONTILLAGG = " "  OR TIDREGITAB.LONTILLANTAL = 0) AND
         (TIDREGITAB.OKOD1 = " " OR TIDREGITAB.OANT1 = 0) AND
         (TIDREGITAB.OKOD2 = " " OR TIDREGITAB.OANT2 = 0) AND
         (TIDREGITAB.OKOD3 = " " OR TIDREGITAB.OANT3 = 0)          
        THEN DO:
           IF (PERSONALTAB.BEFATTNING = "TIMANST�LLD" OR PERSONALTAB.BEFATTNING = "LOKALV�RDARE"
      	  OR PERSONALTAB.BEFATTNING = "PRAKTIKANT") AND 
      	  TIDREGITAB.TOTALT > 0 THEN DO: 
      	     IF TIDREGITAB.PRISTYP = "FR�NVARO." THEN avtal = avtal.
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

 /*test*/
/*OUTPUT TO d:\DELAD\server\PRO9S\gkal\filinnan.d.
FOR EACH pa90fil:
  EXPORT pa90fil.
END.*/


/* OBS ! BARA LONTILL p� avtal*/

/*FOR EACH pa90fil where pa90fil.PLONTILLAGG = "881":                          ??? borde vara 781??
   FIND FIRST pabuff WHERE pabuff.PPERSKOD = pa90fil.PPERSKOD AND pabuff.PDATUM = pa90fil.PDATUM
   AND pabuff.PTRAKTKOD NE "" NO-LOCK NO-ERROR.
   IF AVAILABLE pabuff THEN DO:
      DELETE pa90fil.
   END.
END.*/
/*Ta bort kostf�rm�n lart 781 vilart KLUM l�nesystem KOSTLM f�r endags och flerdygnf�rr�ttning. L
�nesystemet genererar automatiskt
Skicka kostf�rm�n bara f�r utlandsresa lart 742 vilart UTLT l�nesystem TRAKTHEL*/
FOR EACH pa90fil where pa90fil.PLONTILLAGG = "781":
   FIND FIRST pabuff WHERE pabuff.PPERSKOD = pa90fil.PPERSKOD AND pabuff.PDATUM = pa90fil.PDATUM
   AND pabuff.PLONTILLAGG = "742" NO-LOCK NO-ERROR.
   IF NOT AVAILABLE pabuff THEN DO:
      DELETE pa90fil.
   END.
END.

FOR EACH pa90fil where pa90fil.PLONTILLAGG NE "":
   FIND FIRST LONTILL WHERE LONTILL.LONTILLAGG = pa90fil.PLONTILLAGG
   AND LONTILL.KOD = pa90fil.PKOD USE-INDEX LON NO-LOCK NO-ERROR.
   IF AVAILABLE LONTILL THEN ASSIGN pa90fil.PLONTILLAGG = LONTILL.VILART.
END.
FOR EACH pa90fil where pa90fil.POVERTIDTILL NE "":
   FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = pa90fil.POVERTIDTILL
   /*AND OVERKOD.KOD = pa90fil.PAVTAL*/ USE-INDEX OVER NO-LOCK NO-ERROR.
   IF AVAILABLE OVERKOD THEN ASSIGN pa90fil.POVERTIDTILL = OVERKOD.VILART.
END.
FOR EACH pa90fil where pa90fil.PBEREDSKAP NE "":
   FIND FIRST BERKOD WHERE BERKOD.BEREDSKAP = pa90fil.PBEREDSKAP 
   /*AND BERKOD.BEREDSKAPSAVTAL = pa90fil.PAVTAL*/ USE-INDEX BERE NO-LOCK NO-ERROR.
   IF AVAILABLE BERKOD THEN ASSIGN pa90fil.PBEREDSKAP = BERKOD.VILART.
END.
FOR EACH pa90fil where pa90fil.PTRAKTKOD NE "":
   FIND FIRST TRAKTATAB WHERE TRAKTATAB.TRAKTKOD = pa90fil.PTRAKTKOD
   /*AND TRAKTATAB.TRAAVTAL = pa90fil.PAVTAL*/ USE-INDEX TRAAV NO-LOCK NO-ERROR.
   IF AVAILABLE TRAKTATAB THEN ASSIGN pa90fil.PTRAKTKOD = TRAKTATAB.VILART.
END.
/*test*/
/*OUTPUT TO d:\DELAD\server\PRO9S\gkal\filefter.d.
FOR EACH pa90fil:
  EXPORT pa90fil.
END.*/

RUN GKALVE2.P.      /* summerar veckans sk�rd till tidigare */
RUN sammut_UI (INPUT 2).
{EUROPEANAMERICAN.I}