/*flmanNO.p flexmanadskorning nordkraft */ 
/*INNEVARANDE DAG*/
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.

DEFINE  new SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.     
DEFINE new SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE new SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE new SHARED VARIABLE sekunder AS INTEGER NO-UNDO.    
DEFINE new SHARED VARIABLE regstartsek AS INTEGER NO-UNDO.
DEFINE new SHARED VARIABLE regslutsek AS INTEGER NO-UNDO. 
DEFINE new SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE new SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE new SHARED VARIABLE persrec AS RECID NO-UNDO.

DEFINE VARIABLE flextot LIKE TIDREGITAB.TOTALT NO-UNDO.
DEFINE VARIABLE fltid AS INTEGER NO-UNDO. 
DEFINE VARIABLE flmtid AS INTEGER NO-UNDO. 
DEFINE VARIABLE flejkord AS INTEGER NO-UNDO. 
DEFINE VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE VARIABLE avdrag AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE VARIABLE utbet AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE VARIABLE accflex AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE VARIABLE pflex AS INTEGER NO-UNDO.    
DEFINE TEMP-TABLE pa90sum
   FIELD PPERSONNUMMER LIKE PERSONALTAB.PERSONNUMMER
   FIELD PLONTILLAGG LIKE TIDREGITAB.LONTILLAGG
   FIELD PLONTILLANTAL LIKE TIDREGITAB.LONTILLANTAL
   FIELD POVERTIDTILL LIKE TIDREGITAB.OVERTIDTILL
   FIELD POVERANTAL LIKE TIDREGITAB.OVERANTAL
   FIELD PTRAKTKOD LIKE TIDREGITAB.TRAKTKOD
   FIELD PTRAKTANTAL LIKE TIDREGITAB.TRAKTANTAL
   FIELD PBEREDSKAP LIKE TIDREGITAB.BEREDSKAP
   FIELD PBERANTAL AS DECIMAL FORMAT "999.99"
   FIELD PDATUM LIKE TIDREGITAB.DATUM
   FIELD PVECKONUMMER LIKE TIDREGITAB.VECKONUMMER
   FIELD PSORT AS CHARACTER FORMAT "XX"
   FIELD PPAR8 AS DECIMAL
   FIELD PANSTNR LIKE PERSONALTAB.ANSTNR.
  /* FIELD PVECKONUMMER LIKE TIDREGITAB.VECKONUMMER
   FIELD PPAR8 AS DECIMAL.*/
   DEFINE VARIABLE ST AS INTEGER.

DEFINE BUFFER flexbuff FOR FLEXTID.         
DEFINE QUERY persfq FOR PERSONALTAB.   
FIND FIRST FORETAG NO-LOCK NO-ERROR.
IF AVAILABLE FORETAG THEN globforetag = FORETAG.FORETAG.
vkdatum = 01/31/99.
regdatum = vkdatum.
RUN REGVEC.P.
FOR EACH pa90sum:
   DELETE pa90sum.
END.
OPEN QUERY persq FOR EACH PERSONALTAB WHERE PERSONALTAB.AKTIV = TRUE 
USE-INDEX PERSONALKOD NO-LOCK.
GET FIRST persq NO-LOCK.
DO WHILE AVAILABLE(PERSONALTAB):             
   persrec = RECID(PERSONALTAB).
   FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD 
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.         
   IF AVAILABLE FLEXAVT AND FLEXAVT.FLEXTID = TRUE THEN DO:
      OPEN QUERY tidq FOR EACH TIDREGITAB WHERE
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.TIDLOG = TRUE AND
      TIDREGITAB.GODKAND NE "  " AND TIDREGITAB.VECKOKORD = " " AND
      TIDREGITAB.DATUM <= vkdatum
      USE-INDEX PSTART NO-LOCK. 
      GET FIRST tidq NO-LOCK.
      DO WHILE AVAILABLE (TIDREGITAB):          
          /*IF TIDREGITAB.GODKAND = "" THEN DO:
              regdatum = TIDREGITAB.DATUM.
	       regvnr = TIDREGITAB.VECKONUMMER.
	       RUN SLUTARB.P.
              IF TIDREGITAB.AONR = "910927" THEN DO:                                         
                 nytid = TIDREGITAB.TOTALT.
                 RUN TIMSEK.P.
                 flejkord = flejkord - sekunder.             
              END. 
              ELSE IF TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.LONTILLAGG = "" THEN DO:
                 IF regstart = regslut THEN DO:
                    nytid = TIDREGITAB.TOTALT.
                    RUN TIMSEK.P.
                    flejkord = flejkord + sekunder.        
                 END.   
                 ELSE IF TIDREGITAB.START < regstart THEN DO:                                                                     
                    nytid = TIDREGITAB.TOTALT.
                    RUN TIMSEK.P.
                    flejkord = flejkord + sekunder.                       
                 END.
                 ELSE IF TIDREGITAB.START GE regslut THEN DO:                                                                             
                    ASSIGN flextot = TIDREGITAB.TOTALT.                      
                    ASSIGN                    
                    nytid = flextot.
                    RUN TIMSEK.P.
                    flejkord = flejkord + sekunder.              
                 END.
              END.   
          END.    
          ELSE DO:*/
              regdatum = TIDREGITAB.DATUM.
	       regvnr = TIDREGITAB.VECKONUMMER.
	       RUN SLUTARB.P.
              IF TIDREGITAB.AONR = "910927" THEN DO:                                         
                 nytid = TIDREGITAB.TOTALT.
                 RUN TIMSEK.P.
                 fltid = fltid - sekunder.             
              END. 
              ELSE IF TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.LONTILLAGG = "" THEN DO:
                 IF regstart = regslut THEN DO:
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
                    ASSIGN flextot = TIDREGITAB.TOTALT.                     
                    ASSIGN                    
                    nytid = flextot.
                    RUN TIMSEK.P.
                    fltid = fltid + sekunder.              
                 END.
              END.   
          /*END.*/   

          GET NEXT tidq NO-LOCK.
      END.   
      IF fltid NE 0 THEN DO:
         sekunder = fltid.
         RUN FSEKTIM.P.            
         CREATE pa90sum.
         ASSIGN pa90sum.PPERSONNUMMER = PERSONALTAB.PERSONNUMMER
         pa90sum.PLONTILLAGG = "378"
         pa90sum.PLONTILLANTAL = fnytid
         pa90sum.PSORT = "TI"
         pa90sum.PDATUM = vkdatum.	 
      END.   
 
       
   /*   FIND FIRST FLEXSALDO WHERE FLEXSALDO.PERSONALKOD = PERSONALTAB.PERSONALKOD
      USE-INDEX PKOD EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE FLEXSALDO THEN DO:
         CREATE FLEXSALDO.
         ASSIGN FLEXSALDO.PERSONALKOD = PERSONALTAB.PERSONALKOD.
      END.   
      ASSIGN FLEXSALDO.BACFLEX = FLEXSALDO.PERIODFLEX.  
      sekunder = fltid + flmtid. 
      pflex  = sekunder.
      RUN FSEKTIM.P.
      ASSIGN FLEXSALDO.PERIODFLEX = fnytid.
      nytid = FLEXSALDO.ACCFLEX.
      RUN TIMSEK.P.
      sekunder = sekunder + pflex.
      RUN FSEKTIM.P.      
      accflex = fnytid.
      IF  accflex > 60 THEN DO:
         nytid = accflex - 60.       
         RUN TIMSEK.P.
         sekunder = fltid + flmtid - sekunder.
         RUN FSEKTIM.P.
         ASSIGN FLEXSALDO.PERIODFLEX = fnytid.         
         ASSIGN FLEXSALDO.ACCFLEX = 60.
      END.   
      ELSE IF accflex > -10 THEN DO:
         nytid = accflex + 10.       
         RUN TIMSEK.P.
         sekunder = fltid + flmtid - sekunder.
         RUN FSEKTIM.P.
         ASSIGN FLEXSALDO.PERIODFLEX = fnytid.           
         ASSIGN FLEXSALDO.ACCFLEX = -10.
      END. 
      ELSE ASSIGN FLEXSALDO.ACCFLEX = accflex.
      
      sekunder = flejkord.
      RUN FSEKTIM.P.
      ASSIGN
      FLEXSALDO.EJKORDFLEX = fnytid.
      ASSIGN fltid = 0
      flejkord = 0.*/
   END.         
   GET NEXT persq NO-LOCK.
END.
FOR EACH pa90sum WHERE pa90sum.PPERSONNUMMER = " ":
  DELETE pa90sum.
END.
      
IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:   
   IF globforetag = "NORD" THEN DO:      
      OUTPUT TO P:\progress\flex.d NO-ECHO.
   END.
   ELSE DO:   
      OUTPUT TO C:\GURU\flex.d NO-ECHO.     
   END.      
END.
	  
FOR EACH pa90sum
BY pa90sum.PPERSONNUMMER BY pa90sum.PLONTILLAGG BY pa90sum.POVERTIDTILL
BY pa90sum.PTRAKTKOD BY pa90sum.PBEREDSKAP:
   EXPORT pa90sum.
END.
IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:   
   IF globforetag = "NORD" THEN DO:
      OUTPUT TO P:\progress\pasumma.d APPEND NO-ECHO.    
   END.
   ELSE DO:
      OUTPUT TO C:\GURU\pasumma.d APPEND NO-ECHO. 
   END.      
END.
	  
FOR EACH pa90sum
BY pa90sum.PPERSONNUMMER BY pa90sum.PLONTILLAGG BY pa90sum.POVERTIDTILL
BY pa90sum.PTRAKTKOD BY pa90sum.PBEREDSKAP:
   EXPORT pa90sum.
END.

FOR EACH pa90sum:
   DELETE pa90sum.
END.
