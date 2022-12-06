/*flexnatt.P*/ 
/*INNEVARANDE DAG*/
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.     
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER NO-UNDO.    
DEFINE NEW SHARED VARIABLE regstartsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE regslutsek AS INTEGER NO-UNDO. 

DEFINE VARIABLE flextot LIKE TIDREGITAB.TOTALT NO-UNDO.
DEFINE VARIABLE fltid AS INTEGER NO-UNDO.  
DEFINE VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.

DEFINE BUFFER flexbuff FOR FLEXTID.         
DEFINE QUERY persfq FOR PERSONALTAB.   
FIND FIRST FORETAG NO-LOCK NO-ERROR.
IF AVAILABLE FORETAG THEN globforetag = FORETAG.FORETAG.

OPEN QUERY persq FOR EACH PERSONALTAB WHERE PERSONALTAB.AKTIV = TRUE 
USE-INDEX PERSONALKOD NO-LOCK.
GET FIRST persq NO-LOCK.
DO WHILE AVAILABLE(PERSONALTAB):             
   FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD 
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.         
   IF AVAILABLE FLEXAVT AND FLEXAVT.FLEXTID = TRUE THEN DO:
      OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
      AND TIDREGITAB.VECKOKORD = "" AND TIDREGITAB.TIDLOG = TRUE 
      AND TIDREGITAB.DATUM > 09/30/98 USE-INDEX PSTART NO-LOCK.   
      GET FIRST tidq NO-LOCK.
      DO WHILE AVAILABLE (TIDREGITAB):    
          IF TIDREGITAB.AONR = "172" THEN DO:                                         
             nytid = TIDREGITAB.TOTALT.
             RUN TIMSEK.P.
             fltid = fltid - sekunder.             
          END. 
          ELSE IF TIDREGITAB.OVERTIDUTTAG = "F" THEN DO:
             IF TIDREGITAB.START GE 6 AND TIDREGITAB.START < 8 THEN DO:                                                                     
                nytid = TIDREGITAB.TOTALT.
                RUN TIMSEK.P.
                fltid = fltid + sekunder.                       
             END.
             ELSE IF TIDREGITAB.SLUT GE 15 AND TIDREGITAB.SLUT LE 18 THEN DO:                
                IF TIDREGITAB.SLUT > 18 THEN DO:
                   nytid = TIDREGITAB.START.
                   RUN TIMSEK.P.
                   ASSIGN
                   regstartsek = sekunder
                   nytid = 18.00.
                   RUN TIMSEK.P.
                   ASSIGN
                   regslutsek = sekunder.                          
                   RUN TOTTID.P.
                   ASSIGN flextot = nytid.                                   
                END.   
                ELSE DO:                       
                   ASSIGN flextot = TIDREGITAB.TOTALT.
                END.  
                ASSIGN                    
                nytid = flextot.
                RUN TIMSEK.P.
                fltid = fltid + sekunder.              
             END.
          END.   
          GET NEXT tidq NO-LOCK.
      END.   
      sekunder = fltid.
      RUN FSEKTIM.P.
      FIND FIRST FLEXSALDO WHERE FLEXSALDO.PERSONALKOD = PERSONALTAB.PERSONALKOD
      USE-INDEX PKOD EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE FLEXSALDO THEN DO:
         CREATE FLEXSALDO.
         ASSIGN FLEXSALDO.PERSONALKOD = PERSONALTAB.PERSONALKOD.
      END.   
      ASSIGN FLEXSALDO.EJKORDFLEX = fnytid.
      ASSIGN fltid = 0.
   END.         
   GET NEXT persq NO-LOCK.
END.      
