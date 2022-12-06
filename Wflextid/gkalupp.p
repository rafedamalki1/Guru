/*gkalupp.p*/
DEFINE NEW SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
/*DEFINE NEW SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/


DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE NEW SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE NEW SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE frustarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE fruslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffestart AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffeslut AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchstarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE regtotalt LIKE TIDREGITAB.TOTALT NO-UNDO.   
DEFINE NEW SHARED VARIABLE regstartsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE regslutsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.


DEFINE TEMP-TABLE flexen
   FIELD PLONTILLANTAL AS DECIMAL FORMAT "-999.99"
   FIELD PDATUM LIKE TIDREGITAB.DATUM
   FIELD PKOD LIKE PERSONALTAB.PERSONALKOD
   INDEX PKOD IS PRIMARY PKOD ASCENDING.   
DEFINE TEMP-TABLE flexsum
   FIELD PLONTILLANTAL AS DECIMAL FORMAT "-999.99"
   FIELD PLONTILLSEK AS INTEGER
   FIELD PKOD LIKE PERSONALTAB.PERSONALKOD
   INDEX PKOD IS PRIMARY PKOD ASCENDING.   

DEFINE VARIABLE plflex AS DECIMAL NO-UNDO.
DEFINE VARIABLE seku AS INTEGER NO-UNDO.
DEFINE VARIABLE arrflex AS INTEGER NO-UNDO.
DEFINE VARIABLE pkod AS CHARACTER NO-UNDO.
&Scoped-define NEW NEW
{TIDPERS.I}

DEFINE INPUT PARAMETER TABLE FOR tidpers.  

RUN ejkordflexkal_UI.
PROCEDURE ejkordflexkal_UI :
   OPEN QUERY pq FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK.   
   GET FIRST pq NO-LOCK.
   DO WHILE AVAILABLE(tidpers):
      persrec = RECID(tidpers).
      FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = tidpers.PERSONALKOD 
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.            
      IF AVAILABLE FLEXAVT AND FLEXAVT.FLEXTID = TRUE THEN DO TRANSACTION:
         FIND FIRST FLEXSALDO WHERE FLEXSALDO.PERSONALKOD = tidpers.PERSONALKOD 
         EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE FLEXSALDO THEN DO:
            ASSIGN 
            FLEXSALDO.EJKORDFLEX = 0.   
         END.   
      END.   
      OPEN QUERY tq FOR EACH TIDREGITAB WHERE
      TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD
      AND TIDLOG = TRUE
      AND TIDREGITAB.VECKOKORD = "" USE-INDEX PSTART NO-LOCK.    
      GET FIRST tq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):
         IF AVAILABLE FLEXAVT AND FLEXAVT.FLEXTID = TRUE THEN DO:
   	      regdatum = TIDREGITAB.DATUM.
   	      regvnr = TIDREGITAB.VECKONUMMER.
            pkod = tidpers.PERSONALKOD.
   	      RUN SLUTARBW.P 
            (INPUT-OUTPUT pkod,INPUT-OUTPUT regstart,INPUT-OUTPUT regslut, 
            INPUT-OUTPUT regvnr,INPUT-OUTPUT regdagnamn,INPUT-OUTPUT regdatum, 
            INPUT-OUTPUT regtotalt,INPUT-OUTPUT frustarten,INPUT-OUTPUT fruslutet, 
            INPUT-OUTPUT kaffestart,INPUT-OUTPUT kaffeslut,INPUT-OUTPUT lunchstarten,
            INPUT-OUTPUT lunchslutet,INPUT-OUTPUT nytid,INPUT-OUTPUT sekunder). 
   	      musz = FALSE.
   	      IF TIDREGITAB.AONR = "155" THEN DO:
               nytid = TIDREGITAB.TOTALT.
               RUN TIMSEK.P.           
               CREATE flexen.
               ASSIGN flexen.PKOD = tidpers.PERSONALKOD                 
               flexen.PLONTILLANTAL = (-1) * sekunder
               flexen.PDATUM = TIDREGITAB.DATUM.             
            END.
            ELSE IF TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.LONTILLAGG = "" THEN DO:
               IF TIDREGITAB.AONR = "155" THEN musz = musz.
               ELSE IF regstart = regslut THEN musz = TRUE.
               ELSE IF TIDREGITAB.START < regstart THEN musz = TRUE.                                                                                     
               ELSE IF TIDREGITAB.SLUT > regslut THEN musz = TRUE.                                                             
            END.  
            IF musz = TRUE THEN DO:              
               musz = FALSE.
               IF TIDREGITAB.START < regstart THEN DO:
                  IF TIDREGITAB.SLUT > regstart THEN DO:                          
                     nytid = TIDREGITAB.START.
                     RUN TIMSEK.P.
                     seku = sekunder.
                     nytid = regstart.
                     RUN TIMSEK.P.
                     sekunder = sekunder - seku.
                     RUN SEKTIM.P.                   
                     plflex = nytid.
                  END.
                  ELSE DO:                     
                     plflex = TIDREGITAB.TOTALT.
                  END.
                  nytid = plflex.
                  RUN TIMSEK.P.           
                  CREATE flexen.
                  ASSIGN flexen.PKOD = tidpers.PERSONALKOD
                  flexen.PLONTILLANTAL = sekunder
                  flexen.PDATUM = TIDREGITAB.DATUM.             
               END.
               IF TIDREGITAB.SLUT > regslut THEN DO:
                  IF TIDREGITAB.START < regslut THEN DO:                  
                     nytid = TIDREGITAB.SLUT.
                     RUN TIMSEK.P.
                     seku = sekunder.
                     nytid = regslut.
                     RUN TIMSEK.P.
                     sekunder = seku - sekunder.
                     RUN SEKTIM.P.                     
                     plflex = nytid.
                  END.
                  ELSE DO:                      
                      plflex = TIDREGITAB.TOTALT.
                  END.                        
                  nytid = plflex.
                  RUN TIMSEK.P.           
                  CREATE flexen.
                  ASSIGN flexen.PKOD = tidpers.PERSONALKOD
                  flexen.PLONTILLANTAL = sekunder
                  flexen.PDATUM = TIDREGITAB.DATUM.             
               END.               
            END.               
         END.   	           
         GET NEXT tq NO-LOCK.
      END.   
      GET NEXT pq NO-LOCK.
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
   FOR EACH flexsum:            
      DO TRANSACTION:
         FIND FIRST FLEXSALDO WHERE FLEXSALDO.PERSONALKOD = flexsum.PKOD 
         EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE FLEXSALDO THEN DO:
            CREATE FLEXSALDO.
            ASSIGN FLEXSALDO.PERSONALKOD = flexsum.PKOD.
         END.   
         ASSIGN 
         FLEXSALDO.EJKORDFLEX = flexsum.PLONTILLANTAL.
      END.   
   END.   
END PROCEDURE.
