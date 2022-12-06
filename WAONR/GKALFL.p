 /*GKALFL.P*/

{LESAMMAN.I}
DEFINE VARIABLE globanv AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER kordatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ganv LIKE ANVANDARE.ANVANDARE NO-UNDO.
RUN sammut_UI (INPUT 1).
 &Scoped-define NEW NEW
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.   
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.     
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE NEW SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
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


DEFINE VARIABLE personal LIKE PERSONALTAB.PERSONALKOD.      /*LÖN*/
DEFINE VARIABLE flex AS LOGICAL NO-UNDO.
DEFINE VARIABLE fltid AS INTEGER NO-UNDO.
DEFINE VARIABLE seku AS INTEGER NO-UNDO.
DEFINE VARIABLE totsal AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE VARIABLE diff AS DECIMAL FORMAT "-99.99" NO-UNDO.


DEFINE VARIABLE arrflex AS INTEGER NO-UNDO.

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
{AMERICANEUROPEAN.I}
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
vkdatum = kordatum.
globanv = ganv.
EMPTY TEMP-TABLE flexen NO-ERROR. 
OPEN QUERY pq FOR EACH PERSONALTAB WHERE PERSONALTAB.PERSMASK = TRUE
USE-INDEX PERSONALKOD NO-LOCK.
GET FIRST pq NO-LOCK.
DO WHILE AVAILABLE(PERSONALTAB):
   personal = PERSONALTAB.PERSONALKOD.
   
   persrec = RECID(PERSONALTAB).
   FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD 
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.            
   IF AVAILABLE FLEXAVT AND FLEXAVT.FLEXTID = TRUE THEN DO:
      OPEN QUERY tq FOR EACH TIDREGITAB WHERE
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.GODKAND BEGINS "G" AND TIDREGITAB.VECKOKORD = "" AND
      TIDREGITAB.DATUM <= vkdatum
      USE-INDEX PSTART NO-LOCK.       
      GET FIRST tq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):
         /* om flextid skall räknas ut*/ 
    	   flex = FALSE.
         regdatum = TIDREGITAB.DATUM.
         regvnr = TIDREGITAB.VECKONUMMER.
         RUN SLUTARB.P.
         musz = FALSE.
         IF TIDREGITAB.AONR = "155" THEN DO:
            nytid = TIDREGITAB.TOTALT.
            RUN TIMSEK.P.
            CREATE flexen.
            ASSIGN flexen.PKOD = PERSONALTAB.PERSONALKOD
            flexen.PLONTILLANTAL = (-1) * sekunder
            flexen.PDATUM = TIDREGITAB.DATUM.
         END.          
         IF TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.LONTILLAGG = "" THEN DO:
            IF TIDREGITAB.AONR = "155" THEN fltid = fltid.
            ELSE IF regstart = regslut THEN DO:
               nytid = TIDREGITAB.TOTALT.
               RUN TIMSEK.P.
               fltid = fltid + sekunder.      
            END.   
            ELSE DO:            
               IF TIDREGITAB.START < regstart THEN DO:   
                  IF TIDREGITAB.SLUT > regstart THEN DO:                  
                     nytid = TIDREGITAB.START.
                     RUN TIMSEK.P.
                     seku = sekunder.
                     nytid = regstart.
                     RUN TIMSEK.P.
                     sekunder = sekunder - seku.
                     fltid = fltid + sekunder.                                                       
                  END.
                  ELSE DO:
                      nytid = TIDREGITAB.TOTALT.                  
                      RUN TIMSEK.P.
                      fltid = fltid + sekunder.
                  END.
               END.
               IF TIDREGITAB.SLUT GE regslut THEN DO:                                                             
                  IF TIDREGITAB.START < regslut THEN DO:                     
                     nytid = TIDREGITAB.SLUT.
                     RUN TIMSEK.P.
                     seku = sekunder.
                     nytid = regslut.
                     RUN TIMSEK.P.
                     sekunder = seku - sekunder.
                     fltid = fltid + sekunder.                                   
                  END.
                  ELSE DO:
                     nytid = TIDREGITAB.TOTALT.
                     RUN TIMSEK.P.
                     fltid = fltid + sekunder.                                   
                  END.                        
               END.
            END.  
         END.
         IF fltid NE 0 THEN DO:               
            sekunder = fltid.
            RUN FSEKTIM.P.                          
            CREATE flexen.
            ASSIGN flexen.PKOD = PERSONALTAB.PERSONALKOD
            flexen.PLONTILLANTAL = sekunder
            flexen.PDATUM = TIDREGITAB.DATUM.
            fltid = 0. 
            flex = TRUE.
         END.   
         GET NEXT tq.
      END.   	                    
   END.
   GET NEXT pq.
END.

if Guru.Konstanter:globforetag = "elpa" then DO:
   
   OUTPUT TO \\pc112\delad\pro9s\korning\flexen.d.
   for each flexen BY flexen.PKOD:
     display flexen.
   end.
   OUTPUT CLOSE.
   
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
         totsal = fnytid.
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
         seku = sekunder.         
         RUN FSEKTIM.P.
         ASSIGN 
         FLEXSALDO.ACCFLEX = fnytid
         FLEXSALDO.PERIODFLEX = flexsum.PLONTILLANTAL.      
         nytid = flexsum.PLONTILLANTAL.
         RUN TIMSEK.P.
         sekunder = sekunder + seku.
         RUN FSEKTIM.P.
         totsal = fnytid.
         IF PERSONALTAB.PERSONALKOD = "16101" THEN musz = musz.
         ELSE IF totsal > 25 THEN DO:          
            diff = totsal - 25.             
            nytid = diff.
            RUN TIMSEK.P.                 
            ASSIGN  seku = sekunder.
            nytid = FLEXSALDO.PERIODFLEX.
            RUN TIMSEK.P.
            sekunder = sekunder - seku.
            RUN FSEKTIM.P.
            ASSIGN FLEXSALDO.PERIODFLEX = fnytid.
            
            CREATE FLBET.
            ASSIGN
            FLBET.ACCFORE = totsal
            FLBET.ACCEFTER = 25
            FLBET.PERSONALKOD = FLEXSALDO.PERSONALKOD
            FLBET.ANVANDARE = "FLEXKÖRNING"
            FLBET.DATUM = vkdatum
            FLBET.TIMMAR = 0 - diff .
         END.
         IF PERSONALTAB.PERSONALKOD = "16101" THEN musz = musz.
         ELSE IF totsal < -10 THEN DO:
            diff = totsal + 10.        
            nytid = diff.
            RUN TIMSEK.P.                 
            ASSIGN  seku = sekunder.
            nytid = FLEXSALDO.PERIODFLEX.
            RUN TIMSEK.P.
            sekunder = sekunder - seku.
            RUN FSEKTIM.P.
            ASSIGN FLEXSALDO.PERIODFLEX = fnytid.
            CREATE FLBET.
            ASSIGN
            FLBET.ACCFORE = totsal
            FLBET.ACCEFTER = -10
            FLBET.PERSONALKOD = FLEXSALDO.PERSONALKOD
            FLBET.ANVANDARE = "FLEXKÖRNING"
            FLBET.DATUM = vkdatum
            FLBET.TIMMAR = 0 - diff.
   
         END.
      END.       
   END.   
   GET NEXT pq NO-LOCK.
END.   
   
     
if Guru.Konstanter:globforetag = "ELPA" then DO:
   
   OUTPUT TO \\pc112\delad\pro9s\korning\flexsum.d.
   for each flexsum BY flexsum.PKOD:
     display flexsum.
   end.
   OUTPUT CLOSE.
   
END.
RUN sammut_UI (INPUT 2).
{EUROPEANAMERICAN.I}
