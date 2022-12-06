/*NATTGMAN.P*/ 
/*GAMLA DAGAR*/
/*DEFINE NEW SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/

DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.     
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER NO-UNDO.    
DEFINE NEW SHARED VARIABLE regstartsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE regslutsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE fldrec AS RECID NO-UNDO. 
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE regtotalt LIKE TIDREGITAB.TOTALT NO-UNDO.
DEFINE NEW SHARED VARIABLE frustarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE fruslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffestart AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffeslut AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchstarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE dagnr AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE NEW SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE VARIABLE minlu AS INTEGER NO-UNDO.    
DEFINE VARIABLE lusta AS INTEGER NO-UNDO.
DEFINE VARIABLE luslu AS INTEGER NO-UNDO.
DEFINE VARIABLE vschemat LIKE VECKOARBAV.VECKOSCHEMA NO-UNDO.  
DEFINE VARIABLE total AS INTEGER NO-UNDO. 
DEFINE VARIABLE flexrec AS RECID NO-UNDO. 
DEFINE VARIABLE fltid AS INTEGER NO-UNDO. 
DEFINE VARIABLE flexkvst LIKE TIDREGITAB.START NO-UNDO.  
DEFINE VARIABLE flrec AS RECID NO-UNDO. 

&Scoped-define NEW
{TIDPERS.I}

DEFINE BUFFER flexbuff FOR FLEXTID.         
FIND FIRST FORETAG NO-LOCK NO-ERROR.
IF AVAILABLE FORETAG THEN Guru.Konstanter:globforetag = FORETAG.FORETAG.
regdatum = TODAY - 1.
RUN REGVEC.P.
OPEN QUERY persq FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK.
GET FIRST persq NO-LOCK.
DO WHILE AVAILABLE(tidpers):                        
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = tidpers.PERSONALKOD
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.         
   persrec = RECID(PERSONALTAB).
   RUN GAMKOLL.P.   
   GET NEXT persq NO-LOCK.
END.     
RUN TEJKSAL.P. 
RUN TEJKSIS.P.

