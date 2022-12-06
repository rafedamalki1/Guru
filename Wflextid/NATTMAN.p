/*NATTMAN.P*/ 
/*INNEVARANDE DAG*/
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

DEFINE VARIABLE vschemat LIKE VECKOARBAV.VECKOSCHEMA NO-UNDO.  
DEFINE VARIABLE total AS INTEGER NO-UNDO. 
DEFINE VARIABLE flexrec AS RECID NO-UNDO. 
DEFINE VARIABLE flexkvst LIKE TIDREGITAB.START NO-UNDO.     
DEFINE VARIABLE fltid AS INTEGER NO-UNDO. 
DEFINE VARIABLE flrec3 AS RECID NO-UNDO. 
DEFINE VARIABLE persrak AS INTEGER NO-UNDO.

DEFINE BUFFER flexbuff FOR FLEXTID.         
DEFINE QUERY persfq FOR PERSONALTAB. 
&Scoped-define NEW NEW
{TIDPERS.I}

DEFINE INPUT PARAMETER TABLE FOR tidpers.  
FIND FIRST FORETAG NO-LOCK NO-ERROR.
IF AVAILABLE FORETAG THEN Guru.Konstanter:globforetag = FORETAG.FORETAG.
persrak = 0.
FOR EACH tidpers NO-LOCK:
   persrak = persrak + 1.
END.
IF TIME > 68400 THEN ASSIGN regdatum = TODAY.
ELSE regdatum = TODAY - 1.
/*f�r att det ska g� att f�rdigst�lla  tid fram�t. De l�gger in en utst�mpling f�r kv�llen i f�rv�g coh har registrerat resten av m�naden*/
IF persrak = 1 THEN ASSIGN regdatum = TODAY.
RUN REGVEC.P.
OPEN QUERY persq FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK.
GET FIRST persq NO-LOCK.
DO WHILE AVAILABLE(tidpers):                                           
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = tidpers.PERSONALKOD
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   persrec = RECID(PERSONALTAB).
   RUN FDAGKOLL.P.         
   GET NEXT persq NO-LOCK.
END.      
RUN NATTGMAN.P.