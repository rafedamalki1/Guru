/*FLAVVAPP.P*/
&Scoped-define NEW NEW
{TIDPERS.I}
DEFINE TEMP-TABLE namntemp
   FIELD EFTERNAMN AS CHARACTER
   FIELD FORNAMN AS CHARACTER  
   FIELD NAMN AS CHARACTER  
   FIELD NTEXT AS CHARACTER
   FIELD REGIS AS CHARACTER  
   FIELD DATUM AS DATE
   FIELD ALLVAL AS INTEGER
   FIELD MANADVAR AS INTEGER.     
&Scoped-define NEW    
&Scoped-define SHARED 
{FLEXTAB.I}
DEFINE INPUT PARAMETER TABLE FOR tidpers.
DEFINE OUTPUT PARAMETER TABLE FOR flexdagtemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR namntemp.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
FIND FIRST tidpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
FIND FIRST namntemp NO-ERROR.
persrec = tidpers.TIDPERSREC.
IF namntemp.ALLVAL = 2 THEN DO:
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = tidpers.OMRADE USE-INDEX OMR NO-LOCK NO-ERROR.
   namntemp.NAMN = OMRADETAB.NAMN.   
END.
IF namntemp.ALLVAL = 3 THEN DO:
   namntemp.REGIS = "Alla".
END.
IF namntemp.ALLVAL = 4 THEN DO:
   namntemp.REGIS = "".
END.
IF namntemp.ALLVAL = 5 THEN namntemp.REGIS = "Markerade enheter".  
IF namntemp.MANADVAR = 0 THEN DO:
   ASSIGN 
   namntemp.NTEXT = "Avvikelselista kontrollerade registreringar tom:". 
END.   
IF namntemp.MANADVAR = 1 THEN DO:
   namntemp.NTEXT = "Avvikelselista EJ kontrollerade registreringar tom:". 
END.   
OPEN QUERY tfq FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK,
EACH FLEXDAG WHERE FLEXDAG.PERSONALKOD = tidpers.PERSONALKOD AND 
FLEXDAG.KORD = 01/01/1997 AND FLEXDAG.FELOK = FALSE AND 
FLEXDAG.DATUM LE namntemp.DATUM USE-INDEX FLEX NO-LOCK.        
GET FIRST tfq NO-LOCK.
DO WHILE AVAILABLE(FLEXDAG):
   CREATE flexdagtemp.
   BUFFER-COPY FLEXDAG TO flexdagtemp.
   ASSIGN
   flexdagtemp.FREC = RECID(FLEXDAG)
   flexdagtemp.FORNAMN = tidpers.FORNAMN
   flexdagtemp.EFTERNAMN = tidpers.EFTERNAMN.
   GET NEXT tfq NO-LOCK.
END.