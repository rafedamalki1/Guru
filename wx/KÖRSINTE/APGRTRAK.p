 /*APGRTRAK.P*/
&Scoped-define NEW NEW
{TIDPERS.I}
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}

FIND FIRST FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
RUN STYRFORE.P (INPUT globforetag).
DEFINE TEMP-TABLE mankoll    
   FIELD PERSONALKOD LIKE  PERSONALTAB.PERSONALKOD 
   FIELD AR AS INTEGER
   FIELD MANADNR AS INTEGER
   INDEX PKOD IS PRIMARY PERSONALKOD AR MANADNR.
DEFINE TEMP-TABLE traktid   
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD EFTERNAMN LIKE    PERSONALTAB.EFTERNAMN 
   FIELD FORNAMN LIKE      PERSONALTAB.FORNAMN   
   FIELD AONR LIKE TIDREGITAB.AONR   
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD TRAKTAMENTE LIKE TIDREGITAB.TRAKTAMENTE 
   FIELD MED AS LOGICAL                     
   FIELD ORT LIKE AONRTAB.ORT     
   FIELD VECKONUMMER LIKE TIDREGITAB.VECKONUMMER 
   FIELD DAG LIKE TIDREGITAB.DAG                     
   FIELD DATUM LIKE TIDREGITAB.DATUM
   FIELD AONRTRAKT LIKE AONRTAB.TRAKTAMENTE
   INDEX AONR AONR DELNR PERSONALKOD DATUM
   INDEX PKOD PERSONALKOD DATUM AONR.

{TIDUTTT.I}
DEFINE BUFFER traktidbuff FOR traktid.          
DEFINE BUFFER tidbuf FOR TIDREGITAB.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE NEW SHARED VARIABLE regar AS INTEGER FORMAT "99" NO-UNDO.
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE regmnr AS INTEGER FORMAT "99" NO-UNDO.
DEFINE NEW SHARED VARIABLE regmannamn AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE VARIABLE listmnr AS INTEGER FORMAT "99" NO-UNDO.
DEFINE VARIABLE listar AS INTEGER FORMAT "99" NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(90)" NO-UNDO.

DEFINE INPUT PARAMETER cgvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER indatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER inregvnr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tidpers.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.
gvisatidpermanad = cgvisatidpermanad.
regvnr = inregvnr.
regdatum = indatum.
str="==========.=======================.===.===========.==============================".      
RUN huvud_UI. 

   {GDPRLOGGCLIENT.I}

PROCEDURE huvud_UI :
     /*HUVUD*/ 
   DO TRANSACTION:    
      CREATE tidut.                   
      ASSIGN
      SUBSTRING(tidut.UT,1) = "GRANSKNING AV ÄNDRADE TRAKTAMENTSZONER"
      SUBSTRING(tidut.UT,60) = STRING(TODAY)
      SUBSTRING(tidut.UT,70) = STRING(TIME,"HH:MM:SS").
      CREATE tidut.                   
      ASSIGN
      SUBSTRING(tidut.UT,1) = 
      "DÅ AONR:ET EJ HAR TRAKTAMENTSZON MEN TIDREGISTRERINGEN HAR TRAKTAMENTSZON".  
      CREATE tidut. 
      IF gvisatidpermanad = TRUE THEN DO:         
         SUBSTRING(tidut.UT,1) = "ÅR " + STRING(regar,"9999") + " MÅNAD " + regmannamn.
      END.
      ELSE DO:            
         ASSIGN
         SUBSTRING(tidut.UT,1) = 
         "VECKONUMMER " + STRING(regvnr).
      END.
      CREATE tidut.                         
      ASSIGN
      SUBSTRING(tidut.UT,1) = "ENHET/SIGN"
      SUBSTRING(tidut.UT,12) = "NAMN"               
      SUBSTRING(tidut.UT,36) = "DAG"              
      SUBSTRING(tidut.UT,40) = CAPS(Guru.Konstanter:gaok)
      SUBSTRING(tidut.UT,52) = "BENÄMNING/ORT".      
      CREATE tidut.  
      ASSIGN tidut.UT = str.   
   END.
   FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK:
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + tidpers.PERSONALKOD. 
      IF gvisatidpermanad = TRUE THEN DO:         
         OPEN QUERY tidq FOR EACH TIDREGITAB WHERE 
         TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
         YEAR(TIDREGITAB.DATUM) = YEAR(regdatum) AND 
         MONTH(TIDREGITAB.DATUM) = MONTH(regdatum) AND 
         TIDREGITAB.TIDLOG = TRUE  
         USE-INDEX PSTART NO-LOCK.      
      END.
      ELSE DO:
         OPEN QUERY tidq FOR EACH TIDREGITAB WHERE 
         TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
         TIDREGITAB.VECKONUMMER = regvnr AND TIDREGITAB.TIDLOG = TRUE  
         USE-INDEX PVNR NO-LOCK.      
      END.

      GET FIRST tidq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):                     
         IF TIDREGITAB.TRAKTAMENTE NE 0 THEN DO:
            FIND FIRST traktid WHERE traktid.PERSONALKOD = tidpers.PERSONALKOD AND
            traktid.DATUM = TIDREGITAB.DATUM AND
            traktid.AONR = TIDREGITAB.AONR AND  
            traktid.DELNR = TIDREGITAB.DELNR NO-LOCK NO-ERROR.
            IF NOT AVAILABLE traktid THEN DO:
               CREATE traktid.
               ASSIGN 
               traktid.PERSONALKOD = tidpers.PERSONALKOD 
               traktid.EFTERNAMN = tidpers.EFTERNAMN 
               traktid.FORNAMN = tidpers.FORNAMN 
               traktid.VECKONUMMER = TIDREGITAB.VECKONUMMER 
               traktid.DAG = TIDREGITAB.DAG     
               traktid.DATUM = TIDREGITAB.DATUM
               traktid.AONR = TIDREGITAB.AONR   
               traktid.DELNR = TIDREGITAB.DELNR
               traktid.TRAKTAMENTE = TIDREGITAB.TRAKTAMENTE 
               traktid.MED = FALSE.                    
            END.           
         END.
         GET NEXT tidq NO-LOCK.         
      END.                    
      CLOSE QUERY tidq.  
   END.              
   FOR EACH traktid WHERE traktid.MED = FALSE USE-INDEX AONR:      
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = traktid.AONR AND 
      AONRTAB.DELNR = traktid.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
      FOR EACH traktidbuff WHERE traktidbuff.AONR = AONRTAB.AONR AND 
      traktidbuff.DELNR = AONRTAB.DELNR AND traktidbuff.MED = FALSE USE-INDEX AONR:
         ASSIGN
         traktidbuff.MED = TRUE    
         traktidbuff.ORT = AONRTAB.ORT
         traktidbuff.AONRTRAKT = AONRTAB.TRAKTAMENTE.
      END.                                        
   END.  
   FOR EACH traktid USE-INDEX PKOD NO-LOCK:
      CREATE tidut.                         
      ASSIGN
      SUBSTRING(tidut.UT,1) = traktid.PERSONALKOD
      SUBSTRING(tidut.UT,12) = SUBSTRING(traktid.FORNAMN,1,1)       
      SUBSTRING(tidut.UT,13) = "."
      SUBSTRING(tidut.UT,15) = SUBSTRING(traktid.EFTERNAMN,1,20)      
      SUBSTRING(tidut.UT,36) = traktid.DAG 
      SUBSTRING(tidut.UT,40) = traktid.AONR  
      SUBSTRING(tidut.UT,47) = STRING(traktid.DELNR,Guru.Konstanter:varforetypchar[1]) 
      SUBSTRING(tidut.UT,52) = SUBSTRING(traktid.ORT,1,30).
      IF gvisatidpermanad = TRUE THEN DO:
         SUBSTRING(tidut.UT,36,3) = "   ".
         SUBSTRING(tidut.UT,36,2) = SUBSTRING(STRING(traktid.DATUM,"999999"),5,2).
      END.  
   END.
   FIND LAST tidut NO-LOCK NO-ERROR.
END PROCEDURE.

