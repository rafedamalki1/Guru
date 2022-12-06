 /*APGRTILL.P*/
&Scoped-define NEW NEW
{TIDPERS.I}

DEFINE TEMP-TABLE mankoll    
   FIELD PERSONALKOD LIKE  PERSONALTAB.PERSONALKOD 
   FIELD AR AS INTEGER
   FIELD MANADNR AS INTEGER
   INDEX PKOD IS PRIMARY PERSONALKOD AR MANADNR.
{TIDUTTT.I}
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
DEFINE INPUT PARAMETER gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER indatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER inregvnr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tidpers.
DEFINE INPUT PARAMETER gkand AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.
regvnr = inregvnr.
regdatum = indatum.
str =                                                                                                              
"===============================================================================================================".   
RUN huvud_UI. 

   {GDPRLOGGCLIENT.I}
PROCEDURE huvud_UI :
     /*HUVUD*/ 
   DO TRANSACTION:    
      CREATE tidut.                   
      IF gkand = FALSE THEN DO:      
         ASSIGN SUBSTRING(tidut.UT,4) = "GRANSKNING AV TILLÄGG".
      END.
      ELSE DO:
         ASSIGN SUBSTRING(tidut.UT,4) = "GRANSKNING AV TILLÄGG FÖR GODKÄNDA ENHETER".
      END.
      ASSIGN
      SUBSTRING(tidut.UT,70) = STRING(TODAY)
      SUBSTRING(tidut.UT,80) = STRING(TIME,"HH:MM:SS").
      CREATE tidut.                         
      ASSIGN
      SUBSTRING(tidut.UT,1) = "ENHET/".
      CREATE tidut.                         
      ASSIGN
      SUBSTRING(tidut.UT,1) = "SIGN"
      SUBSTRING(tidut.UT,9) = "FÖRNAMN"
      SUBSTRING(tidut.UT,25) = "EFTERNAMN"   
      SUBSTRING(tidut.UT,51) = "VNR"  
      SUBSTRING(tidut.UT,55) = "DAG" 
      SUBSTRING(tidut.UT,59) = "LART"
      SUBSTRING(tidut.UT,65) = "SORT"
      SUBSTRING(tidut.UT,71) = "ANTAL"
      SUBSTRING(tidut.UT,77) = "FÖRKLARANDE TEXT".
      IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,50,8) = "DATUM   ".
      CREATE tidut.  
      ASSIGN tidut.UT = str.   
   END.   
   FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK: 
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + tidpers.PERSONALKOD.
      IF gkand = FALSE THEN DO:      
         IF gvisatidpermanad = TRUE THEN DO:         
            OPEN QUERY tidq FOR EACH TIDREGITAB WHERE 
            TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
            YEAR(TIDREGITAB.DATUM) = YEAR(regdatum) AND 
            MONTH(TIDREGITAB.DATUM) = MONTH(regdatum)   
            USE-INDEX PSTART NO-LOCK.
         END.
         ELSE DO:
            OPEN QUERY tidq FOR EACH TIDREGITAB WHERE 
            TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
            TIDREGITAB.VECKONUMMER = regvnr   
            USE-INDEX PVNR NO-LOCK.
         END.
      END.
      ELSE DO:
         IF gvisatidpermanad = TRUE THEN DO:         
            OPEN QUERY tidq FOR EACH TIDREGITAB WHERE 
            TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
            YEAR(TIDREGITAB.DATUM) = YEAR(regdatum) AND 
            MONTH(TIDREGITAB.DATUM) = MONTH(regdatum)   
            AND TIDREGITAB.GODKAND BEGINS "G"
            USE-INDEX PSTART NO-LOCK.
         END.
         ELSE DO:
            OPEN QUERY tidq FOR EACH TIDREGITAB WHERE 
            TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
            TIDREGITAB.VECKONUMMER = regvnr  
            AND TIDREGITAB.GODKAND BEGINS "G"
            USE-INDEX PVNR NO-LOCK.
         END.
      END.
      GET FIRST tidq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):                
         IF TIDREGITAB.TRAKTAUTO = TRUE AND
            TIDREGITAB.LONAUTO = TRUE AND
            TIDREGITAB.OVERAUTO = TRUE THEN DO:              
         END. 
         ELSE DO TRANSACTION: 
            IF TIDREGITAB.LONTILLAGG NE " " AND 
            TIDREGITAB.LONAUTO = NO THEN DO:
               FIND FIRST LONTILL WHERE LONTILL.LONTILLAGG = TIDREGITAB.LONTILLAGG
               USE-INDEX LONTIL NO-LOCK NO-ERROR.
               /* gäller sundsvall men globforetag är inte definierad*/
               IF SUBSTRING(LONTILL.TYPKOD,1,3) = "BIL" THEN musz = musz.
               ELSE DO:                
                  CREATE tidut.               
                  ASSIGN
                  SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                  SUBSTRING(tidut.UT,9) = tidpers.FORNAMN
                  SUBSTRING(tidut.UT,25) = tidpers.EFTERNAMN    
                  SUBSTRING(tidut.UT,51) = STRING(regvnr)
                  SUBSTRING(tidut.UT,55) = TIDREGITAB.DAG  
                  SUBSTRING(tidut.UT,59) = STRING(LONTILL.VILART)
                  SUBSTRING(tidut.UT,65) = LONTILL.ENHET
                  SUBSTRING(tidut.UT,71) = STRING(TIDREGITAB.LONTILLANTAL)
                  SUBSTRING(tidut.UT,77) = LONTILL.LONKODTEXT. 
                  IF TIDREGITAB.LONTILLANTAL < 1 THEN 
                  SUBSTRING(tidut.UT,71) = STRING(TIDREGITAB.LONTILLANTAL,"9.9").                         
                  IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,50,8) = STRING(TIDREGITAB.DATUM,"99/99/99").
               END.
            END.                   
            IF TIDREGITAB.OKOD1 NE " " AND
            TIDREGITAB.OVERAUTO = NO THEN DO:
               FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL =
               TIDREGITAB.OKOD1 USE-INDEX OVER NO-LOCK NO-ERROR.       
               IF SUBSTRING(tidut.UT,59) NE " " THEN CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,9) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,25) = tidpers.EFTERNAMN    
               SUBSTRING(tidut.UT,57) = STRING(regvnr)
               SUBSTRING(tidut.UT,55) = TIDREGITAB.DAG  
               SUBSTRING(tidut.UT,59) = STRING(OVERKOD.VILART)
               SUBSTRING(tidut.UT,65) = "TI"
               SUBSTRING(tidut.UT,71) = STRING(TIDREGITAB.OANT1)
               SUBSTRING(tidut.UT,77) = OVERKOD.LONKODTEXT.     
               IF TIDREGITAB.OANT1 < 1 THEN 
               SUBSTRING(tidut.UT,71) = STRING(TIDREGITAB.OANT1,"9.9").
               IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,50,8) = STRING(TIDREGITAB.DATUM,"99/99/99").
               
            END. 
            IF TIDREGITAB.OKOD2 NE " " AND
            TIDREGITAB.OVERAUTO = NO THEN DO:
               FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL =
               TIDREGITAB.OKOD2 USE-INDEX OVER NO-LOCK NO-ERROR.       
               IF SUBSTRING(tidut.UT,59) NE " " THEN CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,9) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,25) = tidpers.EFTERNAMN    
               SUBSTRING(tidut.UT,51) = STRING(regvnr)
               SUBSTRING(tidut.UT,55) = TIDREGITAB.DAG  
               SUBSTRING(tidut.UT,59) = STRING(OVERKOD.VILART)
               SUBSTRING(tidut.UT,65) = "TI"
               SUBSTRING(tidut.UT,71) = STRING(TIDREGITAB.OANT2)
               SUBSTRING(tidut.UT,77) = OVERKOD.LONKODTEXT.     
               IF TIDREGITAB.OANT2 < 1 THEN 
               SUBSTRING(tidut.UT,71) = STRING(TIDREGITAB.OANT2,"9.9").
               IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,50,8) = STRING(TIDREGITAB.DATUM,"99/99/99").
            END.
            IF TIDREGITAB.OKOD3 NE " " AND
            TIDREGITAB.OVERAUTO = NO THEN DO:
               FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL =
               TIDREGITAB.OKOD3 USE-INDEX OVER NO-LOCK NO-ERROR.       
               IF SUBSTRING(tidut.UT,59) NE " " THEN CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,9) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,25) = tidpers.EFTERNAMN    
               SUBSTRING(tidut.UT,51) = STRING(regvnr)
               SUBSTRING(tidut.UT,55) = TIDREGITAB.DAG  
               SUBSTRING(tidut.UT,59) = STRING(OVERKOD.VILART)
               SUBSTRING(tidut.UT,65) = "TI"
               SUBSTRING(tidut.UT,71) = STRING(TIDREGITAB.OANT3)
               SUBSTRING(tidut.UT,77) = OVERKOD.LONKODTEXT.     
               IF TIDREGITAB.OANT2 < 1 THEN 
               SUBSTRING(tidut.UT,71) = STRING(TIDREGITAB.OANT3,"9.9").
               IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,50,8) = STRING(TIDREGITAB.DATUM,"99/99/99").
            END.
            IF TIDREGITAB.TRAKTKOD NE " " AND TIDREGITAB.TRAKTAUTO = NO THEN DO:      
               FIND FIRST TRAKTATAB WHERE TRAKTATAB.TRAKTKOD = TIDREGITAB.TRAKTKOD
               USE-INDEX TRAKTKOD NO-LOCK NO-ERROR.          
               IF SUBSTRING(tidut.UT,59) NE " " THEN CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,9) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,25) = tidpers.EFTERNAMN    
               SUBSTRING(tidut.UT,51) = STRING(regvnr)
               SUBSTRING(tidut.UT,55) = TIDREGITAB.DAG  
               SUBSTRING(tidut.UT,59) = STRING(TRAKTATAB.VILART)
               SUBSTRING(tidut.UT,65) = "ST"
               SUBSTRING(tidut.UT,71) = STRING(TIDREGITAB.TRAKTANTAL)
               SUBSTRING(tidut.UT,77) = TRAKTATAB.FORKL.                      
               IF TIDREGITAB.TRAKTANTAL < 1 THEN 
               SUBSTRING(tidut.UT,71) = STRING(TIDREGITAB.TRAKTANTAL,"9.9").
               IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,50,8) = STRING(TIDREGITAB.DATUM,"99/99/99").
            END.         
         END.  
         GET NEXT tidq NO-LOCK.         
      END.                    
      CLOSE QUERY tidq.
   END.       
   FIND LAST tidut NO-LOCK NO-ERROR.

END PROCEDURE.

