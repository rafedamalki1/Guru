/*RESKOLLA.P*/
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}

DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER enfle AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ganv AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER rstart AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER rslut AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.   
DEFINE VARIABLE regdatum AS DATE NO-UNDO.


IF vadgora = 1 THEN DO:
   ASSIGN
   regdatum = bdatum.
   REPEAT:
      IF regdatum > avdatum THEN LEAVE.       
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = pkod AND
      TIDREGITAB.DATUM = regdatum AND
      TIDREGITAB.ENFLERDAGS NE "" USE-INDEX PSTART NO-LOCK NO-ERROR.
      IF NOT AVAILABLE TIDREGITAB THEN DO:
         regdatum = regdatum + 1.
      END.
      ELSE DO:              
         musz = FALSE.
         FIND LAST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = pkod AND
         TIDREGITAB.DATUM = regdatum  AND
         TIDREGITAB.ENFLERDAGS NE " "  USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:
            IF regdatum = bdatum AND TIDREGITAB.ENFLERDAGS = enfle THEN DO:               
               CREATE felmeddtemp.
               felmeddtemp.FELMEDD = "Det finns redan en resa upplagd under denna tidsperiod ." + STRING(regdatum).                             
               LEAVE.           
            END.     
            IF regdatum = avdatum AND TIDREGITAB.ENFLERDAGS = enfle THEN DO:
               CREATE felmeddtemp.
               felmeddtemp.FELMEDD = "Det finns redan en resa upplagd under denna tidsperiod ." + STRING(regdatum).               
               LEAVE.
            END.
            IF regdatum > bdatum THEN DO:
               CREATE felmeddtemp.
               felmeddtemp.FELMEDD = "Det finns redan en resa upplagd under denna tidsperiod ." + STRING(regdatum).               
               LEAVE.
            END.         
         END.
         regdatum = regdatum + 1.
      END.   
   END. 
END.
IF vadgora = 2 THEN DO:
   FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = pkod AND
   TIDREGITAB.DATUM >= bdatum AND TIDREGITAB.DATUM <= avdatum AND
   TIDREGITAB.ENFLERDAGS = enfle AND TIDREGITAB.GODKAND = '' 
   USE-INDEX PSTART NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE TIDREGITAB THEN DO:
      CREATE felmeddtemp.
      felmeddtemp.FELMEDD = "Det finns ingen " + enfle + " -resa att ändra på.".
      RETURN.
   END.
   regdatum = bdatum. 
   REPEAT:      
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = pkod AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.ENFLERDAGS = enfle AND 
      TIDREGITAB.GODKAND = '' USE-INDEX PSTART NO-LOCK NO-ERROR.    
      IF NOT AVAILABLE TIDREGITAB THEN DO:
         CREATE felmeddtemp.
         felmeddtemp.FELMEDD = "Det finns ingen " + enfle + " -resa att ändra på den " + STRING(regdatum).
         LEAVE.         
      END.      
      regdatum = regdatum + 1.
      IF regdatum > avdatum THEN LEAVE.
   END. 
END.
IF vadgora = 3 THEN DO:
   OPEN QUERY tidq 
   FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = pkod AND
   TIDREGITAB.DATUM >= bdatum AND TIDREGITAB.DATUM <= avdatum AND
   TIDREGITAB.ENFLERDAGS = enfle AND TIDREGITAB.GODKAND = '' USE-INDEX PSTART NO-LOCK.
   GET FIRST tidq NO-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB):
      DO TRANSACTION:
         GET CURRENT tidq EXCLUSIVE-LOCK.
         DELETE TIDREGITAB.
      END.
      GET NEXT tidq NO-LOCK.
   END.
END.
