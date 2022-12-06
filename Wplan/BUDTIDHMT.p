/*BUDTIDHMT.P*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}
{REGVAR.I}


{TIDUTTTNEW.I}
DEFINE INPUT PARAMETER kto AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER valomr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER franar AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.

DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO. 
DEFINE VARIABLE kontbenvar AS CHARACTER NO-UNDO.
FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = valomr USE-INDEX OMR
NO-LOCK NO-ERROR.
FIND FIRST KBENAMNING WHERE NO-LOCK NO-ERROR.
IF kto = "K1" THEN kontbenvar = CAPS(KBENAMNING.K1).
ELSE IF kto = "K2" THEN kontbenvar = CAPS(KBENAMNING.K2).
ELSE IF kto = "K3" THEN kontbenvar = CAPS(KBENAMNING.K3).
ELSE IF kto = "K4" THEN kontbenvar = CAPS(KBENAMNING.K4).
ELSE IF kto = "K4" THEN kontbenvar = CAPS(KBENAMNING.K4).

DO TRANSACTION:                     
   CREATE tidut.
   SUBSTRING(tidut.UT,1) = "BUDGET".        
   ASSIGN         
   SUBSTRING(tidut.UT,40) = STRING(TODAY)
   SUBSTRING(tidut.UT,50) = STRING(TIME,"HH:MM:SS").
   CREATE tidut.
   SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gomrk) + ":".         
   SUBSTRING(tidut.UT,8) = OMRADETAB.NAMN.
   CREATE tidut.
   SUBSTRING(tidut.UT,1) = "ÅRTAL :".         
   SUBSTRING(tidut.UT,8) = STRING(franar).        
   CREATE tidut.
   CREATE tidut.                 
   
   ASSIGN
   str = "=======.=========================.==========.==========".         
   SUBSTRING(tidut.UT,1) = str.                     
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,1) = kontbenvar
   SUBSTRING(tidut.UT,9) = "BENÄMNING" 
   SUBSTRING(tidut.UT,35) = "PENGAR"
   SUBSTRING(tidut.UT,46) = "TIMMAR".        
   CREATE tidut.                 
   SUBSTRING(tidut.UT,1) = str.               
   OPEN QUERY kq FOR EACH BUDGET WHERE BUDGET.OMRADE = valomr AND 
   BUDGET.KONTO = kto AND BUDGET.ARTAL = franar USE-INDEX BUD NO-LOCK.
   GET FIRST kq NO-LOCK.
   DO WHILE AVAILABLE(BUDGET):
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = BUDGET.KONTONR
      SUBSTRING(tidut.UT,9) = BUDGET.BENAMNING
      SUBSTRING(tidut.UT,35) = STRING(BUDGET.PENGAR,">>>>>>>9")
      SUBSTRING(tidut.UT,46) = STRING(BUDGET.TIMMAR,">>>>>>>9").
      CREATE tidut.
      GET NEXT kq NO-LOCK.
   END.
   CLOSE QUERY kq.  
END.
