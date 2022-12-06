/*TIDSTARTW.P*/
{REGVAR.I}
{TIDALLT.I}
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO. 
DEFINE VARIABLE helpslut AS DECIMAL NO-UNDO.
FIND FIRST tidallt WHERE 
tidallt.PERSONAL = pkod AND
tidallt.DATUM = regdatum AND tidallt.START LE regstart AND
tidallt.SLUT > regstart AND tidallt.TIDLOG = TRUE 
USE-INDEX PSTART NO-LOCK NO-ERROR.
IF NOT AVAILABLE tidallt THEN DO:
   FIND FIRST tidallt WHERE tidallt.RECTIDVIS = tidtabrec NO-LOCK NO-ERROR.
   IF AVAILABLE tidallt THEN DO:
      helpslut = tidallt.SLUT.
      FIND FIRST tidallt WHERE 
      tidallt.PERSONAL = pkod AND
      tidallt.DATUM = regdatum AND tidallt.SLUT > regstart AND
      tidallt.SLUT <= helpslut AND
      tidallt.TIDLOG = TRUE AND tidallt.RECTIDVIS NE tidtabrec
      USE-INDEX PSTART NO-LOCK NO-ERROR.
      IF AVAILABLE tidallt THEN DO:
         musz = TRUE.
         MESSAGE "Det finns redan en registrering med start "
                  tidallt.START " och slut " tidallt.SLUT " den " tidallt.DATUM "!"  
                  VIEW-AS ALERT-BOX.
         RETURN.
      END.
      ELSE DO:
         musz = FALSE.
         RETURN.
      END.
   END.
   ELSE DO:
      musz = FALSE.
      RETURN.
   END.
   
END.
ELSE DO:    
   IF vart = "AND" OR vart = "NYA"  THEN IF tidallt.RECTIDVIS = tidtabrec THEN RETURN.
   musz = TRUE.
   MESSAGE "Det finns redan en registrering med start "
            tidallt.START " och slut " tidallt.SLUT " den " tidallt.DATUM "!"  
            VIEW-AS ALERT-BOX.
   RETURN.
END.                
 
