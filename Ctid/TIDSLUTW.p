/*TIDSLUTW.P*/
{REGVAR.I}
{TIDALLT.I}
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO. 
IF musz = FALSE THEN DO:                /*EJ DYGNSBRYT*/
   musz = FALSE.
   FIND FIRST tidallt WHERE 
   tidallt.PERSONAL = pkod AND
   tidallt.DATUM = regdatum AND tidallt.START < regslut AND
   tidallt.SLUT >= regslut AND tidallt.TIDLOG = TRUE 
   USE-INDEX PSTART NO-LOCK NO-ERROR.   
   IF NOT AVAILABLE tidallt THEN DO: 
      FIND FIRST tidallt WHERE 
      tidallt.PERSONAL = pkod AND
      tidallt.DATUM = regdatum AND tidallt.START > regstart AND
      tidallt.SLUT < regslut AND tidallt.TIDLOG = TRUE AND
      tidallt.RECTIDVIS NE tidtabrec
      USE-INDEX PSTART NO-LOCK NO-ERROR.        
      IF NOT AVAILABLE tidallt THEN DO:    
         musz = FALSE.
         RETURN.   
      END.  
      ELSE DO:
         RUN dubbel_UI.
         RETURN.          
      END.
   END.  
   ELSE DO:
      RUN dubbel_UI.
      RETURN. 
   END.   
END.   
ELSE DO:
   musz = FALSE.
   FIND FIRST tidallt WHERE 
   tidallt.PERSONAL = pkod AND
   tidallt.DATUM = regdatum AND tidallt.START GE regstart AND
   tidallt.SLUT LE 24.00 AND tidallt.TIDLOG = TRUE 
   USE-INDEX PSTART NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tidallt THEN DO:
      musz = FALSE.   
      FIND FIRST tidallt WHERE 
      tidallt.PERSONAL = pkod AND
      tidallt.DATUM = (regdatum + 1) AND tidallt.START GE 00.00 AND
      tidallt.SLUT LE regslut AND tidallt.TIDLOG = TRUE 
      USE-INDEX PSTART NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidallt THEN DO:
         musz = FALSE.      
         RETURN.
      END.
      ELSE DO:
         RUN dubbel_UI.
         RETURN. 
      END.    
   END.  
   ELSE DO:
      RUN dubbel_UI.  
      RETURN.           
   END.     
END.                
PROCEDURE dubbel_UI. 
   IF vart = "AND" OR vart = "NYA" THEN IF tidallt.RECTIDVIS = tidtabrec THEN RETURN.
   musz = TRUE.
   MESSAGE "Det finns redan en registrering med start "
            tidallt.START " och slut"    
            tidallt.SLUT " den " tidallt.DATUM "!"  
            VIEW-AS ALERT-BOX.                  
END PROCEDURE. 
