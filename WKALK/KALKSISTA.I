/*KALKSISTA.I*/
PROCEDURE kalksista_UI :  
   DEFINE INPUT-OUTPUT PARAMETER tempvar AS INTEGER NO-UNDO.
   DEFINE BUFFER FASTKALKSISTA FOR FASTSPEC.
   DEFINE BUFFER KALKKALKSISTA FOR KALKSPEC.
   DEFINE VARIABLE felkoll AS LOGICAL NO-UNDO.
   IF tempvar < OMRADETAB.KALKYLINT1 THEN tempvar = OMRADETAB.KALKYLINT1.
   REPEAT:
      FIND FIRST FASTKALKSISTA WHERE FASTKALKSISTA.KALKNR = tempvar NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FASTKALKSISTA THEN DO:
         FIND FIRST KALKKALKSISTA WHERE KALKKALKSISTA.KALKNR = tempvar NO-LOCK NO-ERROR.
         IF NOT AVAILABLE KALKKALKSISTA THEN LEAVE.
      END.
      tempvar = tempvar + 1.
      IF tempvar > OMRADETAB.KALKYLINT2 THEN DO:
         felkoll = TRUE.
         LEAVE.
      END.    
   END.              
   DO TRANSACTION:
      FIND CURRENT OMRADETAB EXCLUSIVE-LOCK.
      OMRADETAB.KALKYLSIST = tempvar.
      IF felkoll = TRUE THEN tempvar = ?.  
   END.   
END PROCEDURE.
