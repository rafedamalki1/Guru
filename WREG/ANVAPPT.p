/*ANVAPPT.P*/
DEFINE INPUT PARAMETER globanv AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER sidH AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER sidB AS INTEGER NO-UNDO.

IF globanv NE "" THEN DO TRANSACTION:
   FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = globanv 
   EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE ANVANDARE THEN DO:  
      ASSIGN 
      ANVANDARE.SIDL = sidB
      ANVANDARE.SIDS = sidH.          
   END.
END.
