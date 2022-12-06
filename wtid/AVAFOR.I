/*AVAFOR.I*/
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
{EXTRADATA.I}

PROCEDURE havafor:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER avarfor AS INTEGER NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "AVAFOR"                   
   inextradatatemp.HUVUDCH = pkod.
   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN
      avarfor = extradatatemp.SOKINT[1].         
   END.   
   ELSE avarfor = 0.         
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.
