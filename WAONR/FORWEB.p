/*FORWEB.P */
DEFINE INPUT PARAMETER wvc AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER versionok AS LOGICAL NO-UNDO.
DEFINE VARIABLE pwc AS CHARACTER NO-UNDO.
DEFINE VARIABLE vc AS CHARACTER NO-UNDO.
DEFINE VARIABLE wcorg AS CHARACTER NO-UNDO.
DEFINE VARIABLE wcspar1 AS INTEGER NO-UNDO.
DEFINE VARIABLE wcspar2 AS INTEGER NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
IF wvc BEGINS "10w" OR wvc BEGINS "09w" THEN DO:
   ASSIGN
   pwc = SUBSTRING(wvc,1,3)
   vc = SUBSTRING(wvc,4).
END.
ELSE DO:
   ASSIGN
   vc = wvc
   pwc = "".
END.
ASSIGN
wcspar1 = 1
wcspar2 = 2.
IF pwc = "10w" THEN DO:
   ASSIGN
   wcspar1 = 2
   wcspar2 = 1.
END.
versionok = TRUE.
{EXTRADATA.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
EMPTY TEMP-TABLE extradatatemp NO-ERROR.       
CREATE inextradatatemp.          
ASSIGN
inextradatatemp.PROGRAM = "GURUWCVER"                   
inextradatatemp.HUVUDCH = FORETAG.FORETAG
inextradatatemp.HUVUDINT = ?.                    
RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
FIND FIRST extradatatemp NO-LOCK NO-ERROR.
IF AVAILABLE extradatatemp THEN DO: 
   wcorg = extradatatemp.SOKCHAR[wcspar2].
   IF vc = extradatatemp.SOKCHAR[wcspar1] THEN versionok = FALSE.
   ELSE DO:
      IF INTEGER(SUBSTRING(vc,1,LENGTH(vc) - 1)) > INTEGER(SUBSTRING(extradatatemp.SOKCHAR[wcspar1],1,LENGTH(extradatatemp.SOKCHAR[wcspar1]) - 1)) THEN DO:
         RUN spara_UI.
      END.
      ELSE versionok = TRUE.
   END.      
END.  
ELSE DO:
   RUN spara_UI.   
END.
EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
EMPTY TEMP-TABLE extradatatemp NO-ERROR.       
IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.

PROCEDURE spara_UI :
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.       
   versionok = FALSE.
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "GURUWCVER"                   
   inextradatatemp.HUVUDCH = FORETAG.FORETAG
   inextradatatemp.HUVUDINT = ?
   inextradatatemp.SOKCHAR[wcspar1] = vc
   inextradatatemp.SOKCHAR[wcspar2] = wcorg.
   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp). 
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.       
END PROCEDURE.

