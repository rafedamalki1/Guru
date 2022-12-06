/*  BORTLOGG.P
   Parameters: 
   DEFINE VARIABLE logprogh AS HANDLE NO-UNDO.
   RUN loggskap_UI IN logprogh (INPUT 1,INPUT SUBSTRING(FILE-INFO:FILE-NAME,(R-INDEX(FILE-INFO:FILE-NAME,"\") + 1)), INPUT "AONR", INPUT aonrvar + " " + STRING(delnrvar)).
   IF VALID-HANDLE(logprogh) THEN DELETE PROCEDURE logprogh NO-ERROR.       
*/

{EXTRADATA.I}
{GLOBVAR2DEL1.I}
DEFINE VARIABLE vc AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcnr AS CHARACTER NO-UNDO.
DEFINE VARIABLE vc2 AS CHARACTER NO-UNDO.

PROCEDURE loggskap_UI :
   DEFINE INPUT PARAMETER loglevel AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER pronamn AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER typ AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varnr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE outanvanv AS CHARACTER NO-UNDO.
   DEFINE VARIABLE outdatornamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE datornamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE pcanv AS CHARACTER NO-UNDO.
   DEFINE VARIABLE loggaok AS LOGICAL NO-UNDO.
   DEFINE VARIABLE loggapp AS HANDLE NO-UNDO.
   DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
   DEFINE VARIABLE Urlsite AS CHARACTER NO-UNDO.
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.   
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      /* kolla loggning ja/nej*/
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "LOGNIV"                   
   inextradatatemp.HUVUDCH =  ?              
   inextradatatemp.HUVUDINT =  loglevel.   
   RUN finnsextra_UI IN edataapph (INPUT TABLE inextradatatemp,OUTPUT loggaok).        
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.   
   IF loggaok = FALSE THEN DO:
      IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph NO-ERROR.      
      edataapph = ?. 
      RETURN.
   END.
   RUN INLOAPI.P (OUTPUT outanvanv, OUTPUT outdatornamn).

   datornamn = TRIM(outdatornamn) + CHR(10).
   pcanv = TRIM(outanvanv) + CHR(10).

   
         /*skapa log om loglevel = 1 */
   IF loglevel = 1 THEN DO:      
      
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
      CREATE inextradatatemp.
      ASSIGN
      inextradatatemp.PROGRAM = "LOGPOST"
      inextradatatemp.HUVUDINT =  loglevel
      inextradatatemp.SOKCHAR[1] = pronamn
      inextradatatemp.SOKCHAR[2] = datornamn
      inextradatatemp.SOKCHAR[3] = Guru.Konstanter:globanv
      inextradatatemp.SOKCHAR[4] = typ
      inextradatatemp.SOKCHAR[5] = varnr
      inextradatatemp.SOKDAT[1] = TODAY
      inextradatatemp.SOKINT[1] = TIME.
      RUN sparaextra_UI IN edataapph (INPUT TABLE inextradatatemp).
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   END.

   IF loglevel = 2 THEN DO:
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
         /*
         RUN WEBVERSION.P (INPUT 1,INPUT-OUTPUT vc,INPUT-OUTPUT vcnr,OUTPUT vc2,OUTPUT Urlsite).
         */
         RUN WEBVERSION.P (INPUT 1,INPUT-OUTPUT vc,INPUT-OUTPUT vcnr,OUTPUT vc2).
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
         CREATE inextradatatemp.
         ASSIGN
         inextradatatemp.PROGRAM = "logpost"
         inextradatatemp.HUVUDINT =  loglevel   
         inextradatatemp.SOKCHAR[1] = pronamn
         inextradatatemp.SOKCHAR[2] = datornamn
         inextradatatemp.SOKCHAR[3] = Guru.Konstanter:globanv
         inextradatatemp.SOKCHAR[4] = typ
         inextradatatemp.SOKCHAR[5] = vc
         inextradatatemp.SOKDAT[1] = TODAY
         inextradatatemp.SOKINT[1] = TIME. 
         RUN sparaextra_UI IN edataapph (INPUT TABLE inextradatatemp).
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
      END.
   END.
   ELSE IF loglevel = 3 THEN DO: /* import från HD */
   
   END.

   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph NO-ERROR.      
   edataapph = ?. 
END PROCEDURE.




PROCEDURE loggvis_UI :
END PROCEDURE.
