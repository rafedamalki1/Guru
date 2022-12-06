{LISTMTRL.I}      

PROCEDURE sklist_UI:
DEFINE INPUT PARAMETER anr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER oom AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR list_mtrl. 
   
   
   FOR EACH BERMTRL WHERE BERMTRL.AONR = "486" AND BERMTRL.omrade = "0910" NO-LOCK:
      CREATE list_mtrl.
      BUFFER-COPY BERMTRL TO list_mtrl.
      FIND FIRST mtrl WHERE mtrl.enr = BERMTRL.enr AND mtrl.levkod = BERMTRL.levkod  AND mtrl.kalknr = 0 NO-LOCK NO-ERROR.
      IF AVAILABLE mtrl THEN DO:
         IF mtrl.kund = TRUE THEN DO:
            /*dagtemp.DAGCMB:PRIVATE-DATA = dagtemp.DAGCMB:SCREEN-VALUE.*/
            ASSIGN             
            list_mtrl.ENR:PRIVATE-DATA  = "netto" . 
         END.
      END.
   END.
END.
