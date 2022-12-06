DEFINE VARIABLE ttbuffcopyh AS HANDLE NO-UNDO.
DEFINE VARIABLE tthandle AS HANDLE NO-UNDO.
DEFINE VARIABLE ttbuffh AS HANDLE NO-UNDO.
DEFINE TEMP-TABLE valberedningtemp NO-UNDO
   FIELD ao AS CHARACTER.
CREATE valberedningtemp.
valberedningtemp.ao = "1".
tthandle = TEMP-TABLE valberedningtemp:HANDLE.
ttbuffh = tthandle:DEFAULT-BUFFER-HANDLE.
RUN ttcopy_UI (INPUT TABLE-HANDLE tthandle).

PROCEDURE ttcopy_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER TABLE-HANDLE tthandle.
   DEFINE VARIABLE ttcopyh AS HANDLE NO-UNDO.
   DEFINE VARIABLE komcop AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ttqh AS HANDLE NO-UNDO.
   DEFINE VARIABLE ttbuffh AS HANDLE NO-UNDO.

   ttbuffh = tthandle:DEFAULT-BUFFER-HANDLE.  
   CREATE TEMP-TABLE ttcopyh. 
   ttcopyh:CREATE-LIKE(ttbuffh:NAME).
   /*
   CREATE TEMP-TABLE ttcopyh. 
   ttcopyh:ADD-FIELDS-FROM(tthandle:NAME).                                
   */
   ttcopyh:TEMP-TABLE-PREPARE("ttkopia"). 
   ttbuffcopyh = ttcopyh:DEFAULT-BUFFER-HANDLE.
   komcop = "FOR EACH " + ttbuffh:TABLE + " NO-LOCK.".
   CREATE QUERY ttqh.
   ttqh:SET-BUFFERS(ttbuffh).
   ttqh:QUERY-PREPARE(komcop).
   ttqh:QUERY-OPEN().
   ttqh:GET-FIRST(NO-LOCK).
   DO WHILE ttqh:QUERY-OFF-END = FALSE:
      ttbuffcopyh:BUFFER-CREATE().
      ttbuffcopyh:BUFFER-COPY(ttbuffh).
      ttqh:GET-NEXT(NO-LOCK).
   END.
   ttqh:QUERY-CLOSE.
   DELETE OBJECT ttqh NO-ERROR.     
END PROCEDURE.

