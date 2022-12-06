/*DYNPOSTFINNS.I*/
DEFINE VARIABLE dynorginaltab AS HANDLE NO-UNDO.
DEFINE VARIABLE orgorgqh AS HANDLE      NO-UNDO.
DEFINE VAR DynWp AS CHARACTER NO-UNDO.
DynWp = "DynTableU" + STRING(TIME).
CREATE WIDGET-POOL STRING(DynWp) NO-ERROR.
PROCEDURE finnspostdyn_UI.
   DEFINE INPUT  PARAMETER orginaltab AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER wherevar AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER finnspostok AS LOGICAL     NO-UNDO.
   
  
   finnspostok = FALSE.
   DEFINE VARIABLE kommandosortquery AS CHARACTER   NO-UNDO.
   
   wherevar = "WHERE " + wherevar. 
   CREATE BUFFER dynorginaltab FOR TABLE orginaltab IN WIDGET-POOL STRING(DynWp).
   kommandosortquery = "FOR EACH " + dynorginaltab:TABLE + " " + wherevar + " NO-LOCK.".
   
   CREATE QUERY orgorgqh IN WIDGET-POOL STRING(DynWp).
   orgorgqh:SET-BUFFERS(dynorginaltab).
   orgorgqh:QUERY-PREPARE(kommandosortquery).
   orgorgqh:QUERY-OPEN().
   orgorgqh:GET-FIRST(NO-LOCK).
   IF orgorgqh:QUERY-OFF-END THEN finnspostok = FALSE.
   ELSE finnspostok = TRUE.
   
  
END PROCEDURE.
PROCEDURE faltvarde_UI:
   DEFINE INPUT  PARAMETER faltvar AS CHARACTER   NO-UNDO.   
   DEFINE OUTPUT PARAMETER faltvarde AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE dbfalt AS HANDLE NO-UNDO.
   IF dynorginaltab = ? THEN RETURN.
   dbfalt = dynorginaltab:BUFFER-FIELD(faltvar) NO-ERROR. 
   faltvarde = dbfalt:BUFFER-VALUE. 
   
END PROCEDURE.
PROCEDURE queryclose_UI:
    
   orgorgqh:QUERY-CLOSE().
END PROCEDURE.
PROCEDURE DYNPOSTavs_UI :
   DELETE WIDGET-POOL STRING(DynWp) NO-ERROR. 
END PROCEDURE.