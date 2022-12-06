/*xdynload.p*/
{DYNTTDUMP.I}

DEFINE INPUT PARAMETER TABLE FOR tabnamn.      
DEFINE VARIABLE finns AS LOGICAL NO-UNDO.
DEFINE VARIABLE qh AS HANDLE NO-UNDO.
DEFINE VARIABLE bh AS HANDLE NO-UNDO.
DEFINE VARIABLE falth AS HANDLE NO-UNDO.
DEFINE VARIABLE falt2h AS HANDLE NO-UNDO.
DEFINE VARIABLE tabvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE kollh  AS HANDLE NO-UNDO.
DEFINE VARIABLE kollqh AS HANDLE NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
DEFINE VARIABLE uservar AS CHARACTER NO-UNDO.
uservar = USERID.
IF uservar NE {setuser.I} THEN DO:
   SETUSERID({setuser.I},{setpwd.I}).
END.
{AMERICANEUROPEAN.I}
SESSION:NUMERIC-FORMAT = "AMERICAN".
FIND FIRST tabnamn WHERE NO-LOCK NO-ERROR.
IF AVAILABLE tabnamn THEN finns = TRUE.
tabvar = "_File".
kommando = "FOR EACH " + tabvar + " WHERE " + tabvar + "._FROZEN = FALSE NO-LOCK".
CREATE BUFFER bh FOR TABLE tabvar.
CREATE QUERY qh.
qh:SET-BUFFERS(bh).
qh:QUERY-PREPARE(kommando).   
qh:QUERY-OPEN().
qh:GET-FIRST(NO-LOCK).
DO WHILE bh:AVAILABLE:
   falth = bh:BUFFER-FIELD("_FILE-NAME").
   falt2h = bh:BUFFER-FIELD("_Dump-name").
    IF finns = TRUE THEN DO:
      FOR EACH tabnamn WHERE tabnamn.NAMN = falth:BUFFER-VALUE:
         tabnamn.DUMPNAMN = SESSION:TEMP-DIR + falt2h:BUFFER-VALUE + ".XML".
      END.
      
   END.
   ELSE DO:   
      CREATE tabnamn.
      ASSIGN
      tabnamn.DUMPNAMN = SESSION:TEMP-DIR + falt2h:BUFFER-VALUE + ".XML".
      tabnamn.NAMN = falth:BUFFER-VALUE.
   END.   
   qh:GET-NEXT(NO-LOCK).        
END.

  

FOR EACH tabnamn WHERE NO-LOCK:
   CREATE BUFFER orgbufh FOR TABLE tabnamn.NAMN.
   CREATE TEMP-TABLE ttcopyh. 
   ttcopyh:CREATE-LIKE(orgbufh).
   CREATE TEMP-TABLE ttcopyh. 
   ttcopyh:CREATE-LIKE(orgbufh).
   ttcopyh:TEMP-TABLE-PREPARE("ttkopia"). 
   ttbuffcopyh = ttcopyh:DEFAULT-BUFFER-HANDLE.
   
   ASSIGN
   cFile = tabnamn.DUMPNAMN
   cSourceType             = "FILE"
   cReadMode               = "MERGE"
   cSchemaLocation         = ?
   lOverrideDefaultMapping = FALSE.
   RUN tabkoll_UI.
   ttcopyh:READ-XML (cSourceType, cFile, cReadMode,cSchemaLocation, lOverrideDefaultMapping).
  
   RUN inxmltab_UI.
   
   
END.
      
PROCEDURE inxmltab_UI:
   DEFINE VARIABLE komcop AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ttgh AS HANDLE NO-UNDO.
   komcop = "FOR EACH " + ttbuffcopyh:TABLE +  " NO-LOCK.".
   CREATE QUERY ttgh.
   ttgh:SET-BUFFERS(ttbuffcopyh).
   ttgh:QUERY-PREPARE(komcop).
   ttgh:QUERY-OPEN().
   ttgh:GET-FIRST(NO-LOCK).
   DO WHILE ttgh:QUERY-OFF-END = FALSE:
      DO TRANSACTION: 
         orgbufh:BUFFER-CREATE().
         orgbufh:BUFFER-COPY(ttbuffcopyh).
      END.   
      ttgh:GET-NEXT(NO-LOCK).
   END.
   ttgh:QUERY-CLOSE.
   DELETE OBJECT ttgh NO-ERROR.
END PROCEDURE.



{EUROPEANAMERICAN.I}
