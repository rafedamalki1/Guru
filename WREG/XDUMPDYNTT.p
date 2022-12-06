/*XDUMPDYNTT.P*/
/*kalkyl som dumpas med RECKALKYL och måste läsas in via xkalkin.p*/
/*tidregitab tidfel läses ut utan rectidvis används rectidvis måste detta lösas
*/

{DYNTTDUMP.I}
   
DEFINE INPUT PARAMETER TABLE FOR tabnamn.
   
DEFINE VARIABLE qh AS HANDLE NO-UNDO.
DEFINE VARIABLE bh AS HANDLE NO-UNDO.
DEFINE VARIABLE falth AS HANDLE NO-UNDO.
DEFINE VARIABLE falt2h AS HANDLE NO-UNDO.
DEFINE VARIABLE tabvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE kollh  AS HANDLE NO-UNDO.
DEFINE VARIABLE kollqh AS HANDLE NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
DEFINE VARIABLE uservar AS CHARACTER NO-UNDO.
DEFINE VARIABLE finns AS LOGICAL NO-UNDO.


uservar = USERID.
IF uservar NE {setuser.I} THEN DO:
   SETUSERID({setuser.I},{setpwd.I}).
END.
/*SESSION:NUMERIC-FORMAT = "European".*/
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
         tabnamn.DUMPNAMN = SESSION:TEMP-DIR + falt2h:BUFFER-VALUE + ".xml".
      END.
      
   END.
   ELSE DO:   
      CREATE tabnamn.
      ASSIGN
      tabnamn.DUMPNAMN = SESSION:TEMP-DIR + falt2h:BUFFER-VALUE + ".xml".
      tabnamn.NAMN = falth:BUFFER-VALUE.
   END.   
   qh:GET-NEXT(NO-LOCK).        
END.

IF finns = TRUE THEN DO:
   FOR EACH tabnamn WHERE tabnamn.WHEREFRASE = "" NO-LOCK:
      CREATE BUFFER orgbufh FOR TABLE tabnamn.NAMN.
      CREATE TEMP-TABLE ttcopyh. 
      ttcopyh:CREATE-LIKE(orgbufh).
      CREATE TEMP-TABLE ttcopyh. 
      ttcopyh:CREATE-LIKE(orgbufh).
      ttcopyh:TEMP-TABLE-PREPARE("ttkopia"). 
      ttbuffcopyh = ttcopyh:DEFAULT-BUFFER-HANDLE.
      RUN utxmltab_UI. 
      RUN tabkoll_UI. 
      ttcopyh:WRITE-XML("FILE", tabnamn.DUMPNAMN).
      ttcopyh:EMPTY-TEMP-TABLE() NO-ERROR.
      DELETE tabnamn.
   END.
   FIND FIRST tabnamn WHERE tabnamn.WHEREFRASE NE "" AND tabnamn.UTDUMP = FALSE NO-LOCK NO-ERROR.
   REPEAT: 
      
      IF NOT AVAILABLE tabnamn THEN LEAVE.
      dname = tabnamn.DUMPNAMN.
      tname = tabnamn.NAMN.
      CREATE BUFFER orgbufh FOR TABLE tabnamn.NAMN.
      CREATE TEMP-TABLE ttcopyh. 
      ttcopyh:CREATE-LIKE(orgbufh).
      CREATE TEMP-TABLE ttcopyh. 
      ttcopyh:CREATE-LIKE(orgbufh).
      ttcopyh:TEMP-TABLE-PREPARE("ttkopia"). 
      ttbuffcopyh = ttcopyh:DEFAULT-BUFFER-HANDLE. 
      FOR EACH tabnamn WHERE tabnamn.NAMN = tname NO-LOCK:
         RUN utxmltab_UI. 
         tabnamn.UTDUMP = TRUE.        
      END.
      RUN tabkoll_UI. 
      ttcopyh:WRITE-XML("FILE", dname).
      ttcopyh:EMPTY-TEMP-TABLE() NO-ERROR.
      
      FIND NEXT tabnamn WHERE tabnamn.WHEREFRASE NE "" AND tabnamn.UTDUMP = FALSE NO-LOCK NO-ERROR.
   END.
   
END.
ELSE DO:
   FOR EACH tabnamn WHERE NO-LOCK:
      CREATE BUFFER orgbufh FOR TABLE tabnamn.NAMN.
      CREATE TEMP-TABLE ttcopyh. 
      ttcopyh:CREATE-LIKE(orgbufh).
      CREATE TEMP-TABLE ttcopyh. 
      ttcopyh:CREATE-LIKE(orgbufh).
      ttcopyh:TEMP-TABLE-PREPARE("ttkopia"). 
      ttbuffcopyh = ttcopyh:DEFAULT-BUFFER-HANDLE.
      RUN utxmltab_UI.
      RUN tabkoll_UI. 
      ttcopyh:WRITE-XML("FILE", tabnamn.DUMPNAMN).
      ttcopyh:EMPTY-TEMP-TABLE() NO-ERROR.
   END.
END.      

PROCEDURE utxmltab_UI:
   DEFINE VARIABLE komcop AS CHARACTER NO-UNDO.
   DEFINE VARIABLE orgqh AS HANDLE NO-UNDO.
   komcop = "FOR EACH " + orgbufh:TABLE + " " + tabnamn.WHEREFRASE + " NO-LOCK.".
   CREATE QUERY orgqh.
   orgqh:SET-BUFFERS(orgbufh).
   orgqh:QUERY-PREPARE(komcop).
   orgqh:QUERY-OPEN().
   orgqh:GET-FIRST(NO-LOCK).
   DO WHILE orgqh:QUERY-OFF-END = FALSE:
      ttbuffcopyh:BUFFER-CREATE().
      ttbuffcopyh:BUFFER-COPY(orgbufh).
      orgqh:GET-NEXT(NO-LOCK).
   END.
   orgqh:QUERY-CLOSE.
   DELETE OBJECT orgqh NO-ERROR.
END PROCEDURE.


/*
FIND FIRST tabnamn  WHERE NO-LOCK NO-ERROR.
   CREATE BUFFER orgbufh FOR TABLE tabnamn.NAMN.
   CREATE TEMP-TABLE ttcopyh. 
   ttcopyh:CREATE-LIKE(orgbufh).
   ttcopyh:TEMP-TABLE-PREPARE("ttkopia"). 
   ttbuffcopyh = ttcopyh:DEFAULT-BUFFER-HANDLE.
   FOR EACH tabnamn WHERE NO-LOCK:
      RUN utxmltab_UI.
   END.
   FIND FIRST tabnamn  WHERE NO-LOCK NO-ERROR.
   
FOR EACH tabnamn:
   RUN uttab_UI (INPUT tabnamn.NAMN, INPUT " " + tabnamn.WHEREFRASE).
END.  


PROCEDURE uttab_UI:
   DEFINE INPUT PARAMETER ip_cTable AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER ip_where AS CHARACTER NO-UNDO.
   DEFINE VARIABLE hQuery  AS HANDLE NO-UNDO.
   DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
   DEFINE VARIABLE hField  AS HANDLE NO-UNDO.

   DEFINE VARIABLE lFirst   AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE iField   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE iNumber  AS INTEGER   NO-UNDO.
   DEFINE VARIABLE iExtent  AS INTEGER   NO-UNDO.
   DEFINE VARIABLE iBack    AS INTEGER   NO-UNDO.
   DEFINE VARIABLE iTrailer AS INTEGER   NO-UNDO.
   DEFINE VARIABLE cTimeStamp AS CHARACTER NO-UNDO.
   DEFINE VARIABLE nRecords   AS DECIMAL DECIMALS 0 INITIAL 0 NO-UNDO.

   CREATE QUERY hQuery.
   CREATE BUFFER hBuffer FOR TABLE ip_cTable.

/*    hBuffer:DISABLE-DUMP-TRIGGERS(). /*optional*/ */
   hQuery:SET-BUFFERS(hBuffer).
   hQuery:QUERY-PREPARE('FOR EACH ':u + ip_cTable + ip_where + ' NO-LOCK':u).
   hQuery:QUERY-OPEN().
   hQuery:GET-FIRST().
   ASSIGN
   iNumber    = hBuffer:NUM-FIELDS.
   
   /*skapar fil*/
   
   OUTPUT TO VALUE(tabnamn.DUMPNAMN) APPEND NO-ECHO NO-MAP .
  /* EXPORT ?.*/
   iBack = SEEK(OUTPUT) - 1.
   /*TILL FÖRSTA POS*/
   /*SEEK OUTPUT TO 0.*/
   DO WHILE hBuffer:AVAILABLE:
      ASSIGN
      nRecords = nRecords + 1.
      lFirst = TRUE.
      DO iField = 1 TO iNumber:
         hField = hBuffer:BUFFER-FIELD(iField).
         IF hField:DATA-TYPE = "recid":u THEN DO:
            IF tabnamn.NAMN = "KALKYL" THEN.
            ELSE NEXT.
         END.
        /*
         IF lFirst = TRUE THEN lFirst = FALSE.
         ELSE DO:
            /*TAR BORT CR*/
            SEEK OUTPUT TO SEEK(OUTPUT) - iBack.
            PUT CONTROL ' ':u.
         END.
         */
         IF hField:EXTENT > 1 THEN DO iExtent = 1 TO hField:EXTENT:
            IF iExtent > 1 THEN DO:
               /*TAR BORT CR*/
               SEEK OUTPUT TO SEEK(OUTPUT) - iBack.
               PUT CONTROL ' ':u.
            END.
            EXPORT hField:BUFFER-VALUE(iExtent).
         END.
         ELSE EXPORT hField:BUFFER-VALUE.
      END.
      hQuery:GET-NEXT().
   END.

   OUTPUT CLOSE.
   hQuery:QUERY-CLOSE().
   DELETE OBJECT hQuery.
   DELETE OBJECT hBuffer.
END PROCEDURE.


PROCEDURE kalkin_UI :
   FIND FIRST tabnamn WHERE tabnamn.NAMN = "KALKYL" NO-LOCK NO-ERROR.
   INPUT FROM VALUE(tabnamn.DUMPNAMN) NO-ECHO.
   REPEAT:
      CREATE k2.
      ASSIGN.
      IMPORT k2 NO-ERROR.
   END.
   INPUT CLOSE.
   FOR EACH k2:
      CREATE KALKYL.
      BUFFER-COPY k2 EXCEPT RECKALKYL TO KALKYL . 
      KALKYL.RECKALKYL = k2.RECKALKYL.
   END.
END PROCEDURE.
*/