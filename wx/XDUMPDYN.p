/*XDUMPDYN.P*/
/*kalkyl som dumpas med RECKALKYL och m�ste l�sas in via xkalkin.p*/
/*tidregitab tidfel l�ses ut utan rectidvis anv�nds rectidvis m�ste detta l�sas
*/
DEFINE TEMP-TABLE tabnamn NO-UNDO
   FIELD NAMN AS CHARACTER
   FIELD DUMPNAMN AS CHARACTER
   INDEX NAMN IS PRIMARY NAMN.
DEFINE TEMP-TABLE k2 NO-UNDO
      FIELD OMRADE LIKE KALKYL.OMRADE
      FIELD RECKALKYL AS INTEGER 
      FIELD BEFATTNING LIKE KALKYL.BEFATTNING 
      FIELD PRIS LIKE KALKYL.PRIS 
      FIELD TIMMAR LIKE KALKYL.TIMMAR 
      FIELD OT50 LIKE KALKYL.OT50 
      FIELD OT75 LIKE KALKYL.OT75 
      FIELD OT100 LIKE KALKYL.OT100 
      FIELD PERSMASK LIKE KALKYL.PERSMASK 
      FIELD TYP LIKE KALKYL.TYP
      FIELD RADNR LIKE KALKYL.RADNR. 
   
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
IF uservar NE CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN DO:
   SETUSERID(CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79),"KAGGEN").
END.
/*SESSION:NUMERIC-FORMAT = "European".*/
SESSION:NUMERIC-FORMAT = "AMERICAN".
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
   CREATE tabnamn.
   ASSIGN
   tabnamn.DUMPNAMN = SESSION:TEMP-DIR + falt2h:BUFFER-VALUE + ".d".
   tabnamn.NAMN = falth:BUFFER-VALUE.
   qh:GET-NEXT(NO-LOCK).        
END.
FOR EACH tabnamn:
   RUN uttab_UI (INPUT tabnamn.NAMN).
END.  


PROCEDURE uttab_UI:
   DEFINE INPUT PARAMETER ip_cTable AS CHARACTER NO-UNDO.
   
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
   hQuery:QUERY-PREPARE('FOR EACH ':u + ip_cTable + ' NO-LOCK':u).
   hQuery:QUERY-OPEN().
   hQuery:GET-FIRST().
   ASSIGN
   iNumber    = hBuffer:NUM-FIELDS
   cTimeStamp = STRING(YEAR( TODAY),"9999":u) + "/":u
              + STRING(MONTH(TODAY),"99":u  ) + "/":u
              + STRING(DAY(  TODAY),"99":u  ) + "-":u
              + STRING(TIME,"HH:MM:SS":u).
   /*skapar fil*/
   OUTPUT TO VALUE(tabnamn.DUMPNAMN) NO-ECHO NO-MAP.
   EXPORT ?.
   iBack = SEEK(OUTPUT) - 1.
   /*TILL F�RSTA POS*/
   SEEK OUTPUT TO 0.
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
         IF lFirst = TRUE THEN lFirst = FALSE.
         ELSE DO:
            /*TAR BORT CR*/
            SEEK OUTPUT TO SEEK(OUTPUT) - iBack.
            PUT CONTROL ' ':u.
         END.
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

   PUT UNFORMATTED ".":u SKIP.
   iTrailer = SEEK(OUTPUT).
   PUT UNFORMATTED
       "PSC":u SKIP
       "filename=":u hBuffer:TABLE SKIP
       "records=":u  STRING(nRecords,"9999999999999":u) SKIP
       "ldbname=":u  hBuffer:DBNAME SKIP
       "timestamp=":u cTimeStamp SKIP
       "numformat=":u ASC(SESSION:NUMERIC-SEPARATOR) ",":u ASC(SESSION:NUMERIC-DECIMAL-POINT) SKIP
       "dateformat=":u SESSION:DATE-FORMAT "-":u SESSION:YEAR-OFFSET SKIP
       "map=NO-MAP":u SKIP
       "cpstream=":u SESSION:CPSTREAM SKIP
       ".":u SKIP
       STRING(iTrailer,"9999999999":u) SKIP.

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